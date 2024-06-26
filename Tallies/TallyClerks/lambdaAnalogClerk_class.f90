module lambdaAnalogClerk_class

  use numPrecision
  use tallyCodes
  use dictionary_class,      only : dictionary
  use genericProcedures,     only : fatalError
  use particle_class,        only : particle, particleState, P_PRECURSOR
  use particleDungeon_class, only : particleDungeon
  use outputFile_class,      only : outputFile

  use scoreMemory_class,     only : scoreMemory
  use tallyClerk_inter,      only : tallyClerk, kill_super => kill

  ! Tally Maps
  use tallyMap_inter,        only : tallyMap
  use tallyMapFactory_func,  only : new_tallyMap

  ! Nuclear Data Interfaces
  use nuclearDatabase_inter, only : nuclearDatabase

  implicit none
  private

  !! Locations of diffrent bins wrt memory address of the clerk
  integer(shortInt), parameter :: MEM_SIZE = 2
  integer(longInt), parameter  :: LAMBDA = 0, &  ! Individual score of lambda
                                  EVENT  = 1     ! Number of fission events

  !!
  !! Analog estimator for average lambda (precursors decay constant)
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! myClerk {
  !!   type lambdaAnalogClerk;
  !!   #batches 20000;#
  !!   #map { <tallyMap definition> }#
  !! }
  !!
  !! NOTE: when results are integrated over all time steps, batches has to be provided
  !! otherwise the STD is NaN. Batches needs to be cycles * timeSteps
  !!
  type, public,extends(tallyClerk) :: lambdaAnalogClerk
    private
    integer(shortInt) :: batches = 0
    class(tallyMap), allocatable :: map
  contains
    ! Procedures used during build
    procedure :: init
    procedure :: kill
    procedure :: validReports
    procedure :: getSize

    ! File reports and check status -> run-time procedures
    procedure :: reportSpawn

    ! Output procedures
    procedure  :: display
    procedure  :: print

  end type lambdaAnalogClerk

contains

  !!
  !! Initialise from dictionary and name
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine init(self, dict, name)
    class(lambdaAnalogClerk), intent(inout) :: self
    class(dictionary), intent(in)           :: dict
    character(nameLen), intent(in)          :: name

    ! Needs no settings, just load name
    call self % setName(name)

    ! User provided number of batches to get accurate std
    call dict % getOrDefault(self % batches, 'batches', 0)

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

  end subroutine init

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(lambdaAnalogClerk), intent(inout) :: self

    ! Superclass
    call kill_super(self)

    ! Kill and deallocate map
    if (allocated(self % map)) then
      call self % map % kill()
      deallocate(self % map)
    end if

    self % batches = 0

  end subroutine kill

  !!
  !! Returns array of codes that represent diffrent reports
  !!
  !! See tallyClerk_inter for details
  !!
  function validReports(self) result(validCodes)
    class(lambdaAnalogClerk),intent(in)           :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [ spawn_CODE ]

  end function validReports

  !!
  !! Return memory size of the clerk
  !!
  !! See tallyClerk_inter for details
  !!
  elemental function getSize(self) result(S)
    class(lambdaAnalogClerk), intent(in) :: self
    integer(shortInt)                  :: S

    S = MEM_SIZE
    if (allocated(self % map)) S = S * self % map % bins(0)

  end function getSize

  !!
  !! Process fission report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportSpawn(self, MT, pOld, pNew, xsData, mem)
    class(lambdaAnalogClerk), intent(inout) :: self
    integer(shortInt), intent(in)           :: MT
    class(particle), intent(in)             :: pOld
    class(particleState), intent(in)        :: pNew
    class(nuclearDatabase), intent(inout)   :: xsData
    type(scoreMemory), intent(inout)        :: mem
    integer(shortInt)                       :: binIdx
    integer(longInt)                        :: addr

    ! Check that neutron produced is delayed and score
    if (pNew % type == P_PRECURSOR) then

      ! Get current particle state
      !state = pOld

      ! Find bin index
      if (allocated(self % map)) then
        !binIdx = self % map % map(state)
        ! Map time bin according to precursor time
        binIdx = self % map % map(pNew)
      else
        binIdx = 1
      end if

      ! Return if invalid bin index
      if (binIdx == 0) return

      ! Calculate bin address
      addr = self % getMemAddress() + MEM_SIZE * (binIdx - 1)

      ! Score lambda and flag event
      call mem % score(pNew % lambda, addr + LAMBDA)
      call mem % score(ONE, addr + EVENT)

    end if

  end subroutine reportSpawn

  !!
  !! Display convergance progress on the console
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine display(self, mem)
    class(lambdaAnalogClerk), intent(in) :: self
    type(scoreMemory), intent(in)        :: mem

    ! Does nothing

  end subroutine display

  !!
  !! Write contents of the clerk in the slot to output file
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine print(self, outFile, mem)
    class(lambdaAnalogClerk), intent(in) :: self
    class(outputFile), intent(inout)     :: outFile
    type(scoreMemory), intent(in)        :: mem
    real(defReal)                        :: lam, events, lam_score, STD, STDlam, STDevents
    character(nameLen)                   :: name
    integer(shortInt)                    :: i
    integer(longInt)                     :: addr
    integer(shortInt),dimension(:),allocatable :: resArrayShape

    ! Print to output file
    call outFile % startBlock(self % getName())

    ! Print out map info
    if (allocated(self % map)) then
      call self % map % print(outFile)
      resArrayShape = self % map % binArrayShape()
    else
      resArrayShape = [1]
    end if

    name = 'Res'

    call outFile % startArray(name, resArrayShape)

    ! Print results to the file
    do i = 1, product(resArrayShape)

      addr = self % getMemAddress() + MEM_SIZE * (i - 1)

      ! NOTE: In time dependent calculations, when results are integrated over all time,
      ! the number of batches used to average the results is the number of cycles
      ! times the number of time steps. This must be provided by the user otherwise
      ! the STD will be NaN
      if (self % batches /= 0) then
        call mem % getResult(lam, STDlam, addr + LAMBDA, self % batches)
        call mem % getResult(events, STDevents, addr + EVENT, self % batches)
      else
        call mem % getResult(lam, STDlam, addr + LAMBDA)
        call mem % getResult(events, STDevents, addr + EVENT)
      end if

      lam_score = lam/events
      STD = lam_score * sqrt((STDlam/lam)**2 + (STDevents/events)**2)

      call outFile % addResult(lam_score, STD)

    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module lambdaAnalogClerk_class
