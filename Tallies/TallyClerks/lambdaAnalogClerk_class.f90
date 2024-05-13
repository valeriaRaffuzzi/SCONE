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
  use tallyMap_inter,             only : tallyMap
  use tallyMapFactory_func,       only : new_tallyMap

  ! Nuclear Data Interfaces
  use nuclearDatabase_inter, only : nuclearDatabase

  implicit none
  private

  !! Locations of diffrent bins wrt memory address of the clerk
  integer(shortInt), parameter :: MEM_SIZE = 3
  integer(longInt), parameter  :: LAMBDA = 0, &  ! Individual score of lambda
                                  EVENT  = 1, &  ! Number of fission events
                                  LAMBDA_MEAN = 2 ! Averaged value

  !!
  !! Analog estimator for average lambda (precursors decay constant)
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! myClerk {
  !!   type lambdaAnalogClerk;
  !!   #map { <tallyMap definition> }#
  !! }
  !!
  type, public,extends(tallyClerk) :: lambdaAnalogClerk
    private
    class(tallyMap), allocatable :: map
  contains
    ! Procedures used during build
    procedure :: init
    procedure :: kill
    procedure :: validReports
    procedure :: getSize

    ! File reports and check status -> run-time procedures
    procedure :: reportSpawn
    procedure :: reportCycleEnd

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

  end subroutine kill

  !!
  !! Returns array of codes that represent diffrent reports
  !!
  !! See tallyClerk_inter for details
  !!
  function validReports(self) result(validCodes)
    class(lambdaAnalogClerk),intent(in)           :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [ spawn_CODE, cycleEnd_CODE ]

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
    type(particleState)                     :: state
    integer(shortInt)                       :: binIdx
    integer(longInt)                        :: addr

    ! Check that neutron produced is delayed and score
    if (pNew % type == P_PRECURSOR) then

      ! Get current particle state
      state = pOld

      ! Find bin index
      if (allocated(self % map)) then
        binIdx = self % map % map(state)
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
  !! Process end of the cycle
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportCycleEnd(self, end, mem)
    class(lambdaAnalogClerk), intent(inout) :: self
    class(particleDungeon), intent(in)  :: end
    type(scoreMemory), intent(inout)    :: mem
    integer(longInt)                    :: addr
    integer(shortInt)                   :: i, N
    real(defReal)                       :: lam, events, lambda_score

    ! Score average
    if (mem % lastCycle()) then

      if (allocated(self % map)) then
        N = self % map % bins(0)
      else
        N = 1
      end if

      ! Loop over map bins
      do i = 1, N

        ! Calculate bin address
        addr = self % getMemAddress() + MEM_SIZE * (i - 1)

        lam    = mem % getScore(addr + LAMBDA)
        events = mem % getScore(addr + EVENT)

        ! Calculate average lambda
        if (events == ZERO) then
          lambda_score = ZERO
        else
          lambda_score = lam/events
        end if

        call mem % accumulate(lambda_score, addr + LAMBDA_MEAN)

      end do

    end if

  end subroutine reportCycleEnd

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
    real(defReal)                        :: lam, STD
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
      call mem % getResult(lam, STD, addr + LAMBDA_MEAN)
      call outFile % addResult(lam, STD)
    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module lambdaAnalogClerk_class
