module lifetimeClerk_class

  use numPrecision
  use tallyCodes
  use dictionary_class,      only : dictionary
  use particle_class,        only : particle, particleState
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
  integer(shortInt), parameter :: MEM_SIZE = 2
  integer(longInt), parameter  :: TIME   = 0, &  ! Lifetime score
                                  EVENT  = 1     ! Number of particle death events

  !!
  !! Analog estimator for average neutron lifetime
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! myClerk {
  !!   type lifetimeClerk;
  !!   #batches 20000;#
  !!   #map { <tallyMap definition> }#
  !! }
  !!
  !! NOTE: when results are integrated over all time steps, batches has to be provided
  !! otherwise the STD is NaN. Batches needs to be cycles * timeSteps
  !!
  type, public,extends(tallyClerk) :: lifetimeClerk
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
    procedure :: reportHist

    ! Output procedures
    procedure  :: display
    procedure  :: print

  end type lifetimeClerk

contains

  !!
  !! Initialise from dictionary and name
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine init(self, dict, name)
    class(lifetimeClerk), intent(inout) :: self
    class(dictionary), intent(in)       :: dict
    character(nameLen), intent(in)      :: name

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
    class(lifetimeClerk), intent(inout) :: self

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
    class(lifetimeClerk),intent(in)            :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [ hist_CODE ]

  end function validReports

  !!
  !! Return memory size of the clerk
  !!
  !! See tallyClerk_inter for details
  !!
  elemental function getSize(self) result(S)
    class(lifetimeClerk), intent(in) :: self
    integer(shortInt)                :: S

    S = MEM_SIZE
    if (allocated(self % map)) S = S * self % map % bins(0)

  end function getSize

  !!
  !! Process fission report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportHist(self, p, xsData, mem)
    class(lifetimeClerk), intent(inout)   :: self
    class(particle), intent(in)           :: p
    class(nuclearDatabase), intent(inout) :: xsData
    type(scoreMemory), intent(inout)      :: mem
    real(defReal)                         :: lifetime
    type(particleState)                   :: state
    integer(shortInt)                     :: binIdx
    integer(longInt)                      :: addr

    ! Get current particle state
    state = p

    ! NB: modify the time in the particle state for mapping: this lifetime contribution
    ! goes into the time bin corresponding to when the particle was born
    state % time = p % timeBirth

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

    ! NOTE: it doesn't matter in a fully analog calculation, but the lifetime must
    ! be multiplied by the particle weight for a correct average
    lifetime = (p % time - p % timeBirth) * p % w

    ! Score lifetime and flag death event
    call mem % score(lifetime, addr + TIME)
    call mem % score(p % w, addr + EVENT)

  end subroutine reportHist

  !!
  !! Display convergance progress on the console
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine display(self, mem)
    class(lifetimeClerk), intent(in) :: self
    type(scoreMemory), intent(in)    :: mem

    ! Does nothing

  end subroutine display

  !!
  !! Write contents of the clerk in the slot to output file
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine print(self, outFile, mem)
    class(lifetimeClerk), intent(in) :: self
    class(outputFile), intent(inout) :: outFile
    type(scoreMemory), intent(in)    :: mem
    real(defReal)                    :: life, events, life_score, STD, STDlife, STDevents
    character(nameLen)               :: name
    integer(shortInt)                :: i
    integer(longInt)                 :: addr
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
        call mem % getResult(life, STDlife, addr + TIME, self % batches)
        call mem % getResult(events, STDevents, addr + EVENT, self % batches)
      else
        call mem % getResult(life, STDlife, addr + TIME)
        call mem % getResult(events, STDevents, addr + EVENT)
      end if

      life_score = life/events
      STD = life_score * sqrt((STDlife/life)**2 + (STDevents/events)**2)

      call outFile % addResult(life_score, STD)

    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module lifetimeClerk_class
