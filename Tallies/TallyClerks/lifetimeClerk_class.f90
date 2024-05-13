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
  integer(shortInt), parameter :: MEM_SIZE = 3
  integer(longInt), parameter  :: TIME   = 0, &  ! Lifetime score
                                  EVENT  = 1, &  ! Number of particle death events
                                  LIFETIME_MEAN = 2 ! Averaged value

  !!
  !! Analog estimator for average neutron lifetime
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! myClerk {
  !!   type lifetimeClerk;
  !! }
  !!
  type, public,extends(tallyClerk) :: lifetimeClerk
    private
    class(tallyMap), allocatable :: map
  contains
    ! Procedures used during build
    procedure :: init
    procedure :: kill
    procedure :: validReports
    procedure :: getSize

    ! File reports and check status -> run-time procedures
    procedure :: reportHist
    procedure :: reportCycleEnd

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

  end subroutine kill

  !!
  !! Returns array of codes that represent diffrent reports
  !!
  !! See tallyClerk_inter for details
  !!
  function validReports(self) result(validCodes)
    class(lifetimeClerk),intent(in)            :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [ hist_CODE, cycleEnd_CODE ]

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

    lifetime = p % time - p % timeBirth

    ! Score lifetime and flag death event
    call mem % score(lifetime, addr + TIME)
    call mem % score(ONE, addr + EVENT)

  end subroutine reportHist

  !!
  !! Process end of the cycle
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportCycleEnd(self, end, mem)
    class(lifetimeClerk), intent(inout) :: self
    class(particleDungeon), intent(in)  :: end
    type(scoreMemory), intent(inout)    :: mem
    integer(longInt)                    :: addr
    integer(shortInt)                   :: i, N
    real(defReal)                       :: life, events, time_score

    ! Score average number of neutrons produced by fission
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

        life   = mem % getScore(addr + TIME)
        events = mem % getScore(addr + EVENT)

        if (events /= ZERO) then
          time_score = life/events
          call mem % accumulate(time_score, addr + LIFETIME_MEAN)
        end if

      end do

    end if

  end subroutine reportCycleEnd

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
    real(defReal)                    :: life, STD
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
      call mem % getResult(life, STD, addr + LIFETIME_MEAN)
      call outFile % addResult(life, STD)
    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module lifetimeClerk_class
