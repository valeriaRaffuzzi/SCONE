module betaLambdaClerk_class

  use numPrecision
  use tallyCodes
  use endfConstants
  use universalVariables
  use genericProcedures,          only : fatalError
  use dictionary_class,           only : dictionary
  use particle_class,             only : particle, particleState
  use particleDungeon_class,      only : particleDungeon
  use outputFile_class,           only : outputFile
  use scoreMemory_class,          only : scoreMemory
  use tallyClerk_inter,           only : tallyClerk, kill_super => kill

  ! Nuclear Data interface
  use nuclearDatabase_inter,      only : nuclearDatabase

  ! Tally Filters
  use tallyFilter_inter,          only : tallyFilter
  use tallyFilterFactory_func,    only : new_tallyFilter

  ! Tally Maps
  use tallyMap_inter,             only : tallyMap
  use tallyMapFactory_func,       only : new_tallyMap

  ! Tally Responses
  use tallyResponseSlot_class,    only : tallyResponseSlot

  implicit none
  private

  !!
  !! Collision estimator of reaction rates
  !! Calculates flux weighted integral from collisions
  !!
  !! Private Members:
  !!   filter   -> Space to store tally Filter
  !!   map      -> Space to store tally Map
  !!   response -> Array of responses
  !!   width    -> Number of responses (# of result bins for each map position)
  !!
  !! Interface
  !!   tallyClerk Interface
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! mybetaLambdaClerk {
  !!   type betaLambdaClerk;
  !!   # filter { <tallyFilter definition> } #
  !!   # map    { <tallyMap definition>    } #
  !!   response (resName1 #resName2 ... #)
  !!   resName1 { <tallyResponse definition> }
  !!   #resNamew { <tallyResponse definition #
  !! }
  !!
  type, public, extends(tallyClerk) :: betaLambdaClerk
    private
    ! Filter, Map & Vector of Responses
    class(tallyMap), allocatable :: map

    ! Intermediate data
    real(defReal), dimension(:), allocatable :: beta
    real(defReal), dimension(:), allocatable :: lambda
    real(defReal), dimension(:), allocatable :: fissTot
    real(defReal), dimension(:), allocatable :: lambdaTot

    ! Useful data
    integer(shortInt)  :: width
    integer(shortInt)  :: precN
    integer(shortInt)  :: mapN

  contains
    ! Procedures used during build
    procedure  :: init
    procedure  :: kill
    procedure  :: validReports
    procedure  :: getSize

    ! File reports and check status -> run-time procedures
    procedure  :: reportSpawn
    procedure  :: reportCycleEnd

    ! Output procedures
    procedure  :: display
    procedure  :: print

    ! Helper
    procedure  :: reset

  end type betaLambdaClerk

contains

  !!
  !! Initialise clerk from dictionary and name
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine init(self, dict, name)
    class(betaLambdaClerk), intent(inout) :: self
    class(dictionary), intent(in)         :: dict
    character(nameLen), intent(in)        :: name

    ! Assign name
    call self % setName(name)

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
      self % mapN = self % map % bins(0)
    else
      self % mapN = 1
    end if

    call dict % getOrDefault(self % precN, 'precursorN', 6)
    self % width = self % precN * 2

    allocate(self % beta(self % mapN * self % precN),      &
             self % lambda(self % mapN * self % precN),    &
             self % lambdaTot(self % mapN * self % precN), &
             self % fissTot(self % mapN))

    call self % reset()

  end subroutine init

  !!
  !! Initialise stufff
  !!
  subroutine reset(self)
    class(betaLambdaClerk), intent(inout) :: self

    self % fissTot   = ZERO
    self % lambdaTot = ZERO
    self % beta   = ZERO
    self % lambda = ZERO

  end subroutine reset

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(betaLambdaClerk), intent(inout) :: self

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
    class(betaLambdaClerk),intent(in)           :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [spawn_CODE, cycleEnd_Code]

  end function validReports

  !!
  !! Return memory size of the clerk
  !!
  !! See tallyClerk_inter for details
  !!
  elemental function getSize(self) result(S)
    class(betaLambdaClerk), intent(in) :: self
    integer(shortInt)                  :: S

    S = self % width * self % mapN

  end function getSize

  !!
  !! Process incoming collision report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportSpawn(self, MT, pOld, pNew, xsData, mem)
    class(betaLambdaClerk), intent(inout) :: self
    integer(shortInt), intent(in)         :: MT
    class(particle), intent(in)           :: pOld
    class(particleState), intent(in)      :: pNew
    class(nuclearDatabase), intent(inout) :: xsData
    type(scoreMemory), intent(inout)      :: mem
    integer(shortInt)                     :: binIdx, idx

    if (MT == N_FISSION) then

      ! Find bin index
      if (allocated(self % map)) then
        binIdx = self % map % map(pNew)
      else
        binIdx = 1
      end if

      ! Return if invalid bin index
      if (binIdx == 0) return

      ! Fission weight
      self % fissTot(binIdx) = self % fissTot(binIdx) + pNew % wgt

      if (pNew % precID <= 0 .or. pNew % precID > self % precN .or. pNew % lambda >= huge(pNew % lambda)) return

      ! Calculate bin address
      idx = self % width * (binIdx - 1)

      self % lambdaTot(idx + pNew % precID) = self % lambdaTot(idx + pNew % precID) + pNew % wgt
      self % lambda(idx + pNew % precID) = self % lambda(idx + pNew % precID) + pNew % lambda * pNew % wgt
      self % beta(idx + pNew % precID)   = self % beta(idx + pNew % precID) + pNew % wgt

    end if

  end subroutine reportSpawn

  !!
  !! Process cycle end
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportCycleEnd(self, end, mem)
    class(betaLambdaClerk), intent(inout) :: self
    class(particleDungeon), intent(in)    :: end
    type(scoreMemory), intent(inout)      :: mem
    integer(shortInt)                     :: i, j, binIdx, idx
    integer(longInt)                      :: addr
    real(defReal)                         :: beta, lambda, betaTot, lambdaTot

    if (mem % lastCycle()) then

      addr = self % getMemAddress() - 1

      ! Normalise and accumulate estimates
      do i = 1, self % mapN

        betaTot = self % fissTot(i)

        ! Calculate bin address
        binIdx    = self % width * (i - 1)

        do j = 1, self % precN

          idx = binIdx + j
          if (betaTot > ZERO) then
            beta = self % beta(idx) / betaTot
          else
            beta = ZERO
          end if
          lambdaTot = self % lambdaTot(idx)
          if (lambdaTot > ZERO) then
            lambda = self % lambda(idx) / lambdaTot
          else
            lambda = ZERO
          end if

          call mem % accumulate(beta, addr + idx)
          call mem % accumulate(lambda, addr + self % precN + idx)

        end do

      end do

      call self % reset()

    end if

  end subroutine reportCycleEnd

  !!
  !! Display convergance progress on the console
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine display(self, mem)
    class(betaLambdaClerk), intent(in)  :: self
    type(scoreMemory), intent(in)      :: mem

    print *, 'betaLambdaClerk does not support display yet'

  end subroutine display

  !!
  !! Write contents of the clerk to output file
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine print(self, outFile, mem)
    class(betaLambdaClerk), intent(in)         :: self
    class(outputFile), intent(inout)           :: outFile
    type(scoreMemory), intent(in)              :: mem
    real(defReal)                              :: val, std
    integer(shortInt)                          :: i, j, binIdx, idx
    integer(longInt)                           :: addr
    integer(shortInt),dimension(:),allocatable :: resArrayShape
    character(nameLen)                         :: name

    ! Begin block
    call outFile % startBlock(self % getName())

    ! If clerk has map print map information
    if (allocated(self % map)) then
      call self % map % print(outFile)
    end if

    ! Write results
    ! Get shape of result array
    resArrayShape = [self % precN, self % mapN]

    addr = self % getMemAddress() - 1

    ! Start array
    name ='BETA'
    call outFile % startArray(name, resArrayShape)

    ! Print results to the file
    do i = 1, self % mapN

      ! Calculate bin address
      binIdx = self % width * (i - 1)

      do j = 1, self % precN
        idx = binIdx + j
        call mem % getResult(val, std, addr + idx)
        call outFile % addResult(val,std)
      end do

    end do

    call outFile % endArray()

    ! Start array
    name ='LAMBDA'
    call outFile % startArray(name, resArrayShape)

    ! Print results to the file
    do i = 1, self % mapN

      ! Calculate bin address
      binIdx = self % width * (i - 1)

      do j = 1, self % precN
        idx = binIdx + j
        call mem % getResult(val, std, addr + self % precN + idx)
        call outFile % addResult(val,std)
      end do

    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module betaLambdaClerk_class
