module hittingProbClerk_class

  use numPrecision
  use tallyCodes
  use genericProcedures,          only : fatalError
  use dictionary_class,           only : dictionary
  use particle_class,             only : particle, particleState, P_NEUTRON, P_PRECURSOR
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

  ! Particle Dungeon
  use particleDungeon_class,  only : particleDungeon

  ! Nuclear Data Interface
  use nuclearDataReg_mod,     only : ndReg_get => get
  implicit none
  private

  !!
  !! temporal estimator of populations
  !! Calculates discrete populations at time boundaries
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
  !! myhittingProbClerk {
  !!   type hittingProbClerk;
  !!   # filter { <tallyFilter definition> } #
  !!   # map    { <tallyMap definition>    } #
  !!   response (resName1 #resName2 ... #)
  !!   resName1 { <tallyResponse definition> }
  !!   #resNamew { <tallyResponse definition #
  !! }
  !!
  type, public, extends(tallyClerk) :: hittingProbClerk
    private
    ! Filter, Map & Vector of Responses
    class(tallyFilter), allocatable                  :: filter
    class(tallyMap), allocatable                     :: map
    type(tallyResponseSlot),dimension(:),allocatable :: response
    real(defReal)                                    :: maxT
    integer(shortInt)                                :: maxPop
    logical(defBool), dimension(:), allocatable      :: firstHitsNeutron, firstHitsPrecursor
    real(defReal), dimension(:), allocatable         :: hittingTimesNeutron, hittingTimesPrecursor
    real(defReal), dimension(:), allocatable         :: currentNeutronPops, currentPrecursorPops
    integer(shortInt)                                :: cycles
    type(particleDungeon), allocatable               :: neutrons
    type(particleDungeon), allocatable               :: precursors

    ! Useful data
    integer(shortInt)  :: width = 0

  contains
    ! Procedures used during build
    procedure  :: init
    procedure  :: kill
    procedure  :: validReports
    procedure  :: getSize

    ! File reports and check status -> run-time procedures
    procedure  :: placeParticleSorted
    procedure  :: reportHittingProbIn
    procedure  :: reportHittingProbOut
    procedure  :: reportCycleEnd

    ! Output procedures
    procedure  :: display
    procedure  :: print
  end type hittingProbClerk

contains

  !!
  !! Initialise clerk from dictionary and name
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine init(self, dict, name)
    class(hittingProbClerk), intent(inout)      :: self
    class(dictionary), intent(in)               :: dict
    character(nameLen), intent(in)              :: name
    character(nameLen),dimension(:),allocatable :: responseNames
    integer(shortInt)                           :: i, j
    class(dictionary), pointer                  :: responseDict
    character(nameLen)                          :: type

    ! Assign name
    call self % setName(name)

    ! Load filetr
    if( dict % isPresent('filter')) then
      call new_tallyFilter(self % filter, dict % getDictPtr('filter'))
    end if

    ! Get max time to consider
    call dict % get(self % maxT,'maxT')

    ! Get population threshold
    call dict % get(self % maxPop,'maxPop')

    ! Get population threshold
    call dict % get(self % cycles,'cycles')

    ! Get names of response dictionaries
    call dict % get(responseNames,'response')

    ! Load responses
    allocate(self % response(size(responseNames)))
    do i=1, size(responseNames)
      call self % response(i) % init(dict % getDictPtr( responseNames(i) ))

      responseDict => dict % getDictPtr( responseNames(i) )
      call responseDict % get(type,'type')

      if (type == "neutronResponse") then
        allocate(self % neutrons)
        call self % neutrons % init(20 * self % maxPop)
        allocate(self % currentNeutronPops(self % cycles))
        allocate(self % firstHitsNeutron(self % cycles))
        allocate(self % hittingTimesNeutron(self % cycles))
        do j=1, self % cycles
          self % currentNeutronPops(j) = ZERO
          self % firstHitsNeutron(j) = .true.
          self % hittingTimesNeutron(j) = -ONE
        end do

      else if (type == "precursorResponse") then
        allocate(self % precursors)
        call self % precursors % init(5 * self % maxPop)
        allocate(self % currentPrecursorPops(self % cycles))
        allocate(self % firstHitsPrecursor(self % cycles))
        allocate(self % hittingTimesPrecursor(self % cycles))
        do j=1, self % cycles
          self % currentPrecursorPops(j) = ZERO
          self % firstHitsPrecursor(j) = .true.
          self % hittingTimesPrecursor(j) = -ONE
        end do
      end if
    end do

    ! Set width
    self % width = size(responseNames)

  end subroutine init

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(hittingProbClerk), intent(inout) :: self

    ! Superclass
    call kill_super(self)

    ! Kill and deallocate filter
    if(allocated(self % filter)) then
      deallocate(self % filter)
    end if

    ! Kill and deallocate map
    if(allocated(self % map)) then
      call self % map % kill()
      deallocate(self % map)
    end if

    ! Kill and deallocate responses
    if(allocated(self % response)) then
      deallocate(self % response)
    end if

    self % width = 0

  end subroutine kill

  !!
  !! Returns array of codes that represent diffrent reports
  !!
  !! See tallyClerk_inter for details
  !!
  function validReports(self) result(validCodes)
    class(hittingProbClerk),intent(in)           :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [hittingProb_CODE, cycleEnd_CODE]

  end function validReports

  !!
  !! Return memory size of the clerk
  !!
  !! See tallyClerk_inter for details
  !!
  elemental function getSize(self) result(S)
    class(hittingProbClerk), intent(in) :: self
    integer(shortInt)                 :: S

    S = size(self % response)
    if(allocated(self % map)) S = S * self % map % bins(0)

  end function getSize

  subroutine placeParticleSorted(self, p, particles, score)
    class(hittingProbClerk), intent(inout) :: self
    class(particle), intent(in)            :: p
    type(particleDungeon), intent(inout)   :: particles
    real(defReal), intent(in)              :: score
    real(defReal)                          :: lowestTime
    type(particle)                         :: p_pre, p_temp
    integer(shortInt)                      :: k

    p_pre = p
    p_pre % w = score

    if (particles % popSize() == 0) then
      call particles % detain(p_pre)
      lowestTime = p_pre % time
    
    else
      k = 1
      sortLoop: do
        call particles % copy(p_temp, k)

        if (p_pre % time <= p_temp % time) then
          if (k == 1) lowestTime = p_pre % time
          call particles % replace(p_pre, k)
          p_pre = p_temp
        end if
        k = k + 1
        if (k > particles % popSize()) then
          call particles % detain(p_pre)
          exit sortLoop
        end if
      end do sortLoop

    end if
  end subroutine placeParticleSorted


  !!
  !! Process temporal population report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportHittingProbIn(self, p, mem)
    class(hittingProbClerk), intent(inout) :: self
    class(particle), intent(in)            :: p
    type(scoreMemory), intent(inout)       :: mem
    class(nuclearDatabase), pointer        :: xsData
    integer(shortInt)                      :: i, k, batchIdx
    real(defReal)                          :: scoreVal
    type(particleState)                    :: state 
    type(particle)                         :: p_pre, p_temp
    character(100), parameter :: Here =' reporthittingProbIn (hittingProbClerk_class.f90)'

    batchIdx = mod(mem % batchN, self % cycles) + 1
    if (self % firstHitsNeutron(batchIdx) .eqv. .false.) return
    if (p % time == ZERO) then 
      self % currentNeutronPops(batchIdx) = self % currentNeutronPops(batchIdx) + ONE
      return
    end if

    if (p % time <= self % maxT) then
      ! Append all bins
      !do i=1, self % width

        ! Get current particle state
        state = p

        ! Check if within filter
        if(allocated( self % filter)) then
          if(self % filter % isFail(state)) return
        end if

        xsData => ndReg_get(p % getType(), where = Here)
        scoreVal = self % response(1) % get(p, xsData) !self % response(i) % get(p, xsData) 

        if (scoreVal == ZERO) return
        if (allocated(self % neutrons) .and. p % type == P_NEUTRON) then
          !$OMP CRITICAL
          call self % placeParticleSorted(p, self % neutrons, 1.0_defReal)
          !$OMP END CRITICAL
        end if

      !  if (allocated(self % precursors) .and. p % type == P_PRECURSOR) then
      !    !$OMP CRITICAL
      !    call self % placeParticleSorted(p, self % precursors, 1.0_defReal)
      !    !$OMP END CRITICAL
      !  end if
      !end do
    end if

  end subroutine reportHittingProbIn

  !!
  !! Process temporal population report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportHittingProbOut(self, p, mem)
    class(hittingProbClerk), intent(inout) :: self
    class(particle), intent(in)            :: p
    type(scoreMemory), intent(inout)       :: mem
    class(nuclearDatabase), pointer        :: xsData
    integer(shortInt)                      :: i, k, batchIdx
    real(defReal)                          :: scoreVal, lowestTime
    type(particleState)                    :: state 
    type(particle)                         :: p_pre, p_temp
    character(100), parameter :: Here =' reporthittingProbOut (hittingProbClerk_class.f90)'

    batchIdx = mod(mem % batchN, self % cycles) + 1
    if (self % firstHitsNeutron(batchIdx) .eqv. .false.) return

    if (p % time <= self % maxT) then
      ! Append all bins
      !do i=1, self % width

        ! Get current particle state
        state = p

        ! Check if within filter
        if(allocated( self % filter)) then
          if(self % filter % isFail(state)) return
        end if

        xsData => ndReg_get(p % getType(), where = Here)
        scoreVal = -self % response(1) % get(p, xsData) !-self % response(i) % get(p, xsData)

        if (scoreVal == ZERO) return

        if (allocated(self % neutrons) .and. p % type == P_NEUTRON) then
          !$OMP CRITICAL
          call self % placeParticleSorted(p, self % neutrons, -1.0_defReal)
          !$OMP END CRITICAL
        end if

        !if (allocated(self % precursors) .and. p % type == P_PRECURSOR) then
        !  !$OMP CRITICAL
        !  call self % placeParticleSorted(p, self % precursors, -1.0_defReal)
        !  !$OMP END CRITICAL
        !end if

      !end do
    end if

  end subroutine reportHittingProbOut

  !!
  !! Process end of the cycle
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportCycleEnd(self, end, mem)
    class(hittingProbClerk), intent(inout)  :: self
    class(particleDungeon), intent(in)      :: end
    type(scoreMemory), intent(inout)        :: mem
    integer(shortInt)                       :: batchIdx, i, binIdx = 1
    type(particle)                          :: p_temp
    integer(longInt)                        :: adrr

    adrr = self % getMemAddress() + self % width * (binIdx - 1)  - 1
    batchIdx = mod(mem % batchN, self % cycles) + 1
    !if (batchIdx == 1)  print *, self % neutrons % popSize()

    if (allocated(self % neutrons)) then
      if (self % firstHitsNeutron(batchIdx) .eqv. .true.) then
        do i = 1, self % neutrons % popSize()
          call self % neutrons % copy(p_temp, i)
          self % currentNeutronPops(batchIdx) = self % currentNeutronPops(batchIdx) + p_temp % w
          if (self % currentNeutronPops(batchIdx) >= self % maxPop) then
            self % firstHitsNeutron(batchIdx) = .false.
            self % hittingTimesNeutron(batchIdx) = p_temp % time
            call mem % score(ONE, adrr + 1)
            exit
          end if
          !print *, p_temp % w, self % currentNeutronPops(batchIdx)
        end do
      end if
      call self % neutrons % cleanPop()
      !print *, '---', self % currentNeutronPops(batchIdx)
      !self % currentNeutronPops(batchIdx) = ZERO
    !if (batchIdx == 1) print *, '---', self % currentNeutronPops(1)
    !if (batchIdx == 1)  print *, self % neutrons % popSize()
    end if

    !if (allocated(self % precursors)) then
    !  if (self % firstHitsPrecursor(batchIdx) .eqv. .true.) then
    !    do i = 1, self % precursors % popSize()
    !      call self % precursors % copy(p_temp, i)
    !      self % currentPrecursorPops(batchIdx) = self % currentPrecursorPops(batchIdx) + p_temp % w
    !      if (self % currentPrecursorPops(batchIdx) >= self % maxPop) then
    !        self % firstHitsPrecursor(batchIdx) = .false.
    !        self % hittingTimesPrecursor(batchIdx) = p_temp % time
    !        exit
    !      end if
    !    end do
    !  end if
    !  call self % precursors % cleanPop()
    !end if

  end subroutine reportCycleEnd

  !!
  !! Display convergance progress on the console
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine display(self, mem)
    class(hittingProbClerk), intent(in)  :: self
    type(scoreMemory), intent(in)      :: mem

    print *, 'hittingProbClerk does not support display yet'

  end subroutine display

  !!
  !! Write contents of the clerk to output file
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine print(self, outFile, mem)
    class(hittingProbClerk), intent(in)        :: self
    class(outputFile), intent(inout)           :: outFile
    type(scoreMemory), intent(in)              :: mem
    real(defReal)                              :: val, std
    integer(shortInt)                          :: i
    integer(shortInt),dimension(:),allocatable :: resArrayShape
    character(nameLen)                         :: name

    ! Begin block
    call outFile % startBlock(self % getName())

    ! Write results.
    ! Get shape of result array

    resArrayShape = [size(self % response)]

    ! Start array
    name ='HittingProbability'
    call outFile % startArray(name, resArrayShape)

    ! Print results to the file
    do i=1,product(resArrayShape)
      call mem % getResult(val, self % getMemAddress() - 1 + 1)
      call outFile % addValue(val)
    end do

    call outFile % endArray()

    ! Start array
    resArrayShape = [self % cycles]
    name ='HittingTimes'
    call outFile % startArray(name, resArrayShape)

    ! Print results to the file
    do i=1, product(resArrayShape)
      val = self % hittingTimesNeutron(i)
      call outFile % addValue(val)
    end do

    call outFile % endArray()

    call outFile % endBlock()

  end subroutine print

end module hittingProbClerk_class
