module timeDependentPhysicsPackage_class

  use numPrecision
  use universalVariables
  use endfConstants
  use genericProcedures,              only : fatalError, printFishLineR, numToChar, rotateVector
  use hashFunctions_func,             only : FNV_1
  use dictionary_class,               only : dictionary
  use outputFile_class,               only : outputFile

  ! Timers
  use timer_mod,                      only : registerTimer, timerStart, timerStop, &
                                             timerTime, timerReset, secToChar

  ! Particle classes and Random number generator
  use particle_class,                 only : particle, P_NEUTRON
  use particleDungeon_class,          only : particleDungeon
  use source_inter,                   only : source
  use RNG_class,                      only : RNG

  ! Physics package interface
  use physicsPackage_inter,           only : physicsPackage

  ! Geometry
  use geometry_inter,                 only : geometry
  use geometryReg_mod,                only : gr_geomPtr  => geomPtr, gr_geomIdx  => geomIdx
  use geometryFactory_func,           only : new_geometry

  ! Nuclear Data
  use materialMenu_mod,               only : mm_nMat           => nMat
  use nuclearDataReg_mod,             only : ndReg_init        => init ,&
                                             ndReg_activate    => activate ,&
                                             ndReg_display     => display, &
                                             ndReg_kill        => kill, &
                                             ndReg_get         => get ,&
                                             ndReg_getMatNames => getMatNames
  use nuclearDatabase_inter,          only : nuclearDatabase
  use neutronMaterial_inter,          only : neutronMaterial, neutronMaterial_CptrCast
  use ceNeutronMaterial_class,        only : ceNeutronMaterial
  use mgNeutronMaterial_inter,        only : mgNeutronMaterial
  use fissionCE_class,                only : fissionCE, fissionCE_TptrCast
  use fissionMG_class,                only : fissionMG, fissionMG_TptrCast
  use ceNeutronDatabase_inter,        only : ceNeutronDatabase, ceNeutronDatabase_CptrCast

  ! Operators
  use collisionOperator_class,        only : collisionOperator
  use transportOperator_inter,        only : transportOperator

  ! Tallies
  use tallyCodes
  use tallyAdmin_class,               only : tallyAdmin

  ! Factories
  use transportOperatorFactory_func,  only : new_transportOperator
  use sourceFactory_func,             only : new_source

  implicit none
  private

  !!
  !! Physics Package for time dependent calculations
  !!
  type, public,extends(physicsPackage) :: timeDependentPhysicsPackage
    private
    ! Building blocks
    class(nuclearDatabase), pointer        :: nucData => null()
    class(geometry), pointer               :: geom    => null()
    integer(shortInt)                      :: geomIdx = 0
    type(collisionOperator)                :: collOp
    class(transportOperator), allocatable  :: transOp
    class(RNG), pointer                    :: pRNG    => null()
    type(tallyAdmin),pointer               :: tally   => null()

    ! Settings
    integer(shortInt)  :: N_cycles
    integer(shortInt)  :: N_timeBins
    integer(shortInt)  :: pop
    character(pathLen) :: outputFile
    character(nameLen) :: outputFormat
    integer(shortInt)  :: printSource = 0
    integer(shortInt)  :: particleType
    integer(shortInt)  :: bufferSize
    integer(shortInt)  :: bufferShift
    real(defReal)      :: timeIncrement
    logical(defBool)   :: useCombing
    logical(defBool)   :: usePrecursors

    ! Calculation components
    type(particleDungeon), pointer :: thisTimeInterval                  => null()
    type(particleDungeon), pointer, dimension(:) :: theseBatchDungeons  => null()
    type(particleDungeon), pointer, dimension(:) :: nextBatchDungeons   => null()
    type(particleDungeon), pointer, dimension(:) :: tempBatchDungeons   => null()
    type(particleDungeon), pointer :: precursorDungeon                  => null()
    real(defReal), dimension(:), allocatable :: precursorWeights
    integer(shortInt), dimension(:), allocatable :: batchPops
    class(source), allocatable     :: fixedSource

    ! Timer bins
    integer(shortInt) :: timerMain
    real (defReal)     :: CPU_time_start
    real (defReal)     :: CPU_time_end

  contains
    procedure :: init
    procedure :: printSettings
    procedure :: cycles
    procedure :: collectResults
    procedure :: run
    procedure :: kill

  end type timeDependentPhysicsPackage

contains

  subroutine run(self)
    class(timeDependentPhysicsPackage), intent(inout) :: self

    print *, repeat("<>",50)
    print *, "/\/\ TIME DEPENDENT CALCULATION /\/\"

    call self % cycles(self % tally, self % N_cycles, self % N_timeBins, self % timeIncrement)
    call self % collectResults()

    print *
    print *, "\/\/ END OF TIME DEPENDENT CALCULATION \/\/"
    print *
  end subroutine

  !!
  !!
  !!
  subroutine cycles(self, tally, N_cycles, N_timeBins, timeIncrement)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    type(tallyAdmin), pointer,intent(inout)         :: tally
    integer(shortInt), intent(in)                   :: N_timeBins, N_cycles
    integer(shortInt)                               :: i, t, n, nParticles, batchPop
    integer(shortInt), save                         :: j, bufferExtra
    type(particle), save                            :: p, transferP
    type(particle), save                            :: p_Precursor
    type(particleDungeon), save                     :: buffer
    type(collisionOperator), save                   :: collOp
    class(transportOperator), allocatable, save     :: transOp
    type(RNG), target, save                         :: pRNG
    real(defReal)                                   :: elapsed_T, end_T, T_toEnd
    real(defReal)                                   :: newTotalWeight
    real(defReal)                                   :: decay_T, w_d
    real(defReal), intent(in)                       :: timeIncrement
    integer(shortInt), dimension(N_timeBins)       :: stepPopArray, stepPrecursorArray
    real(defReal), dimension(N_timeBins)           :: stepWeightArray, stepPrecursorWeightArray
    character(100),parameter :: Here ='cycles (timeDependentPhysicsPackage_class.f90)'
    !$omp threadprivate(p, buffer, collOp, transOp, pRNG, j, bufferExtra, transferP)

    !$omp parallel
    ! Create particle buffer
    call buffer % init(self % bufferSize)

    ! Initialise neutron
    p % geomIdx = self % geomIdx
    p % k_eff = ONE

    ! Create a collision + transport operator which can be made thread private
    collOp = self % collOp
    transOp = self % transOp
    !$omp end parallel

    ! Number of particles in each batch
    nParticles = self % pop

    ! Track number of batches for each time step
    allocate(self % batchPops(N_timeBins))
    self % batchPops(1) = N_cycles

    ! Dungeon for each batch
    allocate(self % nextBatchDungeons(self % N_cycles))
    allocate(self % theseBatchDungeons(self % N_cycles))
    allocate(self % tempBatchDungeons(self % N_cycles))

    ! Reset and start timer
    call timerReset(self % timerMain)
    call timerStart(self % timerMain)

    ! First time iteration, fixed source treatment
    ! TODO: add treatment of converged stationary initial source
    print *, 'Time Step: ', 1
    do i = 1, N_cycles
      call self % nextBatchDungeons(i) % init(nParticles)
      print *, 'Cycle: ', i
      call self % fixedSource % generate(self % thisTimeInterval, nParticles, self % pRNG)
      call tally % reportCycleStart(self % thisTimeInterval)

      !$omp parallel do schedule(dynamic)
      gen_t0: do n = 1, nParticles
        pRNG = self % pRNG
        p % pRNG => pRNG
        call p % pRNG % stride(n)
        call self % thisTimeInterval % copy(p, n)
        call self % geom % placeCoord(p % coords)
        p % timeMax = timeIncrement
        call p % savePreHistory()
        p % fate = 0
        history_t0: do
          call transOp % transport(p, tally, self % thisTimeInterval, self % thisTimeInterval)
          if(p % fate == AGED_FATE) then
            call self % nextBatchDungeons(i) % detain(p)
            exit history_t0
          endif
          if(p % isDead) exit history_t0
          call collOp % collide(p, tally, self % thisTimeInterval, self % thisTimeInterval)
          if(p % isDead) exit history_t0
        end do history_t0
      end do gen_t0
      !$omp end parallel do

      call tally % reportCycleEnd(self % thisTimeInterval,1)
      call self % thisTimeInterval % cleanPop()
      call self % pRNG % stride(nParticles)
    end do

    ! Flip batch dungeons
    self % tempBatchDungeons  => self % nextBatchDungeons
    self % nextBatchDungeons  => self % theseBatchDungeons
    self % theseBatchDungeons => self % tempBatchDungeons

    ! Process Remaining time iterations
    do t = 2, N_timeBins
      print *, 'Time Step: ', t
      batchPop = 0
      do i=1, N_cycles
        print *, 'Cycle: ', i
        call self % nextBatchDungeons(i) % init(nParticles)
        if (self % theseBatchDungeons(i) % popSize() == 0) then 
          print *, 'EMPTY SOURCE'
          cycle
        end if
        batchPop = batchPop + 1
        
        if (self % useCombing) then
          call self % theseBatchDungeons(i) % normCombing(nParticles, pRNG)
        end if

        call tally % reportCycleStart(self % theseBatchDungeons(i))
        nParticles = self % theseBatchDungeons(i) % popSize()
        if (nParticles > 0) then
          !$omp parallel do schedule(dynamic)
          gen: do n = 1, nParticles
            pRNG = self % pRNG
            p % pRNG => pRNG
            call p % pRNG % stride(n)
            call self % theseBatchDungeons(i) % copy(p, n)
            p % timeMax = t * timeIncrement
            if (p % time > p % timeMax ) then
              call self % nextBatchDungeons(i) % detain(p)
              cycle gen
            end if
            call self % geom % placeCoord(p % coords)
            p % fate = 0 !update fate
            call p % savePreHistory()
            ! Transport particle untill its death
            history: do
              call collOp % collide(p, tally, self % theseBatchDungeons(i), self % theseBatchDungeons(i))
              if(p % isDead) exit history
              call transOp % transport(p, tally, self % theseBatchDungeons(i), self % theseBatchDungeons(i))
              if(p % fate == AGED_FATE) then
                call self % nextBatchDungeons(i) % detain(p)
                exit history
              endif
              if(p % isDead) exit history
            end do history
          end do gen
          !$omp end parallel do
          call tally % reportCycleEnd(self % theseBatchDungeons(i),t)
          call self % theseBatchDungeons(i) % cleanPop()
          call self % pRNG % stride(nParticles)

        end if
      end do
      self % batchPops(t) = batchPop
      self % tempBatchDungeons  => self % nextBatchDungeons
      self % nextBatchDungeons  => self % theseBatchDungeons
      self % theseBatchDungeons => self % tempBatchDungeons
      
    end do

    call self % tally % setBatchPops(self % batchPops)
    deallocate(self % batchPops)
  end subroutine cycles

  !!
  !! Print calculation results to file
  !!
  subroutine collectResults(self)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    type(outputFile)                                :: out
    character(nameLen)                              :: name
    integer(shortInt)                               :: i

    call out % init(self % outputFormat)

    name = 'seed'
    call out % printValue(self % pRNG % getSeed(),name)

    name = 'pop'
    call out % printValue(self % pop,name)

    name = 'Source_batches'
    call out % printValue(self % N_cycles,name)

    name = 'Time_increment'
    call out % printValue(self % timeIncrement,name)

    name = 'Time_bins'
    call out % printValue(self % N_timeBins,name)

    call cpu_time(self % CPU_time_end)
    name = 'Total_CPU_Time'
    call out % printValue((self % CPU_time_end - self % CPU_time_start),name)

    name = 'Transport_time'
    call out % printValue(timerTime(self % timerMain),name)

    ! Print tally
    call self % tally % print(out)

    call out % writeToFile(self % outputFile)
    
  end subroutine collectResults

  !!
  !! Initialise from individual components and dictionaries for source and tally
  !!
  subroutine init(self, dict)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    class(dictionary), intent(inout)                :: dict
    class(dictionary),pointer                       :: tempDict
    integer(shortInt)                               :: seed_temp
    integer(longInt)                                :: seed
    character(10)                                   :: time
    character(8)                                    :: date
    character(:),allocatable                        :: string
    character(nameLen)                              :: nucData, energy, geomName
    type(outputFile)                                :: test_out
    character(100), parameter :: Here ='init (timeDependentPhysicsPackage_class.f90)'

    call cpu_time(self % CPU_time_start)

    ! Read calculation settings
    call dict % get( self % N_cycles,'cycles')
    call dict % get( self % pop,'pop')
    call dict % get( self % N_timeBins,'timeSteps')
    call dict % get( nucData, 'XSdata')
    call dict % get( energy, 'dataType')
    call dict % get( self % timeIncrement, 'timeIncrement')

    ! Process type of data
    select case(energy)
      case('mg')
        self % particleType = P_NEUTRON_MG
      case('ce')
        self % particleType = P_NEUTRON_CE
      case default
        call fatalError(Here,"dataType must be 'mg' or 'ce'.")
    end select

    ! Read outputfile path
    call dict % getOrDefault(self % outputFile,'outputFile','./output')

    ! Get output format and verify
    ! Initialise output file before calculation (so mistake in format will be cought early)
    call dict % getOrDefault(self % outputFormat, 'outputFormat', 'asciiMATLAB')
    call test_out % init(self % outputFormat)

    ! Parallel buffer size
    call dict % getOrDefault( self % bufferSize, 'buffer', 50)

    ! Whether to use combing (default = no)
    call dict % getOrDefault(self % useCombing, 'combing', .false.)

    ! Whether to implement precursors (default = yes)
    call dict % getOrDefault(self % usePrecursors, 'precursors', .false.)

    ! Register timer
    self % timerMain = registerTimer('transportTime')

    ! Initialise RNG
    allocate(self % pRNG)

    ! *** It is a bit silly but dictionary cannot store longInt for now
    !     so seeds are limited to 32 bits (can be -ve)
    if( dict % isPresent('seed')) then
      call dict % get(seed_temp,'seed')

    else
      ! Obtain time string and hash it to obtain random seed
      call date_and_time(date, time)
      string = date // time
      call FNV_1(string,seed_temp)

    end if
    seed = seed_temp
    call self % pRNG % init(seed)

    ! Read whether to print particle source per cycle
    call dict % getOrDefault(self % printSource, 'printSource', 0)

    ! Build Nuclear Data
    call ndReg_init(dict % getDictPtr("nuclearData"))

    ! Build geometry
    tempDict => dict % getDictPtr('geometry')
    geomName = 'timeDependentGeom'
    call new_geometry(tempDict, geomName)
    self % geomIdx = gr_geomIdx(geomName)
    self % geom    => gr_geomPtr(self % geomIdx)

    ! Activate Nuclear Data *** All materials are active
    call ndReg_activate(self % particleType, nucData, self % geom % activeMats())
    self % nucData => ndReg_get(self % particleType)

    ! Read particle source definition
    tempDict => dict % getDictPtr('source')
    call new_source(self % fixedSource, tempDict, self % geom)

    ! Build collision operator
    tempDict => dict % getDictPtr('collisionOperator')
    call self % collOp % init(tempDict)

    ! Build transport operator
    tempDict => dict % getDictPtr('transportOperator')
    call new_transportOperator(self % transOp, tempDict)

    ! Initialise tally Admin
    tempDict => dict % getDictPtr('tally')
    allocate(self % tally)
    call self % tally % init(tempDict)

    ! Size particle dungeon
    allocate(self % thisTimeInterval)
    call self % thisTimeInterval % init(self % pop)

    ! Size precursor dungeon
    allocate(self % precursorDungeon)
    call self % precursorDungeon % init(10 * self % pop)

    call self % printSettings()

  end subroutine init

  !!
  !! Deallocate memory
  !!
  subroutine kill(self)
    class(timeDependentPhysicsPackage), intent(inout) :: self

    ! TODO: This subroutine

  end subroutine kill

  !!
  !! Print settings of the physics package
  !!
  subroutine printSettings(self)
    class(timeDependentPhysicsPackage), intent(in) :: self
    real(defReal)                                  :: TStart, Tstop, Tincrement

    TStart = 0.0
    Tstop = self % timeIncrement * self % N_timeBins
    Tincrement = self % timeIncrement
    print *, repeat("<>",50)
    print *, "/\/\ TIME DEPENDENT CALCULATION /\/\"
    print *, "Time grid [start, stop, increment]: ", numToChar(TStart), numToChar(Tstop), numToChar(Tincrement)
    print *, "Source batches:                     ", numToChar(self % N_cycles)
    print *, "Population per batch:               ", numToChar(self % pop)
    print *, "Initial RNG Seed:                   ", numToChar(self % pRNG % getSeed())
    print *
    print *, repeat("<>",50)
  end subroutine printSettings

end module timeDependentPhysicsPackage_class
