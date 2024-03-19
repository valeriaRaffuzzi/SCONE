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
    logical(defBool)   :: useImplicit
    integer(shortInt), dimension(:), allocatable  :: batchPopulations

    real(defReal) :: minWgt = 0.25
    real(defReal) :: maxWgt = 1.25
    real(defReal) :: avWgt = 0.5

    ! Calculation components
    type(particleDungeon), pointer, dimension(:) :: currentTime       => null()
    type(particleDungeon), pointer, dimension(:) :: nextTime          => null()
    type(particleDungeon), pointer, dimension(:) :: tempTime          => null()
    type(particleDungeon), pointer, dimension(:) :: precursorDungeons => null() 
    real(defReal), dimension(:), allocatable :: precursorWeights
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
    procedure :: russianRoulette

  end type timeDependentPhysicsPackage

contains

  subroutine run(self)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    real(defReal) :: simTime

    print *, repeat("<>",50)
    print *, "/\/\ TIME DEPENDENT CALCULATION /\/\"

    call self % cycles(self % tally, self % N_cycles, self % N_timeBins, self % timeIncrement, simTime)
    call self % collectResults()

    print *
    print *, "\/\/ END OF TIME DEPENDENT CALCULATION \/\/"
    print *
  end subroutine

  !!
  !!
  !!
  subroutine cycles(self, tally, N_cycles, N_timeBins, timeIncrement, simTime)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    type(tallyAdmin), pointer,intent(inout)         :: tally
    real(defReal), intent(inout)                    :: simTime
    integer(shortInt), intent(in)                   :: N_timeBins, N_cycles
    integer(shortInt)                               :: i, t, n, nParticles, nDelayedParticles
    type(particle), save                            :: p, p_Precursor
    type(particleDungeon), save                     :: buffer
    type(collisionOperator), save                   :: collOp
    class(transportOperator), allocatable, save     :: transOp
    type(RNG), target, save                         :: pRNG
    real(defReal)                                   :: elapsed_T, end_T, T_toEnd, decay_T, w_d
    real(defReal), intent(in)                       :: timeIncrement
    character(100),parameter :: Here ='cycles (timeDependentPhysicsPackage_class.f90)'
    !$omp threadprivate(p, buffer, collOp, transOp, pRNG)

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

    ! Reset and start timer
    call timerReset(self % timerMain)
    call timerStart(self % timerMain)

    do t = 1, N_timeBins
      do i = 1, N_cycles

        if (t == 1) then 
          call self % fixedSource % generate(self % currentTime(i), nParticles, self % pRNG)
        end if


        if (self % usePrecursors .and. (self % useImplicit .eqv. .true.)) then
          nDelayedParticles = self % precursorDungeons(i) % popSize()
          if (nDelayedParticles > 0) then 
            ! Forced precursor decay
            do n = 1, nDelayedParticles
              call self % precursorDungeons(i) % copy(p_Precursor, n)

              ! Sample decay time
              decay_T = timeIncrement * (t - pRNG % get())

              ! Weight adjustment

              w_d = p_Precursor % getPrecWeight(decay_T, timeIncrement)

              pRNG = self % pRNG
              p_Precursor % pRNG => pRNG
              call p_Precursor % pRNG % stride(n)

              ! Update parameters
              p_Precursor % E = p_Precursor % getDelayedEnergy(decay_T)
              p_Precursor % type = P_NEUTRON
              p_Precursor % time = decay_T
              p_Precursor % w = w_d
              !print *, 'precursor weight', numToChar(w_d)
              !print *, numToChar(p_Precursor % E)

              p_Precursor % lambda_i = -ONE
              p_Precursor % fd_i = -ONE


              if (p % time > t * timeIncrement) then 
                p % fate = no_FATE
                call self % nextTime(i) % detain(p)
              else
                p % fate = no_FATE
                ! Add to current dungeon
                call self % currentTime(i) % detain(p_Precursor)
              end if


            end do


          end if
        end if





        if (self % useCombing .and. t /= 1) then
          call self % currentTime(i) % normCombing(self % pop, pRNG)
        end if


        call tally % reportCycleStart(self % currentTime(i))
        nParticles = self % currentTime(i) % popSize()

        !$omp parallel do schedule(dynamic)
        gen: do n = 1, nParticles
          pRNG = self % pRNG
          p % pRNG => pRNG
          call p % pRNG % stride(n)
          call self % currentTime(i) % copy(p, n)
          bufferLoop: do

            if ((p % fate == aged_FATE) .or. (p % fate == no_FATE)) then
              p % fate = no_FATE
              p % isdead = .false.
            else
              p % isdead = .true.
            end if

            call self % geom % placeCoord(p % coords)
            p % timeMax = t * timeIncrement
            call p % savePreHistory()

            ! Transport particle untill its death
            history: do
              if(p % isDead) exit history
              call transOp % transport(p, tally, buffer, buffer)
              if(p % isDead) exit history
              if(p % fate == AGED_FATE) then
                call self % nextTime(i) % detain(p)
                exit history
              endif
              if (self % usePrecursors) then
                call collOp % collide(p, tally, self % precursorDungeons(i), buffer)
              else
                call collOp % collide(p, tally, buffer, buffer)
              end if
              if(p % isDead) exit history
            end do history

            ! Clear out buffer
            if (buffer % isEmpty()) then
              exit bufferLoop
            else
              call buffer % release(p)
            end if

          end do bufferLoop
        end do gen
        !$omp end parallel do

        if (self % usePrecursors .and. (self % useImplicit .eqv. .false.)) then

          nDelayedParticles = self % precursorDungeons(i) % popSize()

          if (nDelayedParticles > 0) then
            !$omp parallel do schedule(dynamic)
            genDelayed: do n = 1, nDelayedParticles
              call self % precursorDungeons(i) % copy(p, n)
              if ((p % time <= t*timeIncrement) .and. (p % time > (t-1)*timeIncrement)) then
                p % type = P_NEUTRON
                pRNG = self % pRNG
                p % pRNG => pRNG
                call p % pRNG % stride(n)
                bufferLoopDelayed: do

                  if ((p % fate == aged_FATE) .or. (p % fate == no_FATE)) then
                    p % fate = no_FATE
                    p % isdead = .false.
                  else
                    p % isdead = .true.
                  end if

                  call self % geom % placeCoord(p % coords)
                  p % timeMax = t * timeIncrement
                  call p % savePreHistory()

                  ! Transport particle untill its death
                  historyDelayed: do
                    if(p % isDead) exit historyDelayed
                    call transOp % transport(p, tally, buffer, buffer)
                    if(p % isDead) exit historyDelayed
                    if(p % fate == AGED_FATE) then
                      call self % nextTime(i) % detain(p)
                      exit historyDelayed
                    endif
                    call collOp % collide(p, tally, self % precursorDungeons(i), buffer)
                    if(p % isDead) exit historyDelayed
                  end do historyDelayed

                  ! Clear out buffer
                  if (buffer % isEmpty()) then
                    exit bufferLoopDelayed
                  else
                    call buffer % release(p)
                  end if

                end do bufferLoopDelayed
              end if
            end do genDelayed
            !$omp end parallel do

          end if
        end if

        call tally % reportCycleEnd(self % currentTime(i))
        call self % pRNG % stride(nParticles + 1)
        call self % currentTime(i) % cleanPop()
      end do

      self % tempTime  => self % nextTime
      self % nextTime  => self % currentTime
      self % currentTime => self % tempTime

      ! Calculate times
      call timerStop(self % timerMain)
      elapsed_T = timerTime(self % timerMain)

      ! Predict time to end
      end_T = real(N_timeBins,defReal) * elapsed_T / t
      T_toEnd = max(ZERO, end_T - elapsed_T)

      ! Display progress
      call printFishLineR(t)
      print *
      print *, 'Time step: ', numToChar(t), ' of ', numToChar(N_timeBins)
      print *, 'Elapsed time: ', trim(secToChar(elapsed_T))
      print *, 'End time:     ', trim(secToChar(end_T))
      print *, 'Time to end:  ', trim(secToChar(T_toEnd))
      call tally % display()

      call tally % setNumBatchesPerTimeStep(N_cycles)
    end do

  end subroutine cycles

  !!
  !! Print calculation results to file
  !!
  subroutine collectResults(self)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    type(outputFile)                                  :: out
    character(nameLen)                                :: name

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
    integer(shortInt)                               :: seed_temp, i
    integer(longInt)                                :: seed
    character(10)                                   :: time
    character(8)                                    :: date
    character(:),allocatable                        :: string
    character(nameLen)                              :: nucData, energy, geomName
    type(outputFile)                                :: test_out
    character(100), parameter :: Here ='init (timeDependentPhysicsPackage_class.f90)'

    call cpu_time(self % CPU_time_start)

    ! Read calculation settings
    call dict % get( self % pop,'pop')
    call dict % get( self % N_cycles,'cycles')
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

    ! Whether to use analog or implicit kinetic (default = Analog)
    call dict % getOrDefault(self % useImplicit, 'useImplicit', .false.)

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
    allocate(self % currentTime(self % N_cycles))
    allocate(self % nextTime(self % N_cycles))

    do i = 1, self % N_cycles
      call self % currentTime(i) % init(70*self % pop)
      call self % nextTime(i) % init(70*self % pop)
    end do

    ! Size precursor dungeon
    if (self % usePrecursors) then
      allocate(self % precursorDungeons(self % N_cycles))
      do i = 1, self % N_cycles
        call self % precursorDungeons(i) % init(70 * self % pop)
      end do
    end if

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
    print *, "Initial Population per batch:       ", numToChar(self % pop)
    print *, "Initial RNG Seed:                   ", numToChar(self % pRNG % getSeed())
    print *
    print *, repeat("<>",50)
  end subroutine printSettings

  subroutine russianRoulette(self, p, avWgt)
    class(timeDependentPhysicsPackage), intent(inout) :: self
    class(particle), intent(inout)     :: p
    real(defReal), intent(in)          :: avWgt

    if (p % pRNG % get() < (ONE - p % w/avWgt)) then
      p % isDead = .true.
    else
      p % w = avWgt
    end if

  end subroutine russianRoulette

end module timeDependentPhysicsPackage_class
