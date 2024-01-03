module scoreMemory_class

  use numPrecision
  use universalVariables, only : array_pad
  use genericProcedures,  only : fatalError, numToChar
  use openmp_func,        only : ompGetMaxThreads, ompGetThreadNum
  use RNG_class,                      only : RNG

  implicit none
  private

  !! Parameters for indexes of per cycle SCORE, Cumulative Sum and Cumulative Sum of squares
  integer(shortInt), parameter :: CSUM  = 1, &
                                  CSUM2 = 2

  !! Size of the 2nd Dimension of bins
  integer(shortInt), parameter :: DIM2 = 2


  !!
  !! scoreMemory is a class that stores space for scores from tallies.
  !! It is separate from tallyClerks and individual responses to allow:
  !!   -> Easy writing and (later) reading from file for archivisation of results
  !!   -> Easy possibility of extention to tally higher moments of result
  !!   -> Possibility of extension to tally covariance of selected tally bins
  !!   -> Easy copying and recombination of results for OpenMP shared memory parallelism
  !!   -> Easy, output format-independent way to perform regression tests
  !!   -> Easy handling of different batch sizes
  !!
  !! For every bin index there are two positions: CSUM, CSUM2. All are initialised to 0.
  !! For scoring, an array is created with dimension (Nbins,nThreads) to mitigate false sharing.
  !! On accumulation, this array adds to the normal bin array.
  !!
  !! Interface:
  !!     init(N,idBS): Initialise with integer size N and integer id. Optional integer Batch Size.
  !!
  !!     kill(): Elemental. Return to uninitialised state.
  !!
  !!     score(score,idx): Score in the bin under idx. FatalError if idx is outside bounds. Score
  !!         is defReal, shortInt or longInt
  !!
  !!     accumulate(score,idx): Accumulate result in cumulative sums in bin under idx. FatalError
  !!         if idx is outside bounds. Score is defReal, shortInt or longInt.
  !!
  !!     getResult(mean, STD, idx, samples): Retrieve mean value and standard deviation of the
  !!         estimate under idx. Use optional samples to specify number of estimates used to
  !!         evaluate mean and STD from default, which is number of batches in score memory.
  !!         STD is optional.
  !!
  !!     getScore(idx): Return current value of score from bin under idx. FatalError if idx is
  !!         outside bounds.
  !!
  !!     closeBin(normFactor,idx): Multiplies score under bin by normFactor and accumulates it in
  !!         cumulative sums. Then sets the bin to zero.
  !!
  !!     closeCycle(normFactor): Multiplies all scores by normFactor and accumulates them in
  !!         cumulative sums. Sets all scors to zero.
  !!
  !!     lastCycle(): Return true if the next call to closeCycle will close a batch.
  !!
  !!     getBatchSize(): Returns number of cycles that constitute a single batch.
  !!
  !! Example use case:
  !!
  !!  do batches=1,20
  !!    do hist=1,10
  !!      call scoreMem % score(hist,1)        ! Score hist (1,10) in bin 1
  !!      call scoreMem % accumulate(hist,2)   ! Accumulate hist in CSUMs of bin 2
  !!    end do
  !!    call scoreMem % closeCycle(ONE)        ! Close batch without normalisation (factor = ONE)
  !!  end do
  !!
  !!  call scoreMem % getResult(mean,STD,1) ! Get result from bin 1 with STD
  !!  call scoreMem % getResult(mean,2,200) ! Get mean from bin 2 assuming 200 samples
  !!
  !! NOTE:  Following indexing is used in bins class member
  !!        bins(binIndex,binType) binType is CSUM/CSUM2
  !! NOTE2: If batch size is not a denominator of cycles scored results accumulated
  !!        in extra cycles are discarded in current implementation
  !!
  type, public :: scoreMemory
      !private
      real(defReal),dimension(:,:),allocatable     :: bins              !! Space for storing cumul data (2nd dim size is always 2!)
      real(defReal),dimension(:,:),allocatable     :: parallelBins      !! Space for scoring for different threads
      integer(longInt)                             :: N = 0             !! Size of memory (number of bins)
      integer(shortInt)                            :: nThreads = 0      !! Number of threads used for parallelBins
      integer(shortInt)                            :: id                !! Id of the tally
      integer(shortInt)                            :: batchN = 0        !! Number of Batches
      integer(shortInt)                            :: cycles = 0        !! Cycles counter
      integer(shortInt)                            :: batchSize = 1     !! Batch interval size (in cycles)

      real(defReal), dimension(:,:), allocatable   :: bootstrapBins
      real(defReal), dimension(:), allocatable     :: plugInMean
      real(defReal), dimension(:), allocatable     :: plugInVar
      integer(shortInt)                            :: Nbootstraps = 0

      real(defReal), dimension(:,:), allocatable    :: plugInSamples, plugInSamplesMean, plugInSamplesVar
      real(defReal), dimension(:), allocatable     :: bootstrapAccumulator, unbiasedMeans, unbiasedVars
      real(defReal), dimension(:), allocatable     ::  biasedMeans, biasedVars, normBias
      integer(shortInt), dimension(:), allocatable :: scoreBinTracker
      integer(shortInt)                            :: nTimeBins
      integer(shortInt)                             :: Ntallies

      logical(defBool)                             :: bootstrapScore = .false.
      real(defReal)                                :: simTime = ONE


  contains
    ! Interface procedures
    procedure :: init
    procedure :: kill
    generic   :: score      => score_defReal, score_shortInt, score_longInt
    generic   :: accumulate => accumulate_defReal, accumulate_shortInt, accumulate_longInt
    generic   :: getResult  => getResult_withSTD, getResult_withoutSTD
    procedure :: getScore
    procedure :: closeCycle
    procedure :: closeBin
    procedure :: lastCycle
    procedure :: getBatchSize
    procedure :: closeBootstrap
    procedure :: getNbootstraps
    procedure :: initScoreBootstrap
    procedure :: closePlugInCycle
    procedure :: closePlugInCycleModified
    procedure :: bootstrapPlugIn
    procedure :: setBootstrapScore
    procedure :: getBootstrapScore
    procedure :: setSimTime
    procedure :: getSimTime

    procedure :: getBiasedMean
    procedure :: getBiasedVar
    procedure :: getNormBias
    ! Private procedures
    procedure, private :: score_defReal
    procedure, private :: score_shortInt
    procedure, private :: score_longInt
    procedure, private :: accumulate_defReal
    procedure, private :: accumulate_shortInt
    procedure, private :: accumulate_longInt
    procedure, private :: getResult_withSTD
    procedure, private :: getResult_withoutSTD

  end type scoreMemory

contains

  !!
  !! Allocate space for the bins given number of bins N
  !! Optionaly change batchSize from 1 to any +ve number
  !!
  subroutine init(self, N, id, batchSize, bootstrap, timeSteps, modified)
    class(scoreMemory),intent(inout)      :: self
    integer(longInt),intent(in)           :: N
    integer(shortInt),intent(in)          :: id
    integer(shortInt),optional,intent(in) :: batchSize, bootstrap, timeSteps, modified
    character(100), parameter :: Here= 'init (scoreMemory_class.f90)'

    self % nThreads = ompGetMaxThreads()

    ! Note the array padding to avoid false sharing
    allocate( self % parallelBins(N + array_pad, self % nThreads))
    self % parallelBins = ZERO

    ! Save size of memory
    self % N = N

    ! Assign memory id
    self % id = id

    if (present(bootstrap) .and. present(timeSteps) .and. (.not. present(modified))) then !bootstrap score
      self % Nbootstraps = bootstrap
      self % nTimeBins = timeSteps

      allocate(self % plugInMean(N))
      self % plugInMean = ZERO

      allocate(self % plugInVar(N))
      self % plugInVar = ZERO

      allocate(self % unbiasedMeans(N))
      self % unbiasedMeans = ZERO

      allocate(self % unbiasedVars(N))
      self % unbiasedVars = ZERO

      self % Ntallies = N / self % nTimeBins

      allocate(self % plugInSamplesMean(self % Ntallies, self % nThreads))
      self % plugInSamplesMean = ZERO

      allocate(self % plugInSamplesVar(self % Ntallies, self % nThreads))
      self % plugInSamplesVar = ZERO

      allocate(self % bootstrapAccumulator(self % nThreads))
      self % bootstrapAccumulator = ZERO

      allocate(self % biasedMeans(N))
      self % biasedMeans = ZERO

      allocate(self % biasedVars(N))
      self % biasedVars = ZERO

      allocate(self % normBias(N))
      self % normBias = ZERO

      allocate(self % scoreBinTracker(self % nThreads))
      self % scoreBinTracker = ZERO

    else if (present(bootstrap) .and. present(timeSteps) .and. present(modified)) then !bootstrap particle

      self % Nbootstraps = bootstrap
      self % nTimeBins = timeSteps

      allocate(self % plugInMean(N))
      self % plugInMean = ZERO

      allocate(self % plugInVar(N))
      self % plugInVar = ZERO

      allocate(self % unbiasedMeans(N))
      self % unbiasedMeans = ZERO

      allocate(self % unbiasedVars(N))
      self % unbiasedVars = ZERO

      self % Ntallies = N / self % nTimeBins

      allocate(self % plugInSamplesMean(self % Ntallies,1))
      self % plugInSamplesMean = ZERO

      allocate(self % plugInSamplesVar(self % Ntallies,1))
      self % plugInSamplesVar = ZERO

      allocate(self % bootstrapAccumulator(self % nThreads))
      self % bootstrapAccumulator = ZERO

      allocate(self % biasedMeans(N))
      self % biasedMeans = ZERO

      allocate(self % biasedVars(N))
      self % biasedVars = ZERO

      allocate(self % normBias(N))
      self % normBias = ZERO

    else
      ! Allocate space and zero all bins
      allocate(self % bins(N, DIM2))
      self % bins = ZERO

      ! Set batchN, cycles and batchSize to default values
      self % batchN    = 0
      self % cycles    = 0
      self % batchSize = 1

      if(present(batchSize)) then
        if(batchSize > 0) then
          self % batchSize = batchSize
        else
          call fatalError(Here,'Batch Size of: '// numToChar(batchSize) //' is invalid')
        end if
      end if

    end if

  end subroutine init

  !!
  !! Deallocate memory and return to uninitialised state
  !!
  subroutine kill(self)
   class(scoreMemory), intent(inout) :: self

   if(allocated(self % bins)) deallocate(self % bins)
   if(allocated(self % parallelBins)) deallocate(self % parallelBins)
   self % N = 0
   self % nThreads = 0
   self % batchN = 0

  end subroutine kill

  !!
  !! Score a result on a given single bin under idx
  !!
  subroutine score_defReal(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    real(defReal), intent(in)         :: score
    integer(longInt), intent(in)      :: idx 
    integer(shortInt),save                 :: thread_idx
    real(defReal), save                     :: ratio
    character(100),parameter :: Here = 'score_defReal (scoreMemory_class.f90)'
    !$omp threadprivate(ratio)

    ! Verify bounds for the index
    if( idx < 0_longInt .or. idx > self % N) then
      call fatalError(Here,'Index '//numToChar(idx)//' is outside bounds of &
                            & memory with size '//numToChar(self % N))
    end if

    thread_idx = ompGetThreadNum() + 1

    ! Add the score
    self % parallelBins(idx, thread_idx) = &
            self % parallelBins(idx, thread_idx) + score
    
    if (self % bootstrapScore) then !think it is time0, time0, time0, time1, time1, time1 ...
      ratio = real(idx) / self % nTimeBins
      !print *, 'setting here', ratio, thread_idx, int(ceiling(ratio))
      self % scoreBinTracker(thread_idx) = int(ceiling(ratio)) !either mod or int((idx / self % nTimeBins) + 0.5). depends on bin distribution. 
    end if

  end subroutine score_defReal

  !!
  !! Score a result with shortInt on a given bin under idx
  !!
  subroutine score_shortInt(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    integer(shortInt), intent(in)     :: score
    integer(longInt), intent(in)      :: idx

    call self % score_defReal(real(score, defReal), idx)

  end subroutine score_shortInt

  !!
  !! Score a result with longInt on a given bin under idx
  !!
  subroutine score_longInt(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    integer(longInt), intent(in)      :: score
    integer(longInt), intent(in)     :: idx

    call self % score_defReal(real(score, defReal), idx)

  end subroutine score_longInt

  !!
  !! Increment the result directly on cumulative sums
  !!
  subroutine accumulate_defReal(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    real(defReal), intent(in)         :: score
    integer(longInt), intent(in)      :: idx
    character(100),parameter :: Here = 'accumulate_defReal (scoreMemory_class.f90)'

    ! Verify bounds for the index
    if( idx < 0_longInt .or. idx > self % N) then
      call fatalError(Here,'Index '//numToChar(idx)//' is outside bounds of &
                            & memory with size '//numToChar(self % N))
    end if

    ! Add the score
    self % bins(idx, CSUM)  = self % bins(idx, CSUM)  + score
    self % bins(idx, CSUM2) = self % bins(idx, CSUM2) + score * score

  end subroutine accumulate_defReal

  !!
  !! Increment the result directly on cumulative sums with shortInt score
  !!
  subroutine accumulate_shortInt(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    integer(shortInt), intent(in)     :: score
    integer(longInt), intent(in)     :: idx

    call self % accumulate_defReal(real(score, defReal), idx)

  end subroutine accumulate_shortInt

  !!
  !! Increment the result directly on cumulative sums with longInt score
  !!
  subroutine accumulate_longInt(self, score, idx)
    class(scoreMemory), intent(inout) :: self
    integer(longInt), intent(in)      :: score
    integer(longInt), intent(in)      :: idx

    call self % accumulate_defReal(real(score, defReal), idx)

  end subroutine accumulate_longInt

  !!
  !! Close Cycle
  !! Increments cycle counter and detects end-of-batch
  !! When batch finishes it normalises all scores by the factor and moves them to CSUMs
  !!
  subroutine closeCycle(self, normFactor)
    class(scoreMemory), intent(inout)       :: self
    real(defReal),intent(in)                :: normFactor
    integer(longInt)                        :: i
    real(defReal), save                     :: res
    !$omp threadprivate(res)

    ! Increment Cycle Counter
    self % cycles = self % cycles + 1

    if(mod(self % cycles, self % batchSize) == 0) then ! Close Batch

      !$omp parallel do
      do i = 1, self % N

        ! Normalise scores
        self % parallelBins(i,:) = self % parallelBins(i,:) * normFactor
        res = sum(self % parallelBins(i,:))

        ! Zero all score bins
        self % parallelBins(i,:) = ZERO

        ! Increment cumulative sums
        self % bins(i,CSUM)  = self % bins(i,CSUM) + res
        self % bins(i,CSUM2) = self % bins(i,CSUM2) + res * res

      end do
      !$omp end parallel do

      ! Increment batch counter
      self % batchN = self % batchN + 1
      self % batchSize = self % batchSize + 1

    end if

  end subroutine closeCycle


subroutine closeBootstrap(self, binIdx, bootstrapIdx)
  class(scoreMemory), intent(inout)       :: self
  integer(shortInt), intent(in)          :: binIdx, bootstrapIdx
  integer(shortInt)                     :: Nsamples
  real(defReal)                          :: bootstrapRes, inv_Nm1, inv_N

  Nsamples = self % batchN
  bootstrapRes = self % bins(binIdx,CSUM) / Nsamples

  if (bootstrapIdx == 1) then
    self % plugInMean(binIdx) = bootstrapRes

    inv_N = ONE / Nsamples
    if( Nsamples /= 1) then
      inv_Nm1 = ONE / (Nsamples - 1)
    else
      inv_Nm1 = ONE
    end if
    self % plugInVar(binIdx) = self % bins(binIdx,CSUM2) * inv_N * inv_Nm1 - bootstrapRes * bootstrapRes * inv_Nm1
  end if

  self % bins(binIdx,CSUM) = ZERO
  self % batchN = 0

  self % bootstrapBins(binIdx,CSUM) = self % bootstrapBins(binIdx,CSUM) + bootstrapRes
  self % bootstrapBins(binIdx,CSUM2) = self % bootstrapBins(binIdx,CSUM2) + bootstrapRes * bootstrapRes

end subroutine closeBootstrap

  !!
  !! Close Cycle
  !! Multiplies score in bin under idx by normFactor, accumulates it and sets it to zero
  !!
  subroutine closeBin(self, normFactor, idx)
    class(scoreMemory), intent(inout) :: self
    real(defReal),intent(in)          :: normFactor
    integer(longInt), intent(in)      :: idx
    real(defReal)                     :: res
    character(100),parameter :: Here = 'closeBin (scoreMemory_class.f90)'

    ! Verify bounds for the index
    if( idx < 0_longInt .or. idx > self % N) then
      call fatalError(Here,'Index '//numToChar(idx)//' is outside bounds of &
                            & memory with size '//numToChar(self % N))
    end if

    ! Normalise score
    self % parallelBins(idx, :) = self % parallelBins(idx, :) * normFactor

    ! Increment cumulative sum
    res = sum(self % parallelBins(idx,:))
    self % bins(idx,CSUM)  = self % bins(idx,CSUM) + res
    self % bins(idx,CSUM2) = self % bins(idx,CSUM2) + res * res

    ! Zero the score
    self % parallelBins(idx,:) = ZERO

  end subroutine closeBin


  !!
  !! Return true if next closeCycle will close a batch
  !!
  function lastCycle(self) result(isIt)
    class(scoreMemory), intent(in) :: self
    logical(defBool)               :: isIt

    isIt =  mod(self % cycles + 1, self % batchSize) == 0

  end function lastCycle

  !!
  !! Return batchSize
  !!
  pure function getBatchSize(self) result(S)
    class(scoreMemory), intent(in) :: self
    integer(shortInt)              :: S

    S = self % batchSize

  end function getBatchSize

  !!
  !! Load mean result and Standard deviation into provided arguments
  !! Load from bin indicated by idx
  !! Returns 0 if index is invalid
  !!
  elemental subroutine getResult_withSTD(self, mean, STD, idx, samples)
    class(scoreMemory), intent(in)         :: self
    real(defReal), intent(out)             :: mean
    real(defReal),intent(out)              :: STD
    integer(longInt), intent(in)           :: idx
    integer(shortInt), intent(in),optional :: samples
    integer(shortInt)                      :: N, i
    real(defReal)                          :: inv_N, inv_Nm1

    !bootstrap
    if (self % Nbootstraps > 0) then
      if (self % bootstrapScore) then
        mean = self % unbiasedMeans(idx)
        STD = sqrt(self % unbiasedVars(idx))
      else
        mean = self % unbiasedMeans(idx)
        STD = sqrt(self % unbiasedVars(idx))
      end if
    else

      !! Verify index. Return 0 if not present
      if( idx < 0_longInt .or. idx > self % N) then
        mean = ZERO
        STD = ZERO
        return
      end if

      ! Check if # of samples is provided
      if( present(samples)) then
        N = samples
      else
        N = self % batchN
      end if

      ! Calculate mean
      mean = (self % bins(idx, CSUM) / N)
      !
      !! Calculate STD
      inv_N   = ONE / N
      if( N /= 1) then
        inv_Nm1 = ONE / (N - 1)
      else
        inv_Nm1 = ONE
      end if
      STD = self % bins(idx, CSUM2) * inv_N * inv_Nm1 - mean * mean * inv_Nm1
      STD = sqrt(STD)
    end if
  end subroutine getResult_withSTD

  !!
  !! Load mean result provided argument
  !! Load from bin indicated by idx
  !! Returns 0 if index is invalid
  !!
  elemental subroutine getResult_withoutSTD(self, mean, idx, samples)
    class(scoreMemory), intent(in)         :: self
    real(defReal), intent(out)             :: mean
    integer(longInt), intent(in)           :: idx
    integer(shortInt), intent(in),optional :: samples
    integer(shortInt)                      :: N

    !! Verify index. Return 0 if not present
    if( idx < 0_longInt .or. idx > self % N) then
      mean = ZERO
      return
    end if

    ! Check if # of samples is provided
    if( present(samples)) then
      N = samples
    else
      N = self % batchN
    end if

    ! Calculate mean
    mean = self % bins(idx, CSUM) / N

  end subroutine getResult_withoutSTD

  !!
  !! Obtain value of a score in a bin
  !! Return ZERO for invalid bin address (idx)
  !!
  elemental function getScore(self, idx) result (score)
    class(scoreMemory), intent(in) :: self
    integer(longInt), intent(in)   :: idx
    real(defReal)                  :: score

    if(idx <= 0_longInt .or. idx > self % N) then
      score = ZERO
    else
      score = sum(self % parallelBins(idx, :))
    end if

  end function getScore

  function getNbootstraps(self) result(nBootstraps)
    class(scoreMemory),intent(in) :: self
    integer(shortInt)               :: nBootstraps

    nBootstraps = self % Nbootstraps
  end function getNbootstraps


  subroutine initScoreBootstrap(self, nParticles)
    class(scoreMemory),intent(inout) :: self
    integer(shortInt), intent(in) :: nParticles

    if (allocated(self % plugInSamples)) deallocate(self % plugInSamples)
    allocate(self % plugInSamples(nParticles, self % Ntallies))
    self % plugInSamples = ZERO
    self % plugInSamplesMean = ZERO
    self % plugInSamplesVar = ZERO

  end subroutine initScoreBootstrap

  subroutine closePlugInCycle(self, n, binIdx)
    class(scoreMemory), intent(inout)       :: self
    integer(shortInt), intent(in)  :: n, binIdx
    real(defReal), save                     :: res
    integer(shortInt), save :: scoreBinIdx
    integer(longInt), save :: scoreLoc
    integer(shortInt) :: thread_idx
    !$omp threadprivate(res, scoreBinIdx, scoreLoc)

    thread_idx = ompGetThreadNum() + 1

    scoreBinIdx = self % scoreBinTracker(thread_idx)

    if (scoreBinIdx == 0) then
      return

    else
      ! Retrieve score
      scoreLoc = (scoreBinIdx-1)*self % nTimeBins + binIdx
      !print *, binIdx, scoreLoc, scoreBinIdx, thread_idx
      res = self % parallelBins(scoreLoc,thread_idx)
      ! Zero thread bin
      self % parallelBins(scoreLoc,thread_idx) = ZERO

      !print *, 'HERE', size(self % plugInSamples), n, scoreBinIdx
      self % plugInSamples(n, scoreBinIdx) = res
      self % plugInSamplesMean(scoreBinIdx, thread_idx) = self % plugInSamplesMean(scoreBinIdx, thread_idx) + res
      self % plugInSamplesVar(scoreBinIdx, thread_idx)  = self % plugInSamplesVar(scoreBinIdx, thread_idx) + res * res
    end if

  end subroutine closePlugInCycle
  
    subroutine closePlugInCycleModified(self, k, binIdx)
    class(scoreMemory), intent(inout)       :: self
    integer(shortInt), intent(in)  :: k, binIdx
    integer(longInt)                        :: i
    real(defReal), save                     :: res
    integer(longInt), save :: scoreLoc
    !$omp threadprivate(res, scoreLoc)

    !$omp parallel do
    do i = 1, self % Ntallies
      scoreLoc = (i-1)*self % nTimeBins + binIdx
      res = sum(self % parallelBins(scoreLoc,:))


      ! Zero all score bins
      self % parallelBins(scoreLoc,:) = ZERO


      self % plugInSamples(k, i) = res
      ! Increment cumulative sums
      self % plugInSamplesMean(i,1)  = self % plugInSamplesMean(i,1) + res
      self % plugInSamplesVar(i,1) = self % plugInSamplesVar(i,1) + res * res

    end do
    !$omp end parallel do

  end subroutine closePlugInCycleModified

  subroutine bootstrapPlugIn(self, nBootstraps, pRNG, N_timeBins, binIdx)
    class(scoreMemory), intent(inout)       :: self
    integer(shortInt), intent(in) :: nBootstraps, N_timeBins, binIdx
    type(RNG), intent(inout)    :: pRNG
    integer(shortInt)         :: x, i, j, N
    integer(shortInt), save        :: idx, threadIdx
    real(defReal)             :: biasedMean, biasedVar, biasAdjustedMean, plugInMean, plugInVar, bootstrapMean, VAR, inv_Nm1, inv_N
    real(defReal)             :: res
    real(defReal), save             :: score
    type(RNG), target, save     :: pRNG1
    integer(longInt)            :: scoreLoc
    !$omp threadprivate(pRNG1)

    !$omp parallel
    pRNG1 = pRNG
    !$omp end parallel
    do x = 1, self % Ntallies

      N = size(self % plugInSamples(:,x))

      res = sum(self % plugInSamplesMean(x,:))
      plugInMean = res / N

      inv_N   = ONE / N
      if( N /= 1) then
        inv_Nm1 = ONE / (N - 1)
      else
        inv_Nm1 = ONE
      end if

      plugInVar = sum(self % plugInSamplesVar(x,:)) * inv_N * inv_Nm1 - plugInMean * plugInMean * inv_Nm1

      biasedMean = plugInMean
      biasedVar = plugInMean * plugInMean

      do i = 1, nBootstraps - 1

        self % bootstrapAccumulator = ZERO
        !$omp parallel do private(score, idx, threadIdx)
        do j = 1, N
          call pRNG1 % stride(j)
          validSample: do
            idx = int(N * pRNG1 % get()) + 1
            threadIdx = ompGetThreadNum() + 1
            if (idx <= N) then
              score = self % plugInSamples(idx,x)
              self % bootstrapAccumulator(threadIdx) = self % bootstrapAccumulator(threadIdx) + score
              exit validSample
            else
              cycle validSample
            end if
          end do validSample

        end do
        !$omp end parallel do

        bootstrapMean = sum(self % bootstrapAccumulator)
        biasedMean = biasedMean + bootstrapMean / N

        biasedVar = biasedVar + (bootstrapMean / N) * (bootstrapMean / N)
      end do


      biasedMean = biasedMean / nBootstraps
      biasAdjustedMean = TWO * plugInMean - biasedMean
      scoreLoc = (x-1)*self % nTimeBins + binIdx
      self % unbiasedMeans(scoreLoc) = biasAdjustedMean


      N = nBootstraps
      if (N /= 1) then
        inv_Nm1 = ONE / (N - 1)
      else
        inv_Nm1 = ONE
      end if
      VAR = biasedVar * inv_Nm1 - biasedMean * biasedMean * inv_Nm1 * N
      self % unbiasedVars(scoreLoc) = TWO * TWO * plugInVar + VAR !-4cov(pluginmean, bootstrapmean)

      self % biasedMeans(scoreLoc) = biasedMean
      self % biasedVars(scoreLoc) = VAR
      self % normBias(scoreLoc) = (biasedMean - plugInMean) / (sqrt(VAR))
      !print *, plugInVar, VAR
      !print *, 'bias', biasedMean - plugInMean
      !print *, 'bias / std error', (biasedMean - plugInMean) / (sqrt(VAR))
    
    end do

  end subroutine bootstrapPlugIn


  subroutine setBootstrapScore(self)
    class(scoreMemory), intent(inout)       :: self

    self % bootstrapScore = .true.
  end subroutine setBootstrapScore

  function getBootstrapScore(self) result(bootstrapScore)
    class(scoreMemory), intent(in)       :: self
    logical(defBool) :: bootstrapScore

    bootstrapScore = self % bootstrapScore
  end function getBootstrapScore

  subroutine setSimTime(self, simTime)
    class(scoreMemory), intent(inout)       :: self
    real(defReal), intent(in)   :: simTime

    self % simTime = simTime
  end subroutine setSimTime

  function getSimTime(self) result(simTime)
    class(scoreMemory), intent(in)       :: self
    real(defReal)   :: simTime

    simTime = self % simTime
  end function getSimTime

  function getBiasedMean(self, idx) result(biasedMean)
    class(scoreMemory), intent(in)       :: self
    integer(shortInt), intent(in) :: idx
    real(defReal)   :: biasedMean

    biasedMean = self % biasedMeans(idx)
  end function getBiasedMean

  function getBiasedVar(self, idx) result(biasedVar)
    class(scoreMemory), intent(in)       :: self
    integer(shortInt), intent(in) :: idx
    real(defReal)   :: biasedVar

    biasedVar = self % biasedVars(idx)
  end function getBiasedVar

  function getNormBias(self, idx) result(normBias)
    class(scoreMemory), intent(in)       :: self
    integer(shortInt), intent(in) :: idx
    real(defReal)   :: normBias

    normBias = self % normBias(idx)
  end function getNormBias

end module scoreMemory_class
