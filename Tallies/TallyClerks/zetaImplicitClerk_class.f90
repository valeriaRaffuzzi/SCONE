module zetaImplicitClerk_class

  use numPrecision
  use tallyCodes
  use endfConstants
  use universalVariables
  use genericProcedures,          only : fatalError, charCmp
  use dictionary_class,           only : dictionary
  use particle_class,             only : particle
  use particleDungeon_class,      only : particleDungeon
  use outputFile_class,           only : outputFile

  ! Nuclear Data Interfaces
  use nuclearDataReg_mod,         only : ndReg_get => get
  use nuclearDatabase_inter,      only : nuclearDatabase
  use neutronMaterial_inter,      only : neutronMaterial, neutronMaterial_CptrCast
  use neutronXSPackages_class,    only : neutronMacroXSs
  use ceNeutronCache_mod,         only : zetaCache

  ! Tally Interfaces
  use scoreMemory_class,          only : scoreMemory
  use tallyResult_class,          only : tallyResult, tallyResultEmpty
  use tallyClerk_inter,           only : tallyClerk, kill_super => kill
  use keffAnalogClerk_class,      only : keffResult

  implicit none
  private


  !! Locations of diffrent bins wrt memory Address of the clerk
  integer(shortInt), parameter :: MEM_SIZE = 8
  integer(longInt), parameter  :: IMP_PROD     = 0 ,&  ! Implicit neutron production (from fission)
                                  SCATT_PROD   = 1 ,&  ! Analog Stattering production (N,XN)
                                  IMP_ABS      = 2 ,&  ! Implicit neutron absorbtion
                                  IMP_PROD_z   = 3 ,&  ! Implicit neutron production (from fission)
                                  SCATT_PROD_z = 4 ,&  ! Analog Stattering production (N,XN)
                                  IMP_ABS_z    = 5 ,&  ! Implicit neutron absorbtion
                                  ANA_LEAK     = 6 ,&  ! Analog Leakage
                                  ZETA_IMP     = 7     ! k-eff estimate
  !!
  !! A simple implicit zeta estimator based on collison estimator of reaction rates,
  !! and on analog estimators of (N,XN) reactions and leakage
  !!
  !! Interface:
  !!   tallyClerk interface
  !!
  !! SAMPLE DICTIOANRY INPUT:
  !!
  !! myClerk {
  !!   type zetaImplicitClerk;
  !! }
  !!
  type, public,extends(tallyClerk) :: zetaImplicitClerk
    private
    ! Settings
    integer(shortInt) :: flush
    integer(shortInt) :: cycles = 0
    integer(shortInt) :: cycleCounter = 1
    logical(defBool)  :: handleVirtual = .true.
  contains
    ! Duplicate interface of the tallyClerk
    ! Procedures used during build
    procedure :: init
    procedure :: kill
    procedure :: validReports
    procedure :: getSize

    ! File reports and check status -> run-time procedures
    procedure :: reportInColl
    procedure :: reportOutColl
    procedure :: reportHist
    procedure :: reportCycleEnd

    ! Output procedures

    procedure :: display
    procedure :: print
    procedure :: getResult

  end type zetaImplicitClerk

contains

  !!
  !! Initialise from dictionary and name
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine init(self, dict, name)
    class(zetaImplicitClerk), intent(inout) :: self
    class(dictionary), intent(in)           :: dict
    character(nameLen), intent(in)          :: name

    ! Set name
    call self % setName(name)

    ! Handle virtual collisions
    call dict % getOrDefault(self % handleVirtual,'handleVirtual', .true.)

    ! History
    call dict % getOrDefault(self % cycles, 'cycles', 0)

    ! Flux history every number of batches
    call dict % getOrDefault(self % flush, 'flush', huge(1_shortInt))

  end subroutine init

  !!
  !! Return to uninitialised State
  !!
  elemental subroutine kill(self)
    class(zetaImplicitClerk), intent(inout) :: self

    ! Call Superclass
    call kill_super(self)

    ! Kill self
    self % handleVirtual = .true.

  end subroutine kill

  !!
  !! Returns array of codes that represent diffrent reports
  !!
  !! See tallyClerk_inter for details
  !!
  function validReports(self) result(validCodes)
    class(zetaImplicitClerk),intent(in)           :: self
    integer(shortInt),dimension(:),allocatable :: validCodes

    validCodes = [inColl_CODE, outColl_CODE, cycleEnd_CODE, hist_CODE]

  end function validReports

  !!
  !! Return memory size of the clerk
  !!
  !! See tallyClerk_inter for details
  !!
  elemental function getSize(self) result(S)
    class(zetaImplicitClerk), intent(in) :: self
    integer(shortInt)                    :: S

    S = MEM_SIZE + self % cycles

  end function getSize

  !!
  !! Process incoming collision report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportInColl(self, p, xsData, mem, virtual)
    class(zetaImplicitClerk), intent(inout)  :: self
    class(particle), intent(in)              :: p
    class(nuclearDatabase),intent(inout)     :: xsData
    type(scoreMemory), intent(inout)         :: mem
    logical(defBool), intent(in)             :: virtual
    type(neutronMacroXSs)                    :: xssMat, xssZeta, xss
    class(neutronMaterial), pointer          :: mat, matZeta
    integer(shortInt)                        :: matIdx, i
    real(defReal)                            :: nuFissXS_mat, absXS_mat, nuFissXS, absXS, flux
    real(defReal)                            :: s1, s2, s3, s4
    character(100), parameter  :: Here = 'reportInColl (zetaImplicitClerk_class.f90)'

    ! Return if collision is virtual but virtual collision handling is off
    if ((.not. self % handleVirtual) .and. virtual) return

    ! Ensure we're not in void (could happen when scoring virtual collisions)
    if (p % matIdx() == VOID_MAT) return

    ! Calculate flux with the right cross section according to virtual collision handling
    if (self % handleVirtual) then
      flux = p % w / xsData % getTrackingXS(p, p % matIdx(), TRACKING_XS)
    else
      flux = p % w / xsData % getTotalMatXS(p, p % matIdx())
    end if

    ! Get material pointer
    mat => neutronMaterial_CptrCast(xsData % getMaterial(p % matIdx()))
    if (.not. associated(mat)) then
      call fatalError(Here,'Unrecognised type of material was retrived from nuclearDatabase')
    end if

    ! Obtain xss for the whole material
    call mat % getMacroXSs(xssMat, p)

    ! DO MG ZETA CASE
    if (p % isMG) then

      ! macro xss of the material
      nuFissXS = xssMat % nuFission
      absXS    = xssMat % capture + xssMat % fission

      s1 = nuFissXS * flux
      s2 = absXS * flux

      ! Add scores to counters
      if (mat % hasZetaMG) then
        call mem % score(s1 * zetaCache, self % getMemAddress() + IMP_PROD_z)
        call mem % score(s2 * zetaCache, self % getMemAddress() + IMP_ABS_Z)
      else
        call mem % score(s1, self % getMemAddress() + IMP_PROD)
        call mem % score(s2, self % getMemAddress() + IMP_ABS)
      end if

    else  ! DO CE ZETA CASE

      ! Zero these xss
      call xssZeta % clean()

      ! Check if the zeta nuclide are in this material
      if (mat % zetaMap % length() /= 0) then

        ! Loop over zeta nuclides
        i = mat % zetaMap % begin()
        do while (i /= mat % zetaMap % end())

          ! Get index of material to get the xss from
          matIdx = mat % zetaMap % atVal(i)

          ! Get material pointer
          matZeta => neutronMaterial_CptrCast(xsData % getMaterial(matIdx))
          if (.not. associated(matZeta)) then
            call fatalError(Here,'Unrecognised type of material was retrived from nuclearDatabase')
          end if

          ! Obtain xss and sum them up
          call matZeta % getMacroXSs(xss, p)
          call xssZeta % sum(xss, ONE)

          i = mat % zetaMap % next(i)

        end do

      end if

      ! macro xss of the zeta nuclides
      nuFissXS = xssZeta % nuFission
      absXS    = xssZeta % capture + xssZeta % fission

      ! macro xss of the remaining nuclides
      nuFissXS_mat = xssMat % nuFission - nuFissXS / zetaCache
      absXS_mat    = xssMat % capture + xssMat % fission - absXS / zetaCache

      if (nuFissXS_mat < ZERO) nuFissXS_mat = ZERO
      if (absXS_mat < ZERO) absXS_mat = ZERO

      s1 = nuFissXS * flux
      s2 = absXS * flux
      s3 = nuFissXS_mat * flux
      s4 = absXS_mat * flux

      ! Add scores to counters
      call mem % score(s1, self % getMemAddress() + IMP_PROD_z)
      call mem % score(s2, self % getMemAddress() + IMP_ABS_Z)
      call mem % score(s3, self % getMemAddress() + IMP_PROD)
      call mem % score(s4, self % getMemAddress() + IMP_ABS)

    end if

  end subroutine reportInColl

  !!
  !! Process outgoing collision report
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportOutColl(self, p, MT, muL, nucIdx, xsData, mem)
    class(zetaImplicitClerk), intent(inout) :: self
    class(particle), intent(in)             :: p
    integer(shortInt), intent(in)           :: MT
    real(defReal), intent(in)               :: muL
    integer(shortInt), intent(in)           :: nucIdx
    class(nuclearDatabase),intent(inout)    :: xsData
    type(scoreMemory), intent(inout)        :: mem
    real(defReal)                           :: score
    class(neutronMaterial), pointer         :: mat
    integer(longInt)                        :: idx
    character(100), parameter  :: Here = 'reportOutColl (zetaImplicitClerk_class.f90)'

    ! Select analog score
    ! Assumes N_XNs are by implicit weight change
    select case(MT)
      case(N_2N)
        score = 1.0_defReal * p % preCollision % wgt
      case(N_3N)
        score = 2.0_defReal * p % preCollision % wgt
      case(N_4N)
        score = 3.0_defReal * p % preCollision % wgt
      case(macroAllScatter, macroIEScatter) ! Catch weight change for MG scattering
        score = max(p % w - p % preCollision % wgt, ZERO)
      case default
        score = ZERO
    end select

    ! Add to scattering production estimator
    ! Use pre collision weight
    if (score > ZERO) then

      idx = SCATT_PROD

      ! Get material pointer
      mat => neutronMaterial_CptrCast(xsData % getMaterial(p % matIdx()))
      if (.not. associated(mat)) then
        call fatalError(Here,'Unrecognised type of material was retrived from nuclearDatabase')
      end if

      ! Check if the zeta nuclide are in this material
      if (mat % zetaMap % length() /= 0) then
        if (mat % zetaMap % getOrDefault(nucIdx, NOT_FOUND) /= NOT_FOUND) then
          idx = SCATT_PROD_z
        end if

      elseif (mat % hasZetaMG .and. p % isMG) then
        idx   = SCATT_PROD_z
        score = score * zetaCache
      end if

      call mem % score(score, self % getMemAddress() + idx)

    end if

  end subroutine reportOutColl

  !!
  !! Process history report
  !! Gets fate code from the particle
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportHist(self, p, xsData, mem)
    class(zetaImplicitClerk), intent(inout) :: self
    class(particle), intent(in)             :: p
    class(nuclearDatabase),intent(inout)    :: xsData
    type(scoreMemory), intent(inout)        :: mem
    real(defReal)                           :: histWgt

    if (p % fate == leak_FATE) then

      ! Obtain and score history weight
      histWgt = p % w

      ! Score analog leakage
      call mem % score(histWgt, self % getMemAddress() + ANA_LEAK)

    end if

  end subroutine reportHist

  !!
  !! Process end of the cycle
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine reportCycleEnd(self, end, mem)
    class(zetaImplicitClerk), intent(inout) :: self
    class(particleDungeon), intent(in)      :: end
    type(scoreMemory), intent(inout)        :: mem
    integer(longInt)                        :: addr
    real(defReal)                           :: nuFiss, absorb, scatterMul, leakage, &
                                               nuFiss_z, absorb_z, scatterMul_z, zeta
    character(100), parameter  :: Here = 'reportCycleEnd (zetaImplicitClerk_class.f90)'

    if (mem % lastCycle()) then

      addr = self % getMemAddress()

      if (mod(mem % cycles, self % flush) == 0) call mem % flushBin(addr + ZETA_IMP)

      nuFiss       = mem % getScore(addr + IMP_PROD)
      absorb       = mem % getScore(addr + IMP_ABS)
      scatterMul   = mem % getScore(addr + SCATT_PROD)
      nuFiss_z     = mem % getScore(addr + IMP_PROD_z)
      absorb_z     = mem % getScore(addr + IMP_ABS_z)
      scatterMul_z = mem % getScore(addr + SCATT_PROD_z)
      leakage      = mem % getScore(addr + ANA_LEAK)

      zeta = (nuFiss_z + scatterMul_z - absorb_z) / (leakage + absorb - nuFiss - scatterMul)
      call mem % accumulate(zeta, addr + ZETA_IMP)

      ! Ensure positivity
      if (zeta < 0) then
        print*, nuFiss_z, scatterMul_z, absorb_z
        print*, leakage, absorb, nuFiss, scatterMul
        print*, zeta
        call fatalError(Here, 'The latest zeta estimate is negative! This model cannot converge.')
      end if

      ! History
      if (self % cycleCounter <= self % cycles) then
        call mem % accumulate(zeta, addr + ZETA_IMP + self % cycleCounter)
        self % cycleCounter = self % cycleCounter + 1
      end if

    end if

  end subroutine reportCycleEnd

  !!
  !! Display convergance progress on the console
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine display(self, mem)
    class(zetaImplicitClerk), intent(in)  :: self
    type(scoreMemory), intent(in)         :: mem
    real(defReal)                         :: zeta, STD
    integer(shortInt)                     :: N

    ! Get current zeta estimate
    if (self % flush /= huge(1_shortInt)) then
      N = mod(mem % cycles, self % flush)
      if (N == 0) N = self % flush
      call mem % getResult(zeta, STD, self % getMemAddress() + ZETA_IMP, samples = N)
    else
      call mem % getResult(zeta, STD, self % getMemAddress() + ZETA_IMP)
    end if

    ! Print to console
    print '(A,F8.5,A,F8.5)', 'zeta (implicit): ', zeta, ' +/- ', STD

  end subroutine display

  !!
  !! Write contents of the clerk to output file
  !!
  !! See tallyClerk_inter for details
  !!
  subroutine print(self, outFile, mem)
    class(zetaImplicitClerk), intent(in) :: self
    class(outputFile), intent(inout)     :: outFile
    type(scoreMemory), intent(in)        :: mem
    character(nameLen)                   :: name
    real(defReal)                        :: val, STD
    integer(longInt)                     :: addr
    integer(shortInt)                    :: i, N

    call outFile % startBlock( self % getName())
    addr = self % getMemAddress()

    name = 'IMP_PROD'
    call mem % getResult(val, STD, addr + IMP_PROD)
    call outFile % printResult(val, STD, name)

    name = 'IMP_ABS'
    call mem % getResult(val, STD, addr + IMP_ABS)
    call outFile % printResult(val, STD, name)

    name = 'SCATT_PROD'
    call mem % getResult(val, STD, addr + SCATT_PROD)
    call outFile % printResult(val, STD, name)

    name = 'IMP_PROD_z'
    call mem % getResult(val, STD, addr + IMP_PROD_z)
    call outFile % printResult(val, STD, name)

    name = 'IMP_ABS_z'
    call mem % getResult(val, STD, addr + IMP_ABS_z)
    call outFile % printResult(val, STD, name)

    name = 'SCATT_PROD_z'
    call mem % getResult(val, STD, addr + SCATT_PROD_z)
    call outFile % printResult(val, STD, name)

    name = 'ANA_LEAK'
    call mem % getResult(val, STD, addr + ANA_LEAK)
    call outFile % printResult(val, STD, name)

    name = 'ZETA_IMP'
    ! Get result value
    if (self % flush /= huge(1_shortInt)) then
      N = mod(mem % cycles, self % flush)
      if (N == 0) N = self % flush
      call mem % getResult(val, STD, self % getMemAddress() + ZETA_IMP, samples = N)
    else
      call mem % getResult(val, STD, self % getMemAddress() + ZETA_IMP)
    end if
    call outFile % printResult(val, STD, name)

    if (self % cycles /= 0) then
      name = 'ZETA_HISTORY'
      call outFile % startArray(name, [self % cycles])

      do i = 1, self % cycles
        call mem % getResult(val, addr + ZETA_IMP + i, samples = 1)
        call outFile % addResult(val, ZERO)
      end do

      call outFile % endArray()
    end if

    call outFile % endBlock()

  end subroutine print

  !!
  !! Return result for interaction with Physics Package
  !!
  !! See tallyClerk_inter for details
  !!
  !! Allocates res to 'keffResult' defined in keffAnalogClerk_class
  !!
  pure subroutine getResult(self, res, mem)
    class(zetaImplicitClerk), intent(in)              :: self
    class(tallyResult), allocatable, intent(inout)    :: res
    type(scoreMemory), intent(in)                     :: mem
    real(defReal)                                     :: zeta, STD
    integer(shortInt)                                 :: N

    ! Get result value
    if (self % flush /= huge(1_shortInt)) then
      N = mod(mem % cycles, self % flush)
      if (N == 0) N = self % flush
      call mem % getResult(zeta, STD, self % getMemAddress() + ZETA_IMP, samples = N)
    else
      call mem % getResult(zeta, STD, self % getMemAddress() + ZETA_IMP)
    end if

    allocate(res, source = keffResult([zeta, STD]))

  end subroutine getResult


end module zetaImplicitClerk_class
