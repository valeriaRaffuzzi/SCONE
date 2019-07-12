module aceNeutronNuclide_class

  use numPrecision
  use endfConstants
  use genericProcedures, only : fatalError
  use RNG_class,         only : RNG
  use aceCard_class,     only : aceCard
  use stack_class,       only : stackInt

  ! Nuclear Data Interfaces
  use ceNeutronDatabase_inter,     only : ceNeutronDatabase
  use ceNeutronNuclide_inter,      only : ceNeutronNuclide, kill_super => kill
  use elasticNeutronScatter_class, only : elasticNeutronScatter
  use fissionCE_class,             only : fissionCE
  use neutronScatter_class,        only : neutronScatter

  implicit none
  private

  ! Grid location parameters
  integer(shortInt), parameter :: ENERGY_GRID   = 1
  integer(shortInt), parameter :: TOTAL_XS      = 2
  integer(shortInt), parameter :: ESCATTER_XS   = 3
  integer(shortInt), parameter :: IESCATTER_XS  = 4
  integer(shortInt), parameter :: CAPTURE_XS    = 5
  integer(shortInt), parameter :: FISSION_XS    = 6
  integer(shortInt), parameter :: NU_FISSION    = 7


  !!
  !!
  !!
  type, public :: reactionMT
    integer(shortInt)                      :: MT       = 0
    integer(shortInt)                      :: firstIdx = 0
    real(defReal),dimension(:),allocatable :: xs
    type(neutronScatter)                   :: kinematics
  end type reactionMT

  !!
  !! Doc Doc Docstring
  !!
  !!
  type, public, extends(ceNeutronNuclide) :: aceNeutronNuclide
    real(defReal), dimension(:,:), allocatable  :: mainData
    type(reactionMT), dimension(:), allocatable :: MTdata
    integer(shortInt)                           :: nMT     = 0

    type(elasticNeutronScatter) :: elasticScatter
    type(fissionCE)             :: fission

  contains
    ! Superclass Interface
    procedure :: invertInelastic
    procedure :: xsOf

    ! Local interface
    procedure :: init
    procedure :: kill

  end type aceNeutronNuclide

contains

  !!
  !! Invert PDF of inelastic stattering
  !!
  !! See ceNeutronNuclide documentation
  !!
  function invertInelastic(self, E, rand) result(MT)
    class(aceNeutronNuclide), intent(in) :: self
    real(defReal), intent(in)            :: E
    class(RNG), intent(inout)            :: rand
    integer(shortInt)                    :: MT

    MT = 1

  end function invertInelastic

  !!
  !! Return Cross-Section of reaction MT at energy E
  !!
  !! See ceNeutronNuclide documentation
  !!
  function xsOf(self, MT, E) result(xs)
    class(aceNeutronNuclide), intent(in) :: self
    integer(shortInt), intent(in)        :: MT
    real(defReal), intent(in)            :: E
    real(defReal)                        :: xs

    xs = ONE

  end function xsOf

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(aceNeutronNuclide), intent(inout) :: self

    ! Call superclass
    call kill_super(self)


  end subroutine kill

  !!
  !! Initialise from an ACE Card
  !!
  !! Args:
  !!   ACE [inout]   -> ACE card
  !!   nucIdx [in]   -> nucIdx of the nuclide
  !!   database [in] -> pointer to ceNeutronDatabase to set XSs to cache
  !!
  !! Errors:
  !!
  !!
  subroutine init(self, ACE, nucIdx, database)
    class(aceNeutronNuclide), intent(inout)       :: self
    class(aceCard), intent(inout)                 :: ACE
    integer(shortInt), intent(in)                 :: nucIdx
    class(ceNeutronDatabase), pointer, intent(in) :: database
    integer(shortInt)                             :: Ngrid, N, K, i
    type(stackInt)                                :: scatterMT, absMT

    ! Reset nuclide just in case
    call self % kill()

    ! Read key data into the superclass
    call self % set( fissile  = ACE % isFissile(), &
                     mass     = ACE % AW,          &
                     kT       = ACE % TZ,          &
                     nucIdx   = nucIdx,            &
                     database = database )

    ! Get size of the grid
    Ngrid = ACE % gridSize()

    ! Allocate space for main XSs
    if(self % isFissile()) then
      N = 7
    else
      N = 5
    end if
    allocate(self % mainData(Ngrid, N))

    self % mainData = ZERO

    ! Load Main XSs
    self % mainData(:,ENERGY_GRID)  = ACE % ESZ_XS('energyGrid')
    self % mainData(:,TOTAL_XS)     = ACE % ESZ_XS('totalXS')
    self % mainData(:,ESCATTER_XS)  = ACE % ESZ_XS('elasticXS')
    self % mainData(:,CAPTURE_XS)   = ACE % ESZ_XS('absorbtionXS')

    ! Get elastic kinematics
    call self % elasticScatter % init(ACE, N_N_ELASTIC)

    if(self % isFissile()) then
      call self % fission % init(ACE, N_FISSION)
      N = ACE % firstIdxFiss()
      K = ACE % numXSPointsFiss()
      self % mainData(N:N+K-1,FISSION_XS) = ACE % xsFiss()

      ! Calculate nuFission
      do i = N, K
        self % mainData(i,NU_FISSION) = self % mainData(i,FISSION_XS) * &
                                        self % fission % release(self % mainData(i,ENERGY_GRID))
      end do
    end if

    ! Read data for MT reaction

    ! Loop over all MT reactions and sort them to scattering and absorbtion stack
    ! TODO: Add some provision for deactivation of custom MT reaction


    ! Get number of scattering reactions
    ! Get total number of MT reactions
    ! Create que of MT numbers
    ! Load each MT Reaction one-by-one



    ! Recalculate Total XS
    ! *** DO NOT FORGET TO CALCULATE TOTAL INELASTIC SCATTER *** !

  end subroutine init
    
end module aceNeutronNuclide_class