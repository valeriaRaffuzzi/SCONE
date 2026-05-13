!!
!! Transport operator for delta tracking
!!
module transportOperatorDTField_class
  use numPrecision
  use universalVariables

  use errors_mod,                  only : fatalError
  use genericProcedures,           only : numToChar
  use particle_class,              only : particle
  use particleDungeon_class,       only : particleDungeon
  use dictionary_class,            only : dictionary

  ! Superclass
  use transportOperator_inter,     only : transportOperator, init_super => init

  ! Geometry interfaces
  use geometry_inter,              only : geometry
  use trapDisplacementField_class, only : trapDisplacementField
  use funcDisplacementField_class, only : funcDisplacementField

  ! Tally interface
  use tallyCodes
  use tallyAdmin_class,            only : tallyAdmin

  ! Nuclear data interfaces
  use nuclearDataReg_mod,          only : ndReg_get => get
  use nuclearDatabase_inter,       only : nuclearDatabase

  implicit none
  private

  !!
  !! Transport operator that moves a particle with delta tracking
  !!
  type, public, extends(transportOperator) :: transportOperatorDTField
    type(funcDisplacementField) :: forward
    type(trapDisplacementField) :: backward
  contains

    procedure :: transit => deltaTracking

    ! Override procedure
    procedure :: init
    procedure :: step
    procedure :: setDelta

  end type transportOperatorDTField

contains

  !!
  !!
  !!
  subroutine setDelta(self, delta)
    class(transportOperatorDTField), intent(inout) :: self
    real(defReal), intent(in)                      :: delta

    self % backward % r_shift = self % forward % r_shift + delta
    self % backward % r_flat  = self % forward % r_flat + delta
    self % backward % delta   = -delta

  end subroutine setDelta

  !!
  !! Move the particle in the geometry
  !!
  subroutine step(self, p, distance)
    class(transportOperatorDTField), intent(inout) :: self
    class(particle), intent(inout)                 :: p
    real(defReal), intent(in)                      :: distance
    real(defReal), dimension(3)                    :: displacement

    ! Calculating displacement before using takeAboveGeom, in order to use maps
    ! (e.g., materialMaps) in the field
    call self % forward % evaluateFunction(p)
    displacement = self % forward % atP(p)

    ! Pop particle out of the geometry
    call p % coords % takeAboveGeom()

    ! Move the particle to the real frame
    call p % coords % assignPosition(p % rGlobal() + displacement)

    call self % geom % teleport(p % coords, distance)

    ! Move back to the map
    call self % setDelta(self % forward % funcVal)
    call p % coords % assignPosition(p % rGlobal() + self % backward % atP(p))
    call self % geom % placeCoord(p % coords)

  end subroutine step

  !!
  !! Performs delta tracking until a real collision point is found
  !!
  subroutine deltaTracking(self, p, tally, thisCycle, nextCycle)
    class(transportOperatorDTField), intent(inout) :: self
    class(particle), intent(inout)            :: p
    type(tallyAdmin), intent(inout)           :: tally
    class(particleDungeon), intent(inout)     :: thisCycle
    class(particleDungeon), intent(inout)     :: nextCycle
    real(defReal)                             :: majorant_inv, sigmaT, distance
    character(100), parameter :: Here = 'deltaTracking (transportOperatorDTField_class.f90)'

    ! Get majorant XS inverse: 1/Sigma_majorant
    majorant_inv = ONE / self % xsData % getTrackingXS(p, p % matIdx(), MAJORANT_XS)

   ! Should never happen! Prevents Inf distances
    if (abs(majorant_inv) > huge(majorant_inv)) call fatalError(Here, "Majorant is 0")

    DTLoop:do
      distance = -log( p% pRNG % get() ) * majorant_inv

      ! Move partice in the geometry
      ! call self % geom % teleport(p % coords, distance)
      call self % step(p, distance)

      ! If particle has leaked, exit
      if (p % matIdx() == OUTSIDE_FILL) then
        p % fate = LEAK_FATE
        p % isDead = .true.
        return
      end if

      ! Check for void
      if (p % matIdx() == VOID_MAT) then
        call tally % reportInColl(p, .true.)
        cycle DTLoop
      end if

      ! Give error if the particle somehow ended in an undefined material
      if (p % matIdx() == UNDEF_MAT) then
        print *, p % rGlobal()
        call fatalError(Here, "Particle is in undefined material")
      end if

      ! Obtain the local cross-section
      sigmaT = self % xsData % getTrackMatXS(p, p % matIdx())

      ! Roll RNG to determine if the collision is real or virtual
      ! Exit the loop if the collision is real, report collision if virtual
      if (p % pRNG % get() < sigmaT*majorant_inv) then
        exit DTLoop
      else
        call tally % reportInColl(p, .true.)
      end if

    end do DTLoop

    call tally % reportTrans(p)

  end subroutine deltaTracking

  !!
  !! Initialise DT transport operator
  !!
  !! See transportOperator_inter for more details
  !!
  subroutine init(self, dict)
    class(transportOperatorDTField), intent(inout) :: self
    class(dictionary), intent(in)                  :: dict
    character(100), parameter :: Here = 'init (transportOperatorDTField_class.f90)'

    ! Initialise superclass
    call init_super(self, dict)

    ! Build forward field
    call self % forward % init(dict)

    ! Build backwards field
    call self % backward % init(dict)

  end subroutine init


end module transportOperatorDTField_class
