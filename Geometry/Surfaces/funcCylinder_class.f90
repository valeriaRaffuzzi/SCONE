module funcCylinder_class

  use numPrecision
  use universalVariables
  use genericProcedures,  only : fatalError, numToChar, swap
  use dictionary_class,   only : dictionary
  use surface_inter,      only : surface, kill_super => kill

  implicit none
  private

  integer(shortInt), parameter :: EXP_FUN = 1, &
                                  LIN_FUN = 2, &
                                  SIN_FUN = 3

  !!
  !! Finite length cylinder aligned with one of the co-ord axis (x, y or z)
  !!
  !! F(r) = max[ {(r1-o1)^2 + (r2-o2)^2 - R^2}; abs(r3-o3)-a]
  !!
  !! r -> position; o -> origin; R -> radius; a-> halfwidth
  !! 1,2 -> planar axis, 3-> cylinder axis
  !!
  !! Three diffrent types are avaliable
  !!   xFuncCylinder -> aligned with X-axis
  !!   yFuncCylinder -> aligned with Y-axis
  !!   zFuncCylinder -> aligned with Z-axis
  !!
  !! Surface Tolerance: SURF_TOL
  !!
  !! Sample Dictionary Input:
  !!  x {type xFuncCylinder; id 2; origin (1.0 -2.0 0.0); length 1.2; radius 0.5;}
  !!  y {type yFuncCylinder; id 2; origin (1.0 2.0 7.0); length 1.3; radius 1.5; function exp; coeff (0 1); }
  !!
  !! Boundary Conditions:
  !!   BC order: -INF, INF
  !!
  !! Private Members:
  !!   origin -> Position of the centre of the cylinder
  !!   a      -> Axial halfwidth (>0.0)
  !!   r      -> Radius (>0.0)
  !!   axis   -> Cylinder axis specifier
  !!   plane  -> Planar axis specifiers
  !!   BC     -> Boundary conditions flags [a_min, a_max]
  !!
  !! Interface:
  !!   surface interface
  !!
  type, public, extends(surface) :: funcCylinder
    private
    real(defReal), dimension(3)     :: origin = ZERO
    real(defReal), dimension(2)     :: dir    = ZERO
    real(defReal)                   :: length = ZERO
    real(defReal)                   :: r      = ZERO
    integer(shortInt)               :: func   = 0
    integer(shortInt)               :: axis   = -7
    integer(shortInt), dimension(2) :: plane  = -7
    integer(shortInt), dimension(2) :: BC     = VACUUM_BC
    real(defReal), dimension(:), allocatable :: coeffs

  contains
    ! Superclass procedures
    procedure :: myType
    procedure :: init
    procedure :: boundingBox
    procedure :: evaluate
    procedure :: distance
    procedure :: going
    procedure :: normal
    procedure :: kill

  end type funcCylinder

contains

  !!
  !! Return surface type name
  !!
  !! See surface_inter for more details
  !!
  pure function myType(self) result(str)
    class(funcCylinder), intent(in) :: self
    character(:), allocatable       :: str
    character(100), parameter :: Here = 'myType (funcCylinder_class.f90)'

    select case (self % axis)
      case (X_AXIS)
        str = 'xFuncCylinder'

      case (Y_AXIS)
        str = 'yFuncCylinder'

      case (Z_AXIS)
        str = 'zFuncCylinder'

      case default
        str = 'Unknown Function Cylinder'

    end select

  end function myType

  !!
  !! Initialise funcCylinder from a dictionary
  !!
  !! See surface_inter for more details
  !!
  subroutine init(self, dict)
    class(funcCylinder), intent(inout)       :: self
    class(dictionary), intent(in)            :: dict
    integer(shortInt)                        :: id
    real(defReal), dimension(:), allocatable :: temp
    real(defReal)                            :: r, length
    character(nameLen)                       :: type
    character(100), parameter :: Here = 'init (funcCylinder_class.f90)'

    ! Read id
    call dict % get(id, 'id')
    if (id <= 0) call fatalError(Here,'ID must be <= 0. Is: '//numToChar(id))
    call self % setID(id)

    ! Read origin of the basis
    call dict % get(temp, 'origin')
    if (size(temp) /= 3) then
      call fatalError(Here, 'origin must have size 3. Has: '//numToChar(size(temp)))
    end if
    self % origin = temp
    deallocate(temp)

    ! Read radius
    call dict % get(r, 'radius')
    if (r <= ZERO) call fatalError(Here, 'Radius must be +ve. Is: '//numToChar(r))
    self % r = r

    ! Read length
    call dict % get(length, 'length')
    if (length <= ZERO) call fatalError(Here, 'Length must be +ve. Is: '//numToChar(length))
    self % length = length

    ! Read type
    call dict % get(type, 'type')
    select case (type)
      case ('xFuncCylinder')
        self % axis = X_AXIS
        self % plane = [Y_AXIS, Z_AXIS]

      case ('yFuncCylinder')
        self % axis = Y_AXIS
        self % plane = [X_AXIS, Z_AXIS]

      case ('zFuncCylinder')
        self % axis = Z_AXIS
        self % plane = [X_AXIS, Y_AXIS]

      case default
        call fatalError(Here, 'Unknown type of funcCylinder: '//type)

    end select

    ! Read function
    call dict % get(temp, 'coefficients')
    call dict % get(type, 'function')
    select case (type)
      case ('exp')
        self % func = EXP_FUN
        if (size(temp) /= 2) call fatalError(Here, 'coefficients must have size 2.')

      case ('lin')
        self % func = LIN_FUN
        if (size(temp) /= 1) call fatalError(Here, 'coefficients must have size 1.')

      case ('sin')
        self % func = SIN_FUN
        if (size(temp) /= 2) call fatalError(Here, 'coefficients must have size 2.')

      case default
        call fatalError(Here, 'Unknown type of function for funcCylinder: '//type)

    end select

    self % coeffs = temp
    deallocate(temp)

    call dict % get(temp, 'direction')
    if (size(temp) /= 2) call fatalError(Here, 'Direction must have size 2. Has: '//numToChar(size(temp)))
    self % dir = temp / norm2(temp)

  end subroutine init

  !!
  !! Return axis-aligned bounding funcCylinder for the surface
  !!
  !! See surface_inter for details
  !!
  pure function boundingBox(self) result(aabb)
    class(funcCylinder), intent(in) :: self
    real(defReal), dimension(6)     :: aabb

    ! Not needed
    aabb = INF

  end function boundingBox

  !!
  !! Evaluate surface expression c = F(r)
  !!
  !! See surface_inter for details
  !!
  pure function evaluate(self, r) result(c)
    class(funcCylinder), intent(in)         :: self
    real(defReal), dimension(3), intent(in) :: r
    real(defReal)                           :: c, a0, func
    real(defReal), dimension(2)             :: r0, rRef
    integer(shortInt), dimension(2)         :: p
    integer(shortInt)                       :: a

    ! Get plane and axis indexes into shorter variables (for clarity)
    p  = self % plane
    a  = self % axis
    r0 = r(p) - self % origin(p)
    a0 = r(a) - self % origin(a)

    ! Particle outside length
    if (a0 < ZERO .or. a0 > self % length) then
      c = ONE
      return
    end if

    select case (self % func)
      case (EXP_FUN)
        func = self % coeffs(1) * exp(self % coeffs(2) * a0) - ONE

      case (LIN_FUN)
        func = self % coeffs(1) * a0

      case (SIN_FUN)
        func = self % coeffs(1) * sin(self % coeffs(2) * a0)

    end select

    rRef = r0 + func * self % dir

    ! Evaluate surface expression
    c = (sum(rRef(p)**2) - self % r * self % r)

  end function evaluate

  !!
  !! Return distance to the surface
  !!
  !! See surface_inter for details
  !!
  !! Uses intersection method analogous to box_class
  !! For min and max distance calculation for cylinder see cylinder_class
  !!
  pure function distance(self, r, u) result(d)
    class(funcCylinder), intent(in)        :: self
    real(defReal), dimension(3), intent(in) :: r
    real(defReal), dimension(3), intent(in) :: u
    real(defReal)                           :: d

    d = INF

  end function distance

  !!
  !! Returns TRUE if particle is going into +ve halfspace
  !!
  !! See surface_inter for details
  !!
  pure function going(self, r, u) result(halfspace)
    class(funcCylinder), intent(in)       :: self
    real(defReal), dimension(3), intent(in) :: r
    real(defReal), dimension(3), intent(in) :: u
    logical(defBool)                        :: halfspace

    halfspace = .true.

  end function going

  !!
  !! Return the normal corresponding to the surface
  !!
  pure function normal(self, r, u) result(n)
    class(funcCylinder), intent(in)         :: self
    real(defReal), dimension(3), intent(in) :: r
    real(defReal), dimension(3), intent(in) :: u
    real(defReal), dimension(3)             :: n

    n = [ZERO, ZERO, ZERO]

  end function normal

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(funcCylinder), intent(inout) :: self

    ! Superclass
    call kill_super(self)

    ! Local
    self % origin = ZERO
    self % dir    = ZERO
    self % length = ZERO
    self % coeffs = ZERO
    self % r      = ZERO
    self % func   = 0
    self % axis   = -7
    self % plane  = -7
    self % BC     = VACUUM_BC

  end subroutine kill


end module funcCylinder_class
