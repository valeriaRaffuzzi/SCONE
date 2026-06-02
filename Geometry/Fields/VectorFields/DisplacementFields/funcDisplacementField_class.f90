module funcDisplacementField_class

  use numPrecision
  use universalVariables
  use genericProcedures,       only : fatalError, numToChar
  use dictionary_class,        only : dictionary
  use particle_class,          only : particle, particleState
  use coord_class,             only : coordList
  use field_inter,             only : field
  use displacementField_inter, only : displacementField

  ! Tally Maps
  use tallyMap_inter,             only : tallyMap
  use tallyMapFactory_func,       only : new_tallyMap

  implicit none
  private

  !!
  !! Public Pointer Cast
  !!
  public :: funcDisplacementField_TptrCast

  !! Parameters
  integer(shortInt), parameter :: EXP_FUN  = 1, &
                                  LIN_FUN  = 2, &
                                  SIN_FUN  = 3, &
                                  FLAT_FUN = 4, &
                                  POLI_FUN = 5, &
                                  POINT_FUN = 6

  !!
  !! Displacement field
  !!
  !! Parametrised field that applies a trapezoidal or triangular 2D perturbation on a plane.
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type funcDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   displacementField interface
  !!
  type, public, extends(displacementField) :: funcDisplacementField
    real(defReal)                   :: r_outer ! Radius support circle
    real(defReal)                   :: r_shift ! Radius of the top displacement
    real(defReal)                   :: r_flat  ! For trapezpoid
    real(defReal)                   :: r_backShift
    real(defReal)                   :: r_backFlat
    real(defReal)                   :: backDelta
    real(defReal)                   :: length  = ZERO
    real(defReal), dimension(2)     :: dir     = ZERO
    real(defReal), dimension(3)     :: origin  ! origin of the circle
    integer(shortInt)               :: func   = 0
    integer(shortInt)               :: N      = 0
    integer(shortInt)               :: axis   = -7
    integer(shortInt), dimension(2) :: plane  = -7
    logical(defBool)                :: radial = .false.
    real(defReal), dimension(:), allocatable :: coeffs
  contains

    ! Superclass interface
    procedure :: init
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards
    procedure :: getDelta

    ! Local procedure
    procedure :: evaluateFunction

  end type funcDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init(self, dict)
    class(funcDisplacementField), intent(inout)    :: self
    class(dictionary), intent(in)              :: dict
    real(defReal), dimension(:), allocatable   :: temp
    real(defReal)                              :: length
    character(nameLen)                         :: axis, type
    character(100), parameter :: Here = 'init (funcDisplacementField_class.f90)'

    ! Read origin of the basis
    call dict % get(temp, 'origin')
    if (size(temp) /= 3) then
      call fatalError(Here, 'origin must have size 3. Has: '//numToChar(size(temp)))
    end if
    self % origin = temp
    deallocate(temp)

    ! Read length
    call dict % get(length, 'length')
    if (length <= ZERO) call fatalError(Here, 'Length must be +ve. Is: '//numToChar(length))
    self % length = length

    ! Get direction
    call dict % get(axis, 'axis')
    select case(axis)
      case ('x')
        self % axis = X_AXIS
        self % plane = [Y_AXIS, Z_AXIS]
      case ('y')
        self % axis = Y_AXIS
        self % plane = [X_AXIS, Z_AXIS]
      case ('z')
        self % axis = Z_AXIS
        self % plane = [X_AXIS, Y_AXIS]
      case default
        call fatalError(Here, 'Unrecognised axis entry')
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
        if (size(temp) /= 3) call fatalError(Here, 'coefficients must have size 2.')

      case ('flat')
        self % func = FLAT_FUN
        if (size(temp) /= 1) call fatalError(Here, 'coefficients must have size 1.')

      case ('poly')
        self % func = POLI_FUN
        if (size(temp) /= 4) call fatalError(Here, 'coefficients must have size 4.')

      case ('pointwise')
        self % func = POINT_FUN
        call dict % get(self % N, 'points')
        if (size(temp) /= self % N) call fatalError(Here, 'coefficients must have size N.')

      case default
        call fatalError(Here, 'Unknown type of function for funcCylinder: '//type)

    end select

    self % coeffs = temp
    deallocate(temp)

    call dict % get(temp, 'direction')
    if (size(temp) == 2) then
      self % dir = temp / norm2(temp)
    elseif (size(temp) == 1) then
      self % radial = .true.
      self % dir = [ONE, ONE]
    else
      call fatalError(Here, 'Direction must have size 1 or 2. Has: '//numToChar(size(temp)))
    end if

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

    ! Load radii
    call dict % get(self % r_outer, 'r_outer')
    call dict % get(self % r_shift, 'r_shift')
    call dict % get(self % r_flat, 'r_flat')

  end subroutine init

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(funcDisplacementField), intent(inout) :: self

    self % origin  = ZERO
    self % r_outer = ZERO
    self % r_shift = ZERO
    self % r_flat  = ZERO

    ! Kill and deallocate map
    if (allocated(self % map)) then
      call self % map % kill()
      deallocate(self % map)
    end if

  end subroutine kill

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function at(self, coords) result(val)
    class(funcDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), dimension(3)              :: val, position, dir
    real(defReal)                            :: func, dr, r0

    ! Initialise result
    val = ZERO

    ! Evaluate function and check validity
    func = self % evaluateFunction(coords)
    if (func == ZERO) return

    ! Get plane and axis indexes into shorter variables (for clarity)
    position = coords % lvl(1) % r - self % origin
    position(self % axis) = ZERO

    if (self % radial) then
      r0 = norm2(position)
    else
      r0 = dot_product(position(self % plane), self % dir)
    end if

    if (r0 < ZERO .or. r0 > self % r_outer) return

    ! Calculate the TRAPEZOIDAL displacement
    if (r0 > self % r_shift) then
      dr = func * (self % r_outer - r0) / (self % r_outer - self % r_shift)
    elseif (self % r_flat /= self % r_shift .and. r0 > self % r_flat) then
      dr = func
    else
      dr = func * (r0 / self % r_flat)
    end if

    if (self % radial) then
      val = dr * position / norm2(position)
    else
      dir(self % plane) = self % dir
      dir(self % axis)  = ZERO
      val = dr * dir / norm2(dir)
  end if

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(funcDisplacementField), intent(in) :: self
    class(particle), intent(in)              :: p
    real(defReal), dimension(3)              :: val

    val = self % at(p % coords)

  end function atP

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function backwards(self, coords, delta) result(val)
    class(funcDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), intent(in), optional      :: delta
    real(defReal), dimension(3)              :: val, position, dir
    real(defReal)                            :: dr, r0, backDelta, flat, shift
    character(100), parameter :: Here = 'backwards (funcDisplacementField_class.f90)'

    ! Initialise result
    val = ZERO

    ! Evaluate function and check validity
    if (present(delta)) then
      backDelta = -delta
    else
      backDelta = -self % evaluateFunction(coords)
    end if

    if (backDelta == ZERO) return

    shift = self % r_shift - backDelta
    flat  = self % r_flat - backDelta

    ! Get plane and axis indexes into shorter variables (for clarity)
    position = coords % lvl(1) % r - self % origin
    position(self % axis) = ZERO

    if (self % radial) then
      r0 = norm2(position)
    else
      r0 = dot_product(position(self % plane), self % dir)
    end if

    if (r0 < ZERO .or. r0 > self % r_outer) return

    ! Calculate the TRAPEZOIDAL displacement
    if (r0 > shift) then
      dr = backDelta * (self % r_outer - r0) / (self % r_outer - shift)
    elseif (r0 > flat) then
      dr = backDelta
    else
      dr = backDelta * (r0 / flat)
    end if

    if (self % radial) then
      val = dr * position / norm2(position)
    else
      dir(self % plane) = self % dir
      dir(self % axis)  = ZERO
      val = dr * dir / norm2(dir)
  end if

  end function backwards

  !!
  !! Get value of delta
  !!
  function getDelta(self, coords) result(val)
    class(funcDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal)                            :: val

    val = self % evaluateFunction(coords)

  end function getDelta

  !!
  !! Evaluate input function given a particle
  !!
  function evaluateFunction(self, coords) result(func)
    class(funcDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal)                            :: func
    real(defReal), dimension(3)              :: position
    real(defReal)                            :: a0, da, f
    integer(shortInt)                        :: idx

    ! Initialise result
    func = ZERO

    ! Check if p is in the correct domain
    if (.not. self % inDomain(coords)) return

    ! Get plane and axis indexes into shorter variables (for clarity)
    position = coords % lvl(1) % r - self % origin
    a0 = position(self % axis)

    ! Particle outside length
    if (a0 < ZERO .or. a0 > self % length) return

    select case (self % func)
      case (EXP_FUN)
        func = self % coeffs(1) * (exp(self % coeffs(2) * a0) - ONE)

      case (LIN_FUN)
        func = self % coeffs(1) * a0

      case (SIN_FUN)
        func = self % coeffs(1) * sin(self % coeffs(2) * a0 + self % coeffs(3))

      case (FLAT_FUN)
        func = self % coeffs(1)

      case(POLI_FUN)
        func = self % coeffs(1) * a0**3 + self % coeffs(2) * a0**2 + &
               self % coeffs(3) * a0 + self % coeffs(4)

      case(POINT_FUN)
        idx = floor(self % N * a0 / self % length)
        da  = self % length / self % N
        f   = a0 / da - idx
        func = self % coeffs(idx + 1) * f + self % coeffs(idx) * (ONE - f)

    end select

  end function evaluateFunction

  !!
  !! Cast field pointer to funcDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of funcDisplacementField
  !!   Pointer to source if source is funcDisplacementField type
  !!
  pure function funcDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)    :: source
    type(funcDisplacementField), pointer :: ptr

    select type (source)
      type is (funcDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function funcDisplacementField_TptrCast


end module funcDisplacementField_class
