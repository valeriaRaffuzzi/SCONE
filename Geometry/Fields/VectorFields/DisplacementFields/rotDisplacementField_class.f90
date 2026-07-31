module rotDisplacementField_class

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
  public :: rotDisplacementField_TptrCast

  !!
  !! Displacement field
  !!
  !! Parametrised field that applies a trapezoidal or triangular 2D perturbation on a plane.
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type rotDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   displacementField interface
  !!
  type, public, extends(displacementField) :: rotDisplacementField
    real(defReal)                   :: r_outer
    real(defReal)                   :: halflength  = ZERO
    real(defReal), dimension(3)     :: origin
    integer(shortInt)               :: N = 0
  contains

    ! Superclass interface
    procedure :: init_dict
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards

  end type rotDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init_dict(self, dict)
    class(rotDisplacementField), intent(inout) :: self
    class(dictionary), intent(in)              :: dict
    real(defReal), dimension(:), allocatable   :: temp
    real(defReal)                              :: halflength
    character(100), parameter :: Here = 'init_dict (rotDisplacementField_class.f90)'

    ! Read origin of the basis
    call dict % get(temp, 'origin')
    if (size(temp) /= 3) then
      call fatalError(Here, 'origin must have size 3. Has: '//numToChar(size(temp)))
    end if
    self % origin = temp
    deallocate(temp)

    ! Read length
    call dict % get(halflength, 'halflength')
    if (halflength < ZERO) call fatalError(Here, 'Length must be +ve. Is: '//numToChar(halflength))
    self % halflength = halflength

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

    ! Load outer radius
    call dict % get(self % r_outer, 'r_outer')

    ! Load number of revolutions
    call dict % get(self % N, 'rotations')

  end subroutine init_dict

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(rotDisplacementField), intent(inout) :: self

    self % origin  = ZERO
    self % r_outer = ZERO
    self % halflength = ZERO

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
    class(rotDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), dimension(3)             :: val, position, pos_new
    real(defReal), dimension(2)             :: p0
    real(defReal)                           :: a0, r0, length, f, theta
    integer(shortInt)                       :: idx

    ! Initialise result
    val = ZERO

    ! Get plane and axis indexes into shorter variables (for clarity)
    position = coords % lvl(1) % r - self % origin
    a0 = position(3) + self % halflength
    p0 = position([1,2])

    r0 = norm2(p0)

    if (r0 > self % r_outer) return

    ! Find axial position
    length = TWO * self % halflength
    idx = floor(self % N * a0 / length)
    f   = self % N * a0 / length - idx

    ! Rotation angle
    theta = f * TWO_PI
    pos_new(1) = p0(1) * cos(theta) - p0(2) * sin(theta)
    pos_new(2) = p0(1) * sin(theta) + p0(2) * cos(theta)
    pos_new(3) = position(3)

    val = pos_new - position

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(rotDisplacementField), intent(in) :: self
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
    class(rotDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), intent(in), optional     :: delta
    real(defReal), dimension(3)             :: val, position, pos_new
    real(defReal), dimension(2)             :: p0
    real(defReal)                           :: a0, r0, length, f, theta
    integer(shortInt)                       :: idx

    ! Initialise result
    val = ZERO

    ! Get plane and axis indexes into shorter variables (for clarity)
    position = coords % lvl(1) % r - self % origin
    a0 = position(3) + self % halflength
    p0 = position([1,2])

    r0 = norm2(p0)

    if (r0 > self % r_outer) return

    ! Find axial position
    length = TWO * self % halflength
    idx = floor(self % N * a0 / length)
    f   = self % N * a0 / length - idx

    ! Rotation angle
    theta = -f * TWO_PI
    pos_new(1) = p0(1) * cos(theta) - p0(2) * sin(theta)
    pos_new(2) = p0(1) * sin(theta) + p0(2) * cos(theta)
    pos_new(3) = position(3)

    val = pos_new - position


  end function backwards

  !!
  !! Cast field pointer to rotDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of rotDisplacementField
  !!   Pointer to source if source is rotDisplacementField type
  !!
  pure function rotDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)    :: source
    type(rotDisplacementField), pointer :: ptr

    select type (source)
      type is (rotDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function rotDisplacementField_TptrCast


end module rotDisplacementField_class
