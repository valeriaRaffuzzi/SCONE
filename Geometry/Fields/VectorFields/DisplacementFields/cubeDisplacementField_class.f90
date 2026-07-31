module cubeDisplacementField_class

  use numPrecision
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
  public :: cubeDisplacementField_TptrCast

  !!
  !! Trapezoidal Displacement Field
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type cubeDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   vectorField interface
  !!
  type, public, extends(displacementField) :: cubeDisplacementField
    real(defReal), dimension(3) :: centre   ! Origin of the cube
    real(defReal)               :: a_cube   ! Half side of the cube
    real(defReal)               :: b_cube   ! Half side of displacement outer boundary
    real(defReal)               :: R_sphere ! target sphere radius
  contains

    ! Superclass interface
    procedure :: init_dict
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards

    ! Subclass interface
    procedure :: build

  end type cubeDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init_dict(self, dict)
    class(cubeDisplacementField), intent(inout) :: self
    class(dictionary), intent(in)               :: dict
    real(defReal), dimension(:), allocatable    :: centre
    real(defReal)                               :: a, b, R
    character(100), parameter :: Here = 'init_dict (cubeDisplacementField_class.f90)'

    ! Load centre
    call dict % get(centre, 'centre')
    if (size(centre) /= 3) then
      call fatalError(Here, 'Value must have size 3. Has: '//numToChar(size(centre)))
    end if

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

    ! Load radii
    call dict % get(a, 'cube')
    call dict % get(b, 'outer')
    call dict % get(R, 'sphere')

    call self % build(centre, a, b, R)

  end subroutine init_dict


  !!
  !! Build the instance from components
  !!
  !! Avoids a need for intermediate dictionary for construction
  !!
  subroutine build(self, centre, a, b, R)
    class(cubeDisplacementField), intent(inout) :: self
    real(defReal), dimension(3), intent(in)     :: centre
    real(defReal), intent(in)                   :: a
    real(defReal), intent(in)                   :: b
    real(defReal), intent(in)                   :: R

    ! Forward field
    self % centre   = centre
    self % a_cube   = a
    self % b_cube   = b
    self % R_sphere = R

  end subroutine build

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(cubeDisplacementField), intent(inout) :: self

    self % centre   = ZERO
    self % a_cube   = ZERO
    self % b_cube   = ZERO
    self % R_sphere = ZERO

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
    class(cubeDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), dimension(3)              :: val
    real(defReal), dimension(3)              :: position
    real(defReal)                            :: r, r_cube, r_b, f

    ! Initialise result
    val = ZERO

    if (.not. self % inDomain(coords)) return

    ! Calculate the position vector
    position = coords % lvl(1) % r - self % centre
    r = norm2(position)

    if (any(abs(position) > self % b_cube)) return

    r_cube = self % a_cube * r / max(abs(position(1)), abs(position(2)), abs(position(3)))
    r_b    = self % b_cube * r / max(abs(position(1)), abs(position(2)), abs(position(3)))

    if (r < r_cube) then
      val = position * (self % R_sphere / r_cube - ONE)
    else
      f   = (r_b - r) / (r_b - r_cube)
      val = f * (self % R_sphere - r_cube) * position / r
    end if

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(cubeDisplacementField), intent(in) :: self
    class(particle), intent(in)              :: p
    real(defReal), dimension(3)              :: val

    if (.not. self % inDomainP(p)) return

    val = self % at(p % coords)

  end function atP

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function backwards(self, coords, delta) result(val)
    class(cubeDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), intent(in), optional      :: delta
    real(defReal), dimension(3)              :: val
    real(defReal), dimension(3)              :: position
    real(defReal)                            :: r, r_cube, r_b, f

    ! Initialise result
    val = ZERO

    if (.not. self % inDomain(coords)) return

    ! Calculate the position vector
    position = coords % lvl(1) % r - self % centre
    r = norm2(position)

    if (any(abs(position) > self % b_cube)) return

    r_cube = self % a_cube * r / max(abs(position(1)), abs(position(2)), abs(position(3)))
    r_b    = self % b_cube * r / max(abs(position(1)), abs(position(2)), abs(position(3)))

    if (r < self % R_sphere) then
      val = position * (r_cube / self % R_sphere - ONE)
    else
      f   = (r_b - r) / (r_b - self % R_sphere)
      val = f * (r_cube - self % R_sphere) * position / r
    end if

  end function backwards

  !!
  !! Cast field pointer to cubeDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of cubeDisplacementField
  !!   Pointer to source if source is cubeDisplacementField type
  !!
  pure function cubeDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)    :: source
    type(cubeDisplacementField), pointer :: ptr

    select type (source)
      type is (cubeDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function cubeDisplacementField_TptrCast


end module cubeDisplacementField_class
