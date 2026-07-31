module hatDisplacementField_class

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
  public :: hatDisplacementField_TptrCast

  !!
  !! Hat Displacement Field
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type hatDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   vectorField interface
  !!
  type, public, extends(displacementField) :: hatDisplacementField
    real(defReal), dimension(3) :: centre  ! Centre of the circle
    real(defReal)               :: r_outer ! Radius support circle
    real(defReal)               :: r_shift ! Radius of the top displacement
    real(defReal)               :: delta   ! Displacement of the top
    real(defReal)               :: r_backShift
    real(defReal)               :: backDelta
    logical(defBool)            :: radial = .false.
    integer(shortInt), dimension(:), allocatable :: normal
    integer(shortInt), dimension(:), allocatable :: direction
  contains

    ! Superclass interface
    procedure :: init_dict
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards

    ! Subclass interface
    procedure :: build

  end type hatDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init_dict(self, dict)
    class(hatDisplacementField), intent(inout) :: self
    class(dictionary), intent(in)              :: dict
    real(defReal), dimension(:), allocatable   :: centre
    character(1)                               :: direction
    real(defReal)                              :: ro, rs, delta
    character(100), parameter :: Here = 'init_dict (hatDisplacementField_class.f90)'

    ! Load centre
    call dict % get(centre, 'centre')
    if (size(centre) /= 3) then
      call fatalError(Here, 'Value must have size 3. Has: '//numToChar(size(centre)))
    end if

    ! Get direction
    call dict % getorDefault(direction, 'direction','r')
    select case(direction)
      case ('x')
        self % normal    = [2, 3]
        self % direction = [1]
      case ('y')
        self % normal    = [1, 3]
        self % direction = [2]
      case ('z')
        self % normal    = [1, 2]
        self % direction = [3]
      case ('r')
        self % normal    = [3]
        self % direction = [1, 2]
        self % radial    = .true.
      case default
        call fatalError(Here, 'Unrecognised direction entry')
    end select

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

    ! Load radii
    call dict % get(ro, 'r_outer')
    call dict % get(rs, 'r_shift')
    call dict % get(delta, 'delta')

    call self % build(centre, ro, rs, delta)

  end subroutine init_dict


  !!
  !! Build the instance from components
  !!
  !! Avoids a need for intermediate dictionary for construction
  !!
  subroutine build(self, centre, r_outer, r_shift, delta)
    class(hatDisplacementField), intent(inout) :: self
    real(defReal), dimension(3), intent(in)    :: centre
    real(defReal), intent(in)                  :: r_outer
    real(defReal), intent(in)                  :: r_shift
    real(defReal), intent(in)                  :: delta
    character(100), parameter :: Here = 'build (hatDisplacementField_class.f90)'

    ! Forward field
    self % centre  = centre
    self % r_outer = r_outer
    self % r_shift = r_shift
    self % delta   = delta

    ! Backward field
    self % r_backShift = self % r_shift + delta
    self % backDelta   = -delta

  end subroutine build

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(hatDisplacementField), intent(inout) :: self

    self % centre  = ZERO
    self % r_outer = ZERO
    self % r_shift = ZERO
    self % delta   = ZERO

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
    class(hatDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), dimension(3)             :: val
    real(defReal), dimension(3)             :: position
    real(defReal)                           :: r, dr

    ! Initialise result
    val = ZERO

    if (.not. self % inDomain(coords)) return

    ! Calculate the position vector
    position = coords % lvl(1) % r - self % centre
    position(self % normal) = ZERO

    if (self % radial) then
      r = norm2(position)
    else
      r = position(self % direction(1))
    end if

    if (r <= ZERO .or. r > self % r_outer) return

    ! Calculate the displacement
    if (r > self % r_shift) then
      dr = self % delta * (self % r_outer - r) / (self % r_outer - self % r_shift)
    else
      dr = self % delta * (r / self % r_shift)
    end if

    val = dr * position / norm2(position)

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(hatDisplacementField), intent(in) :: self
    class(particle), intent(in)             :: p
    real(defReal), dimension(3)             :: val

    val = self % at(p % coords)

  end function atP

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function backwards(self, coords, delta) result(val)
    class(hatDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), intent(in), optional     :: delta
    real(defReal), dimension(3)             :: val
    real(defReal), dimension(3)             :: position
    real(defReal)                           :: r, dr

    ! Initialise result
    val = ZERO

    ! Check domain
    if (.not. self % inDomain(coords)) return

    ! Calculate the position vector
    position = coords % lvl(1) % r - self % centre
    position(self % normal) = ZERO

    if (self % radial) then
      r = norm2(position)
    else
      r = position(self % direction(1))
    end if

    if (r <= ZERO .or. r > self % r_outer) return

    ! Calculate the displacement
    if (r > self % r_backShift) then
      dr = self % backDelta * (self % r_outer - r) / (self % r_outer - self % r_backShift)
    else
      dr = self % backDelta * (r / self % r_backShift)
    end if

    val = dr * position / norm2(position)

  end function backwards

  !!
  !! Cast field pointer to hatDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of hatDisplacementField
  !!   Pointer to source if source is hatDisplacementField type
  !!
  pure function hatDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)    :: source
    type(hatDisplacementField), pointer :: ptr

    select type (source)
      type is (hatDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function hatDisplacementField_TptrCast


end module hatDisplacementField_class
