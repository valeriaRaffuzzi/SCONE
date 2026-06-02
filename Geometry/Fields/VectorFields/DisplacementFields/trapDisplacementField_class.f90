module trapDisplacementField_class

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
  public :: trapDisplacementField_TptrCast

  !!
  !! Trapezoidal Displacement Field
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type trapDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   vectorField interface
  !!
  type, public, extends(displacementField) :: trapDisplacementField
    real(defReal), dimension(3) :: centre  ! Centre of the circle
    real(defReal)               :: r_outer ! Radius support circle
    real(defReal)               :: r_shift ! Radius of the top displacement
    real(defReal)               :: r_flat  ! For trapezpoid
    real(defReal)               :: delta   ! Displacement of the top
    real(defReal)               :: r_backShift
    real(defReal)               :: r_backFlat
    real(defReal)               :: backDelta
    logical(defBool)            :: radial = .false.
    integer(shortInt), dimension(:), allocatable :: normal
    integer(shortInt), dimension(:), allocatable :: direction
  contains

    ! Superclass interface
    procedure :: init
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards
    procedure :: getDelta

    ! Subclass interface
    procedure :: build

  end type trapDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init(self, dict)
    class(trapDisplacementField), intent(inout) :: self
    class(dictionary), intent(in)               :: dict
    real(defReal), dimension(:), allocatable    :: centre
    character(1)                                :: direction
    real(defReal)                               :: ro, rs, rf, delta
    character(100), parameter :: Here = 'init (trapDisplacementField_class.f90)'

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
    call dict % get(rf, 'r_flat')
    call dict % get(delta, 'delta')

    call self % build(centre, ro, rs, rf, delta)

  end subroutine init


  !!
  !! Build the instance from components
  !!
  !! Avoids a need for intermediate dictionary for construction
  !!
  subroutine build(self, centre, r_outer, r_shift, r_flat, delta)
    class(trapDisplacementField), intent(inout) :: self
    real(defReal), dimension(3), intent(in)     :: centre
    real(defReal), intent(in)                   :: r_outer
    real(defReal), intent(in)                   :: r_shift
    real(defReal), intent(in)                   :: r_flat
    real(defReal), intent(in)                   :: delta

    ! Forward field
    self % centre  = centre
    self % r_outer = r_outer
    self % r_shift = r_shift
    self % r_flat  = r_flat
    self % delta   = delta

    ! Backward field
    self % r_backShift = self % r_shift + delta
    self % r_backFlat  = self % r_flat + delta
    self % backDelta   = -delta

  end subroutine build

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(trapDisplacementField), intent(inout) :: self

    self % centre  = ZERO
    self % r_outer = ZERO
    self % r_shift = ZERO
    self % r_flat  = ZERO
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
    class(trapDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), dimension(3)              :: val
    real(defReal), dimension(3)              :: position
    real(defReal)                            :: r, dr

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

    elseif (self % r_flat /= self % r_shift .and. r > self % r_flat) then
      dr = self % delta

    else
      dr = self % delta * (r / self % r_flat)

    end if

    val = dr * position / norm2(position)

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(trapDisplacementField), intent(in) :: self
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
    class(trapDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal), intent(in), optional      :: delta
    real(defReal), dimension(3)              :: val
    real(defReal), dimension(3)              :: position
    real(defReal)                            :: r, dr

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
    if (r > self % r_backShift) then
      dr = self % backDelta * (self % r_outer - r) / (self % r_outer - self % r_backShift)

    elseif (r > self % r_backFlat) then
      dr = self % backDelta

    else
      dr = self % backDelta * (r / self % r_backFlat)
    end if

    val = dr * position / norm2(position)

  end function backwards

  !!
  !! Get value of delta
  !!
  function getDelta(self, coords) result(val)
    class(trapDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal)                            :: val

    val = self % delta

  end function getDelta

  !!
  !! Cast field pointer to trapDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of trapDisplacementField
  !!   Pointer to source if source is trapDisplacementField type
  !!
  pure function trapDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)    :: source
    type(trapDisplacementField), pointer :: ptr

    select type (source)
      type is (trapDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function trapDisplacementField_TptrCast


end module trapDisplacementField_class
