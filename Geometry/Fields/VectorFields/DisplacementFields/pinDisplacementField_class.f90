module pinDisplacementField_class

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
  public :: pinDisplacementField_TptrCast

  !! Parameters
  integer(shortInt), parameter :: P1 = 1, &
                                  P2 = 2, &
                                  AX = 3

  !!
  !! Displacement field
  !!
  !! Parametrised field that applies a trapezoidal or triangular 2D perturbation on a plane.
  !!
  !! Sample Dictionary Input:
  !!   field {
  !!     type pinDisplacementField;
  !!     smth;
  !!   }
  !!
  !! Public Members:
  !!   val -> Value of the field
  !!
  !! Interface:
  !!   displacementField interface
  !!
  type, public, extends(displacementField) :: pinDisplacementField
    integer(shortInt)               :: N
    logical(defBool)                :: axialSymmetry = .false.
    real(defReal)                   :: r_fuel
    real(defReal)                   :: r_gap
    real(defReal)                   :: r_clad
    real(defReal)                   :: r_outer
    real(defReal)                   :: halflength
    real(defReal)                   :: z_bottom
    real(defReal)                   :: z_top
    real(defReal)                   :: delta_z
    real(defReal), dimension(3)     :: origin
    real(defReal), dimension(:,:), allocatable :: delta_r
  contains

    ! Superclass interface
    procedure :: init
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards
    procedure :: getDelta

    ! Local procedure
    procedure, private :: axialDisplacement
    procedure, private :: radialDisplacement

  end type pinDisplacementField

contains

  !!
  !! Initialise from dictionary
  !!
  !! See field_inter for details
  !!
  subroutine init(self, dict)
    class(pinDisplacementField), intent(inout) :: self
    class(dictionary), intent(in)              :: dict
    real(defReal), dimension(:), allocatable   :: temp
    character(100), parameter :: Here = 'init (pinDisplacementField_class.f90)'

    ! Read origin of the basis
    call dict % get(temp, 'origin')
    if (size(temp) /= 3) call fatalError(Here, 'origin must have size 3. Has: '//numToChar(size(temp)))
    self % origin = temp
    deallocate(temp)

    ! Load radii
    call dict % get(self % r_fuel,  'r_fuel')
    call dict % get(self % r_gap,   'r_gap')
    call dict % get(self % r_clad,  'r_clad')
    call dict % get(self % r_outer, 'r_outer')
    call dict % get(self % N, 'nodes')

    ! Read length
    call dict % get(self % halflength, 'halflength')
    call dict % get(self % z_bottom, 'z_bottom')
    call dict % get(self % z_top, 'z_top')

    ! Read settings
    call dict % getOrDefault(self % axialSymmetry, 'symmetric', .false.)

    ! Read deltas
    call dict % get(self % delta_z, 'delta_z')

    allocate(self % delta_r(self % N + 1, 3))

    call dict % get(temp, 'delta_f')
    if (size(temp) /= self % N + 1) call fatalError(Here, 'delta_f must have size: '//numToChar(self % N + 1))
    self % delta_r(:,1) = temp
    deallocate(temp)

    call dict % get(temp, 'delta_g')
    if (size(temp) /= self % N + 1) call fatalError(Here, 'delta_f must have size: '//numToChar(self % N + 1))
    self % delta_r(:,2) = temp
    deallocate(temp)

    call dict % get(temp, 'delta_c')
    if (size(temp) /= self % N + 1) call fatalError(Here, 'delta_f must have size: '//numToChar(self % N + 1))
    self % delta_r(:,3) = temp
    deallocate(temp)

    ! Load map
    if (dict % isPresent('map')) then
      call new_tallyMap(self % map, dict % getDictPtr('map'))
    end if

  end subroutine init

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(pinDisplacementField), intent(inout) :: self

    self % N = 0

    self % origin  = ZERO
    self % r_fuel  = ZERO
    self % r_gap   = ZERO
    self % r_clad  = ZERO
    self % r_outer = ZERO
    self % z_top   = ZERO
    self % z_bottom   = ZERO
    self % halflength = ZERO
    self % delta_z    = ZERO

    ! Kill and deallocate arrays
    if (allocated(self % delta_r)) deallocate(self % delta_r)

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
    class(pinDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), dimension(3)             :: val, position
    real(defReal), dimension(2)             :: plane
    real(defReal)                           :: a0, r0
    character(100), parameter :: Here = 'at (pinDisplacementField_class.f90)'

    ! Initialise result
    val = ZERO

    ! Check if p is in the correct domain
    if (.not. self % inDomain(coords)) return

    ! Calculate relative position
    position = coords % lvl(1) % r - self % origin
    plane    = position([P1,P2])
    a0 = position(AX)
    r0 = norm2(plane)

    ! Particle outside length or radius
    if (a0 < self % z_bottom .or. a0 > self % z_top .or. r0 > self % r_outer) return

    val([P1,P2]) = radialDisplacement(self, a0, r0, plane, backwards = .false.)
    val(AX) = axialDisplacement(self, a0, backwards = .false.)

  end function at

  !!
  !! Get value of the scalar field at the co-ordinate point
  !!
  !! See vectorField_inter for details
  !!
  function atP(self, p) result(val)
    class(pinDisplacementField), intent(in) :: self
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
    class(pinDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), intent(in), optional     :: delta
    real(defReal), dimension(3)             :: val, position
    real(defReal), dimension(2)             :: plane
    real(defReal)                           :: a0, r0
    character(100), parameter :: Here = 'backwards (pinDisplacementField_class.f90)'

    ! Initialise result
    val = ZERO

    ! Check if p is in the correct domain
    if (.not. self % inDomain(coords)) return

    ! Calculate relative position
    position = coords % lvl(1) % r - self % origin
    plane    = position([P1,P2])
    a0 = position(AX)
    r0 = norm2(plane)

    ! Particle outside length or radius
    if (a0 < self % z_bottom .or. a0 > self % z_top .or. r0 > self % r_outer) return

    val([P1,P2]) = radialDisplacement(self, a0, r0, plane, backwards = .true.)
    val(AX) = axialDisplacement(self, a0, backwards = .true.)

  end function backwards

  !!
  !! Get value of delta
  !!
  function getDelta(self, coords) result(val)
    class(pinDisplacementField), intent(in) :: self
    class(coordList), intent(in)             :: coords
    real(defReal)                            :: val

    ! This doesn't really mean anything here
    val = ZERO

  end function getDelta

  !!
  !! Evaluate input function given a particle
  !!
  function axialDisplacement(self, a0, backwards) result(dz)
    class(pinDisplacementField), intent(in) :: self
    real(defReal), intent(in)               :: a0
    logical(defBool), intent(in)            :: backwards
    real(defReal)                           :: dz
    real(defReal)                           :: halflength, delta, z1, z2

    if (.not. self % axialSymmetry .and. a0 < ZERO) return

    ! Assign parameters
    if (backwards) then
      delta = -self % delta_z
      halflength = self % halflength - delta
    else
      delta = self % delta_z
      halflength = self % halflength
    end if

    ! Assign relative coordinate values
    z1 = -halflength
    z2 = halflength

    ! Calculate axial displacement
    if (a0 > z2) then
      dz = delta * (a0 - self % z_top) / (z2 - self % z_top)
    elseif (a0 > z1) then
      dz = delta * a0 / z2
    else
      dz = -delta * (a0 - self % z_bottom) / (z1 - self % z_bottom)
    end if

  end function axialDisplacement

  !!
  !! Evaluate input function given a particle
  !!
  function radialDisplacement(self, a0, r0, plane, backwards) result(dr)
    class(pinDisplacementField), intent(in) :: self
    real(defReal), intent(in)               :: a0
    real(defReal), intent(in)               :: r0
    real(defReal), dimension(2), intent(in) :: plane
    logical(defBool), intent(in)            :: backwards
    real(defReal), dimension(2)             :: dr
    real(defReal)                           :: z, topHeight, bottomHeight, length, f, &
                                               d1, d2, d3, r_fuel, r_gap, r_clad, disp
    integer(shortInt)                       :: idx

    ! Particle outside fuel length
    topHeight    = self % halflength + self % delta_z
    bottomHeight = -self % halflength
    if (self % axialSymmetry) bottomHeight = bottomHeight - self % delta_z
    if (a0 < bottomHeight .or. a0 > topHeight) return

    ! Find axial position
    z      = a0 - bottomHeight
    length = topHeight - bottomHeight
    idx = ceiling(self % N * z / length)
    f   =  ONE - (idx - self % N * z / length)

    ! Assign parameters
    if (backwards) then
      d1 = -(self % delta_r(idx + 1, 1) * f + self % delta_r(idx, 1) * (ONE - f))
      d2 = -(self % delta_r(idx + 1, 2) * f + self % delta_r(idx, 2) * (ONE - f))
      d3 = -(self % delta_r(idx + 1, 3) * f + self % delta_r(idx, 3) * (ONE - f))
      r_fuel = self % r_fuel - d1
      r_gap  = self % r_gap - d2
      r_clad = self % r_clad - d3
    else
      d1 = self % delta_r(idx + 1, 1) * f + self % delta_r(idx, 1) * (ONE - f)
      d2 = self % delta_r(idx + 1, 2) * f + self % delta_r(idx, 2) * (ONE - f)
      d3 = self % delta_r(idx + 1, 3) * f + self % delta_r(idx, 3) * (ONE - f)
      r_fuel = self % r_fuel
      r_gap  = self % r_gap
      r_clad = self % r_clad
    end if

    ! Calculate radial displacement
    if (r0 > r_clad) then
      disp = d3 * (r0 - self % r_outer) / (r_clad - self % r_outer)
    elseif (r0 > r_gap) then
      disp = d2 + (d3 - d2) * (r0 - r_gap) / (r_clad - r_gap)
    elseif (r0 > r_fuel) then
      disp = d1 + (d2 - d1) * (r0 - r_fuel) / (r_gap - r_fuel)
    else
      disp = d1 * r0 / r_fuel
    end if

    ! Project into correct coordinates
    dr = disp * plane / norm2(plane)

  end function radialDisplacement

  !!
  !! Cast field pointer to pinDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of pinDisplacementField
  !!   Pointer to source if source is pinDisplacementField type
  !!
  pure function pinDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)   :: source
    type(pinDisplacementField), pointer :: ptr

    select type (source)
      type is (pinDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function pinDisplacementField_TptrCast


end module pinDisplacementField_class
