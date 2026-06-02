module latDisplacementField_class

  use numPrecision
  use universalVariables
  use genericProcedures,       only : fatalError, numToChar, swap
  use dictionary_class,        only : dictionary
  use box_class,               only : box
  use materialMenu_mod,        only : mm_matIdx => matIdx
  use particle_class,          only : particle, particleState
  use coord_class,             only : coordList
  use field_inter,             only : field
  use displacementField_inter, only : displacementField
  use funcDisplacementField_class, only : funcDisplacementField

  implicit none
  private

  !!
  !! Public Pointer Cast
  !!
  public :: latDisplacementField_TptrCast

  !! Parameters
  integer(shortInt), parameter :: ALL_MATS = -1, &
                                  EXP_FUN  =  1, &
                                  LIN_FUN  =  2, &
                                  SIN_FUN  =  3, &
                                  FLAT_FUN =  4, &
                                  POLI_FUN =  5

  !!
  !! Helper type to store polymorphic instances of displacementFields
  !!
  type dispField
    type(funcDisplacementField) :: slot
  end type dispField

  !!
  !! Piecewise constant field constructed from a lattice-like grid.
  !! Values of the field are piecewise constant.
  !!
  !! Similar to a Cartesian lattice. Centre is placed at origin.
  !! Can include materials: values can be set on a coarse grid and differentiating
  !! between materials within a given grid cell.
  !! If applying the values uniformly to all materials, can use the keyword 'all',
  !! i.e., materials (all);
  !!
  !! Example dictionary:
  !!
  !! myField {
  !!   type latDisplacementField;
  !!   origin (x0 y0 z0);
  !!   shape (Nx Ny Nz);
  !!   pitch (Px Py Pz);
  !!   fields (....);
  !! }
  !!
  type, public, extends(displacementField) :: latDisplacementField
    private
    real(defReal), dimension(3)     :: pitch = ZERO
    integer(shortInt), dimension(3) :: sizeN = 0
    real(defReal), dimension(3)     :: corner = ZERO
    real(defReal), dimension(3)     :: a_bar  = ZERO
    type(box)                       :: outline
    type(dispField), dimension(:), allocatable :: fields

  contains

    ! Superclass procedures
    procedure :: init
    procedure :: kill
    procedure :: at
    procedure :: atP
    procedure :: backwards
    procedure :: getDelta

    ! Local procedure
    procedure, private :: getLocalID

  end type latDisplacementField

contains

  !!
  !! Initialisation
  !!
  subroutine init(self, dict)
    class(latDisplacementField), intent(inout)    :: self
    class(dictionary), intent(in)                 :: dict
    type(dictionary)                              :: tempDict
    integer(shortInt)                             :: N, i
    integer(shortInt), dimension(:), allocatable  :: tempI
    real(defReal), dimension(:), allocatable      :: temp
    real(defReal), dimension(3)                   :: origin
    character(nameLen), dimension(:), allocatable :: fieldNames
    character(100), parameter :: Here = 'init (latDisplacementField_class.f90)'

    ! Load pitch
    call dict % get(temp, 'pitch')
    N = size(temp)

    if (N /= 3) then
      call fatalError(Here, 'Pitch must have size 3. Has: '//numToChar(N))
    end if
    self % pitch = temp

    ! Load origin
    call dict % get(temp, 'origin')
    N = size(temp)

    if (N /= 3) then
      call fatalError(Here, 'Origin must have size 3. Has: '//numToChar(N))
    end if
    origin = temp

    ! Load Size
    call dict % get(tempI, 'shape')
    N = size(tempI)

    if (N /= 3) then
      call fatalError(Here, 'Shape must have size 3. Has: '//numToChar(N))
    else if (any(tempI < 0)) then
      call fatalError(Here, 'Shape contains -ve entries')
    end if
    self % sizeN = tempI

    ! Detect reduced Z dimension
    if (self % sizeN(3) == 0) then
      self % sizeN(3) = 1
      self % pitch(3) = TWO * INF
    end if

    ! Check X & Y for 0 size
    if (any( self % sizeN == 0)) call fatalError(Here, 'Shape in X and Y axis cannot be 0.')

    ! Check for invalid pitch
    if (any(self % pitch < 10 * SURF_TOL)) then
     call fatalError(Here, 'Pitch size must be larger than: '//numToChar( 10 * SURF_TOL))
   end if

    ! Calculate halfwidth and corner
    self % a_bar = self % pitch * HALF - SURF_TOL
    self % corner = origin -(self % sizeN * HALF * self % pitch)

    ! Build outline box
    call tempDict % init(4)
    call tempDict % store('type', 'box')
    call tempDict % store('id', 1)
    call tempDict % store('origin', origin)
    call tempDict % store('halfwidth', abs(self % corner - origin))
    call self % outline % init(tempDict)

    ! Construct fill array
    call dict % get(fieldNames, 'fields')

    ! Allocate space
    allocate(self % fields(size(fieldNames)))

    ! Build fields
    do i = 1, size(fieldNames)
      call self % fields(i) % slot % init(dict % getDictPtr(fieldNames(i)))
    end do

  end subroutine init

  !!
  !! Clean-up
  !!
  elemental subroutine kill(self)
    class(latDisplacementField), intent(inout) :: self
    integer(shortInt)                          :: i

    self % pitch = ZERO
    self % sizeN = 0
    self % corner = ZERO
    self % a_bar  = ZERO
    call self % outline % kill()

    ! Kill fields
    if (allocated(self % fields)) then
      do i = 1, size(self % fields)
        call self % fields(i) % slot % kill()
      end do
      deallocate(self % fields)
    end if

  end subroutine kill

  !!
  !! Get value of the field at the co-ordinate point
  !!
  !! See pieceConstantField for details
  !!
  function at(self, coords) result(val)
    class(latDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), dimension(3)             :: val
    integer(shortInt)                       :: localID

    localID = self % getLocalID(coords % lvl(1) % r, coords % lvl(1) % dir)
    if (localID == 0) then
      val = ZERO
      return
    end if

    val = self % fields(localID) % slot % at(coords)

  end function at

  !!
  !! Get value of the field at the particle's location
  !!
  !! See pieceConstantField for details
  !!
  function atP(self, p) result(val)
    class(latDisplacementField), intent(in) :: self
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
    class(latDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal), intent(in), optional     :: delta
    real(defReal), dimension(3)             :: val
    integer(shortInt)                       :: localID
    character(100), parameter :: Here = 'backwards (latDisplacementField_class.f90)'

    localID = self % getLocalID(coords % lvl(1) % r, coords % lvl(1) % dir)
    if (localID == 0) then
      val = ZERO
      return
    end if

    ! Evaluate function and check validity
    if (present(delta)) then
      val = self % fields(localID) % slot % backwards(coords, delta = delta)
    else
      val = self % fields(localID) % slot % backwards(coords)
    end if

  end function backwards

  !!
  !! Get value of delta
  !!
  function getDelta(self, coords) result(val)
    class(latDisplacementField), intent(in) :: self
    class(coordList), intent(in)            :: coords
    real(defReal)                           :: val
    integer(shortInt)                       :: localID

    localID = self % getLocalID(coords % lvl(1) % r, coords % lvl(1) % dir)
    if (localID == 0) then
      val = ZERO
      return
    end if

    val = self % fields(localID) % slot % getDelta(coords)

  end function getDelta

  !!
  !! Find the local integer ID in the field given position and direction
  !!
  pure function getLocalID(self, r, u) result(localID)
    class(latDisplacementField), intent(in) :: self
    real(defReal), dimension(3), intent(in) :: r
    real(defReal), dimension(3), intent(in) :: u
    integer(shortInt)                       :: localID
    real(defReal), dimension(3)             :: r_bar
    integer(shortInt), dimension(3)         :: ijk
    integer(shortInt)                       :: i, inc

    ijk = floor((r - self % corner) / self % pitch) + 1

    ! Get position wrt middle of the lattice cell
    r_bar = r - self % corner - ijk * self % pitch + HALF * self % pitch

    ! Check if position is within surface tolerance
    ! If it is, push it to next cell
    do i = 1, 3
      if (abs(r_bar(i)) > self % a_bar(i) .and. r_bar(i)*u(i) > ZERO) then

        ! Select increment. Ternary expression
        if (u(i) < ZERO) then
          inc = -1
        else
          inc = 1
        end if

        ijk(i) = ijk(i) + inc
      end if
    end do

    if (any(ijk <= 0 .or. ijk > self % sizeN)) then ! Point is outside lattice
      localID = 0

    else
      localID = ijk(1) + self % sizeN(1) * (ijk(2)-1 + self % sizeN(2) * (ijk(3)-1))

    end if

  end function getLocalID

  !!
  !! Cast field pointer to latDisplacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of latDisplacementField
  !!   Pointer to source if source is latDisplacementField type
  !!
  pure function latDisplacementField_TptrCast(source) result(ptr)
    class(field), pointer, intent(in)   :: source
    type(latDisplacementField), pointer :: ptr

    select type (source)
      type is (latDisplacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function latDisplacementField_TptrCast

end module latDisplacementField_class
