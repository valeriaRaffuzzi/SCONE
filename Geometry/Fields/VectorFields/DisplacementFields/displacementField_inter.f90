module displacementField_inter

  use numPrecision
  use field_inter,       only : field
  use vectorField_inter, only : vectorField
  use coord_class,       only : coordList
  use particle_class,    only : particleState
  use tallyMap_inter,    only : tallyMap

  implicit none
  private

  !!
  !! Public Pointer Cast
  !!
  public :: displacementField_CptrCast

  !!
  !! Simple Real Vector Field for geometry deformations
  !!
  !! Interface:
  !!   vectorField interface
  !!   backwards
  !!
  type, public, abstract, extends(vectorField) :: displacementField
    class(tallyMap), allocatable :: map

  contains

    procedure :: inDomain
    procedure(getDelta), deferred     :: getDelta
    procedure(backwards), deferred    :: backwards

  end type displacementField

  abstract interface

    !!
    !! Get value of delta
    !!
    function getDelta(self, coords) result(val)
      import :: displacementField, coordList, defReal
      class(displacementField), intent(in) :: self
      class(coordList), intent(in)         :: coords
      real(defReal)                        :: val
    end function getDelta

    !!
    !! Get value of the vector field at the co-ordinate point
    !!
    !! Args:
    !!   coords [in] -> Coordinates of the position in the geometry
    !!
    !! Result:
    !!   Size 3 vector of real values.
    !!
    function backwards(self, coords, delta) result(val)
      import :: displacementField, coordList, defReal
      class(displacementField), intent(in) :: self
      class(coordList), intent(in)         :: coords
      real(defReal), intent(in), optional  :: delta
      real(defReal), dimension(3)          :: val
    end function backwards

  end interface

contains

  !!
  !! Check if a set of coordinates is in the deformation domain, i.e., if a
  !! map is allocated
  !!
  function inDomain(self, coords) result(isIt)
    class(displacementField), intent(in) :: self
    class(coordList), intent(in)         :: coords
    type(particleState)                  :: state
    logical(defBool)                     :: isIt

    isIt = .true.
    if (allocated(self % map)) then
      state % r = coords % lvl(1) % r
      if (self % map % map(state) == 0) isIt = .false.
    end if

  end function

  !!
  !! Cast field pointer to displacementField pointer
  !!
  !! Args:
  !!   source [in] -> source pointer of class field
  !!
  !! Result:
  !!   Null is source is not of displacementField
  !!   Pointer to source if source is displacementField class
  !!
  pure function displacementField_CptrCast(source) result(ptr)
    class(field), pointer, intent(in) :: source
    class(displacementField), pointer :: ptr

    select type (source)
    class is (displacementField)
        ptr => source

      class default
        ptr => null()
    end select

  end function displacementField_CptrCast

end module displacementField_inter
