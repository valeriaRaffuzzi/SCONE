module precursorResponse_class

  use numPrecision
  use dictionary_class,    only : dictionary
  use particle_class,      only : particle, P_PRECURSOR
  use tallyResponse_inter, only : tallyResponse

  ! Nuclear Data interface
  use nuclearDatabase_inter, only : nuclearDatabase

  implicit none
  private

  !!
  !! tallyResponse to score precursor contribution
  !!
  !! Always returns ONE if type is precursor
  !!
  !! Interface:
  !!   tallyResponse Interface
  !!
  type, public,extends(tallyResponse) :: precursorResponse
    private
  contains
    procedure :: init
    procedure :: get
    procedure :: kill
  end type precursorResponse

contains

  !!
  !! Initialise Response from dictionary
  !!
  !! See tallyResponse_inter for details
  !!
  subroutine init(self, dict)
    class(precursorResponse), intent(inout) :: self
    class(dictionary), intent(in)      :: dict

    ! Do nothing

  end subroutine init

  !!
  !! Get 1.0 (Response to score precursor)
  !!
  !! See tallyResponse_inter for details
  !!
  function get(self, p, xsData) result(val)
    class(precursorResponse), intent(in)    :: self
    class(particle), intent(in)           :: p
    class(nuclearDatabase), intent(inout) :: xsData
    real(defReal)                         :: val

    if (p % type == P_PRECURSOR) then
      val = ONE
    else
      val = ZERO
    end if

  end function get

  !!
  !! Return to uninitialised State
  !!
  elemental subroutine kill(self)
    class(precursorResponse), intent(inout) :: self

    ! Do nothing for nothing can be done

  end subroutine kill

end module precursorResponse_class
