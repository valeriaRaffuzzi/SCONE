module familyMap_class

    use numPrecision
    use universalVariables
    use preDefEnergyGrids
    use genericProcedures,   only : fatalError
    use dictionary_class,    only : dictionary
    use grid_class,          only : grid
    use particle_class,      only : particleState
    use outputFile_class,    only : outputFile
    use tallyMap1D_inter,    only : tallyMap1D, kill_super => kill
  
    implicit none
    private

    !!
    !! Particle types paramethers
    !!
    integer(shortInt), parameter, public :: P_NEUTRON   = 1, &
                                            P_PHOTON    = 2, &
                                            P_PRECURSOR = 3

    interface familyMap
      module procedure familyMap_fromDict
    end interface
  
    !!
    !! Map that partition into a given number of precursor families
    !! Returns idx = 0 for MG particles
    !!
    !!
    !! Private Members:
    !!
    !!   N -> Integer number of bins in the map
    !!
    !! Interface:
    !!   tallyMap interface
    !!   build -> build instance of spaceMap without dictionary
    !!
    !! NOTE: Behaviour of points exactly at the boundary between two bins is undefined.
    !!       They can be in either of two bins.
    !!
    !! Sample Dictionary Input:
    !!   structMap {
    !!     type familyMap;
    !!     N 6;
    !!   }
    !!
    type, public,extends(tallyMap1D) :: familyMap
      private
      integer(shortInt) :: N = 0
  
    contains
      ! Superclass interface implementaction
      procedure  :: init
      procedure  :: bins
      procedure  :: map
      procedure  :: getAxisName
      procedure  :: print
      procedure  :: kill
  
    end type familyMap
  
  contains
  
  !!
  !! Return instance of familyMap from dictionary
  !!
  !! Args:
  !!   dict[in] -> input dictionary for the map
  !!
  !! Result:
  !!   Initialised familyMap instance
  !!
  !! Errors:
  !!   See init procedure.
  !!
  function familyMap_fromDict(dict) result(new)
    class(dictionary), intent(in)          :: dict
    type(familyMap)                        :: new

    call new % init(dict)

  end function familyMap_fromDict


    !!
    !! Initialise from dictionary
    !!
    !! See tallyMap for specification
    !!
    subroutine init(self, dict)
      class(familyMap), intent(inout)        :: self
      class(dictionary), intent(in)          :: dict
      character(100), parameter     :: Here = 'init (familyMap_class.f90)'
  
      call dict % getOrDefault(self % N, 'N', 1)
  
    end subroutine init
  
    !!
    !! Return total number of bins in this division along dimension D
    !! For D=0 return all bins
    !!
    !! See tallyMap for specification
    !!
    elemental function bins(self, D) result(N)
      class(familyMap), intent(in)    :: self
      integer(shortInt), intent(in)   :: D
      integer(shortInt)               :: N
  
      if (D == 1 .or. D == 0) then
        N = self % N
      else
        N = 0
      end if
  
    end function bins
  
    !!
    !! Map particle to a single bin. Return 0 for particle out of division or MG
    !!
    !! See tallyMap for specification
    !!
    !! NOTE:
    !!   Returns idx = 0 for MG particles
    !!
    elemental function map(self,state) result(idx)
      class(familyMap), intent(in)     :: self
      class(particleState), intent(in) :: state
      integer(shortInt)                :: idx
      character(100), parameter :: Here = 'map (familyMap_class.f90)'

      if (state % type == P_PRECURSOR) then
        ! Find position on the grid
        if (self % N == 1) then
            idx = 1
        else if (state % F <= self % N) then
            idx = state % F
        else
            idx = 0
        end if
      else
        idx = 0
      end if
    end function map
  
    !!
    !! Return string that describes variable used to divide event space
    !!
    !! See tallyMap for specification
    !!
    function getAxisName(self) result(name)
      class(familyMap), intent(in) :: self
      character(nameLen)              :: name
  
      name = 'Family'
  
    end function getAxisName
  
    !!
    !! Add information about division axis to the output file
    !!
    !! See tallyMap for specification
    !!
    subroutine print(self,out)
      class(familyMap), intent(in)     :: self
      class(outputFile), intent(inout) :: out
  
      !! Do nothing

    end subroutine print
  
    !!
    !! Return to uninitialised state
    !!
    elemental subroutine kill(self)
      class(familyMap), intent(inout) :: self
  
      call kill_super(self)
  
      ! Kill local
      self % N = 0
  
    end subroutine kill
  
  end module familyMap_class
  