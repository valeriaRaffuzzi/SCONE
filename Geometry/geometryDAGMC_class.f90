module geometryDAGMC_class

  use numPrecision
  use universalVariables
  use genericProcedures,  only : fatalError, numToChar
  use coord_class,        only : coordList, coord
  use dictionary_class,   only : dictionary
  use charMap_class,      only : charMap
  use geometry_inter,     only : geometry, distCache
  use csg_class,          only : csg
  use universe_inter,     only : universe
  use surface_inter,      only : surface

  ! Nuclear Data
  use materialMenu_mod,   only : nMat

  implicit none
  private

  !!
  !! DAGMC geometry
  !!
  !! Interface:
  !!   Geometry Interface
  !!
  type, public, extends(geometry) :: geometryDAGMC
    type(csg) :: geom

  contains
    ! Superclass procedures
    procedure :: init
    procedure :: kill
    procedure :: placeCoord
    procedure :: whatIsAt
    procedure :: bounds
    procedure :: move_noCache
    procedure :: move_withCache
    procedure :: moveGlobal
    procedure :: teleport
    procedure :: activeMats

    ! Private procedures
    procedure, private :: diveToMat
    procedure, private :: closestDist
    procedure, private :: closestDist_cache
  end type geometryDAGMC

contains

  !!
  !! Initialise geometry
  !!
  !! See geometry_inter for details
  !!
  subroutine init(self, dict, mats, silent)
    class(geometryDAGMC), intent(inout)    :: self
    class(dictionary), intent(in)          :: dict
    type(charMap), intent(in)              :: mats
    logical(defBool), optional, intent(in) :: silent
  end subroutine init

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(geometryDAGMC), intent(inout) :: self
  end subroutine kill

  !!
  !! Place coordinate list into geometry
  !!
  !! See geometry_inter for details
  !!
  subroutine placeCoord(self, coords)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
  end subroutine placeCoord

  !!
  !! Find material and unique cell at a given location
  !!
  !! See geometry_inter for details
  !!
  subroutine whatIsAt(self, matIdx, uniqueID, r, u)
    class(geometryDAGMC), intent(in)                  :: self
    integer(shortInt), intent(out)                    :: matIdx
    integer(shortInt), intent(out)                    :: uniqueID
    real(defReal), dimension(3), intent(in)           :: r
    real(defReal), dimension(3), optional, intent(in) :: u
  end subroutine whatIsAt

  !!
  !! Return Axis Aligned Bounding Box encompassing the geometry
  !!
  !! See geometry_inter for details
  !!
  function bounds(self)
    class(geometryDAGMC), intent(in) :: self
    real(defReal), dimension(6)      :: bounds
  end function bounds

  !!
  !! Given coordinates placed in the geometry move point through the geometry
  !!
  !! See geometry_inter for details
  !!
  subroutine move_noCache(self, coords, maxDist, event)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
    real(defReal), intent(inout)     :: maxDist
    integer(shortInt), intent(out)   :: event
  end subroutine move_noCache

  !!
  !! Given coordinates placed in the geometry move point through the geometry
  !!
  !! See geometry_inter for details
  !!
  subroutine move_withCache(self, coords, maxDist, event, cache)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
    real(defReal), intent(inout)     :: maxDist
    integer(shortInt), intent(out)   :: event
    type(distCache), intent(inout)   :: cache
  end subroutine move_withCache

  !!
  !! Move a particle in the top (global) level in the geometry
  !!
  !! See geometry_inter for details
  !!
  subroutine moveGlobal(self, coords, maxDist, event)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
    real(defReal), intent(inout)     :: maxDist
    integer(shortInt), intent(out)   :: event
  end subroutine moveGlobal

  !!
  !! Move a particle in the top level without stopping
  !!
  !! See geometry_inter for details
  !!
  subroutine teleport(self, coords, dist)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
    real(defReal), intent(in)        :: dist
  end subroutine teleport

  !!
  !! Returns the list of active materials used in the geometry
  !!
  !! See geometry_inter for details
  !!
  function activeMats(self) result(matList)
    class(geometryDAGMC), intent(in)             :: self
    integer(shortInt), dimension(:), allocatable :: matList
  end function activeMats

  !!
  !! Descend down the geometry structure untill material is reached
  !!
  !! Requires strting level to be specified.
  !! It is private procedure common to all movment types in geometry.
  !!
  !! Args:
  !!   coords [inout] -> CoordList of a particle. Assume thet coords are already valid for all
  !!     levels above and including start
  !!   start [in] -> Starting level for meterial search
  !!
  !! Errors:
  !!   fatalError if material cell is not found untill maximum nesting is reached
  !!
  subroutine diveToMat(self, coords, start)
    class(geometryDAGMC), intent(in) :: self
    type(coordList), intent(inout)   :: coords
    integer(shortInt), intent(in)    :: start
    integer(shortInt)                :: rootID, localID, fill, id, i
    class(universe), pointer         :: uni
    real(defReal), dimension(3)      :: offset
    character(100), parameter :: Here = 'diveToMat (geometryDAGMC_class.f90)'

  end subroutine diveToMat

  !!
  !! Return distance to the closest surface
  !!
  !! Searches through all geometry levels. In addition to distance return level
  !! and surfIdx for crossing surface
  !!
  !! Args:
  !!   dist [out]    -> Value of closest distance
  !!   surfIdx [out] -> Surface index for the crossing returned from the universe
  !!   lvl     [out] -> Level at which crossing is closest
  !!   coords [in]   -> Current coordinates of a particle
  !!
  subroutine closestDist(self, dist, surfIdx, lvl, coords)
    class(geometryDAGMC), intent(in) :: self
    real(defReal), intent(out)       :: dist
    integer(shortInt), intent(out)   :: surfIdx
    integer(shortInt), intent(out)   :: lvl
    type(coordList), intent(in)      :: coords
    integer(shortInt)                :: l, test_idx
    real(defReal)                    :: test_dist
    class(universe), pointer         :: uni

    dist = INF
    surfIdx = 0
    lvl = 0

  end subroutine closestDist

  !!
  !! Return distance to the closest surface
  !!
  !! Searches through all geometry levels. In addition to distance return level
  !! and surfIdx for crossing surface
  !!
  !! Args:
  !!   dist [out]    -> Value of closest distance
  !!   surfIdx [out] -> Surface index for the crossing returned from the universe
  !!   lvl     [out] -> Level at which crossing is closest
  !!   coords [in]   -> Current coordinates of a particle
  !!   cache [inout] -> Distance cache. Use valid distances from cache. Put calculated
  !!     distances on the cache.
  !!
  subroutine closestDist_cache(self, dist, surfIdx, lvl, coords, cache)
    class(geometryDAGMC), intent(in) :: self
    real(defReal), intent(out)       :: dist
    integer(shortInt), intent(out)   :: surfIdx
    integer(shortInt), intent(out)   :: lvl
    type(coordList), intent(in)      :: coords
    type(distCache), intent(inout)   :: cache
    integer(shortInt)                :: l, test_idx
    real(defReal)                    :: test_dist
    class(universe), pointer         :: uni

    dist = INF
    surfIdx = 0
    lvl = 0

  end subroutine closestDist_cache

end module geometryDAGMC_class
