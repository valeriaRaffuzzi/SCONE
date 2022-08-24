module cellUniverse_class

  use numPrecision
  use universalVariables, only : UNDEF_MAT, NUDGE, INF
  use genericProcedures,  only : fatalError, numToChar
  use dictionary_class,   only : dictionary
  use coord_class,        only : coord
  use charMap_class,      only : charMap
  use surfaceShelf_class, only : surfaceShelf
  use cell_inter,         only : cell
  use cellShelf_class,    only : cellShelf
  use universe_inter,     only : universe, kill_super => kill, charToFill
  implicit none
  private

  !!
  !! Local helper class to group cell data
  !!
  !! Public Members:
  !!   idx     -> cellIdx of the cell in cellShelf
  !!   localID -> reference cell ID internal to a universe instance
  !!   ptr     -> Pointer to the cell
  !!
  type, private :: localCell
    integer(shortInt)    :: idx = 0
    integer(shortInt)    :: localID = 0
    class(cell), pointer :: ptr => null()
  end type localCell

  !!
  !! Representation of a universe via cells
  !!
  !! Each local cell in the universe correspondes to a cell given by an ID.
  !! An extra local cell is always defined inside the cellUniverse with UNDEF_MAT
  !! (undefined material) filling. If position is not in any user-defined cell, it is in this
  !! extra cell. Extra cell exists to enable plotting of geometry without fatalErrors.
  !!
  !! Sample Input Dictionary:
  !!   uni { type cellUniverse;
  !!         id 7;
  !!         # origin (2.0 0.0 0.0);    #
  !!         # rotation (23.0 0.0 0.0); #
  !!         cells ( 1 3 4);
  !!         fills (fuel void u<2>);
  !!         # combined {3 (2 5); }     #
  !!       }
  !!
  !! Note:
  !!   - Combining cells is equivalent to applying a union operator
  !!   - Local IDs are assigned in order as in definition. A localID corresponds to the location
  !!     in the cells array, except for combined cells. These are stored consecutively and have the same localID.
  !!     In the example above local id would map to following cell [localID: cellID] {1: 1, 2: 2, 2: 5, 4: 4, 5: UNDEF }
  !!   - Cell overlaps are allowed if inside the combined dictionary, but there is no check to find overlaps.
  !!
  !! Public Members:
  !!   cells -> Structure that stores cellIdx, localID and pointers to the cells
  !!
  !! Interface:
  !!   universe interface
  !!
  type, public, extends(universe) :: cellUniverse
    type(localCell), dimension(:), allocatable :: cells
  contains
    ! Superclass procedures
    procedure :: init
    procedure :: kill
    procedure :: findCell
    procedure :: distance
    procedure :: cross
    procedure :: cellOffset
  end type cellUniverse

contains

  !!
  !! Initialise Universe
  !!
  !! See universe_inter for details.
  !!
  subroutine init(self, fill, dict, cells, surfs, mats)
    class(cellUniverse), intent(inout)                        :: self
    integer(shortInt), dimension(:), allocatable, intent(out) :: fill
    class(dictionary), intent(in)                             :: dict
    type(cellShelf), intent(inout)                            :: cells
    type(surfaceShelf), intent(inout)                         :: surfs
    type(charMap), intent(in)                                 :: mats
    class(dictionary), pointer                                :: tempDict
    character(nameLen), dimension(:), allocatable             :: fillName, keys
    integer(shortInt), dimension(:), allocatable  :: cellTemp, fillTemp, cellTot, &
                                                     fillTot, localIDs, cellCombined
    real(defReal), dimension(:), allocatable      :: temp
    integer(shortInt)                             :: id, N, i, end, N_comb
    character(nameLen)                            :: cell
    character(100), parameter :: Here = 'init (cellUniverse_class.f90)'

    ! Load basic data
    call dict % get(id, 'id')
    if (id <= 0) call fatalError(Here, 'Universe ID must be +ve. Is: '//numToChar(id))
    call self % setId(id)

    ! Load origin
    if (dict % isPresent('origin')) then
      call dict % get(temp, 'origin')

      if (size(temp) /= 3) then
        call fatalError(Here, 'Origin must have size 3. Has: '//numToChar(size(temp)))
      end if
      call self % setTransform(origin=temp)

    end if

    ! Load rotation
    if (dict % isPresent('rotation')) then
      call dict % get(temp, 'rotation')

      if (size(temp) /= 3) then
        call fatalError(Here, '3 rotation angles must be given. Has only: '//numToChar(size(temp)))
      end if
      call self % setTransform(rotation=temp)
    end if

    ! Load cells by ID
    call dict % get(cellTemp, 'cells')
    N = size(cellTemp)

    ! Load fills
    call dict % get(fillName, 'fills')

    ! Convert fill characters into integer indexes
    allocate(fillTemp(N))
    do i = 1, N
      fillTemp(i) = charToFill(fillName(i), mats, Here)
    end do

    ! Check if there's any combined cell
    if (.not. dict % isPresent('combined')) then

      ! copy arrays to for name consistency
      fillTot = fillTemp
      cellTot = cellTemp
      ! Create localID array based on cell order
      allocate(localIDs(N))
      do i = 1, N
        localIDs(i) = i
      end do

    else

      N_comb = 0
      ! Get dictionary for combined cells
      tempDict => dict % getDictPtr('combined')
      call tempDict % keys(keys)
      ! Loop over cells to find the size of the final array, which includes combined cells
      do i = 1, N
        cell = numToChar(cellTemp(i))
        if (any(keys == cell)) then
          call tempDict % get(cellCombined, cell)
          N_comb = N_comb + size(cellCombined) - 1
        end if
      end do

      ! Allocate arrays to include the total number of cells and indexes
      allocate(cellTot(N + N_comb), fillTot(N + N_comb), localIDs(N + N_comb))

      ! Loop over cells to add the combined cells to the list of cells and indexes
      end = 1
      do i = 1, N
        cell = numToChar(cellTemp(i))
        if (any(keys == cell)) then
          call tempDict % get(cellCombined, cell)
          cellTot(end : end + size(cellCombined) -1) = cellCombined
          fillTot(end : end + size(cellCombined) -1) = fillTemp(i)
          localIDs(end : end + size(cellCombined) -1) = end
          ! Update location counter
          end = end + size(cellCombined)
        else
          cellTot(end) = cellTemp(i)
          fillTot(end) = fillTemp(i)
          localIDs(end) = end
          ! Update location counter
          end = end + 1
        end if
      end do

      ! Update total cell size
      N = N + N_comb

    end if

    allocate(self % cells(N))
    self % cells % idx = cellTot

    ! Load pointers, convert cell ID to IDX and save localIDs
    do i = 1, N
      ! Convert cell ID to IDX
      self % cells(i) % idx = cells % getIdx(self % cells(i) % idx)
      self % cells(i) % ptr => cells % getPtr(self % cells(i) % idx)
      self % cells(i) % localID = localIDs(i)
      ! Save the fill into cellShelf
      call cells % addFill(self % cells(i) % idx, fillTemp(i))
    end do

    ! Output array of fill indexes
    allocate(fill(N + 1))
    fill(1:N) = fillTot
    fill(N + 1) = UNDEF_MAT

  end subroutine init

  !!
  !! Find local cell ID given a point
  !!
  !! See universe_inter for details.
  !!
  subroutine findCell(self, localID, cellIdx, r, u)
    class(cellUniverse), intent(inout)      :: self
    integer(shortInt), intent(out)          :: localID
    integer(shortInt), intent(out)          :: cellIdx
    real(defReal), dimension(3), intent(in) :: r
    real(defReal), dimension(3), intent(in) :: u
    integer(shortInt)                       :: i

    ! Search all cells
    do i = 1, size(self % cells)

      if (self % cells(i) % ptr % inside(r, u)) then
        cellIdx = self % cells(i) % idx
        localID = self % cells(i) % localID
        return
      end if

    end do

    ! If not found return undefined cell
    localID = size(self % cells) + 1
    cellIdx = 0

  end subroutine findCell

  !!
  !! Return distance to the next boundary between local cells in the universe
  !!
  !! See universe_inter for details.
  !!
  !! Errors:
  !!   fatalError if in UNDEFINED cell
  !!
  subroutine distance(self, d, surfIdx, coords)
    class(cellUniverse), intent(inout) :: self
    real(defReal), intent(out)         :: d
    integer(shortInt), intent(out)     :: surfIdx
    type(coord), intent(in)            :: coords
    integer(shortInt)                  :: localID, cellIdx, i
    character(100), parameter :: Here = 'distance (cellUniverse_class.f90)'

    localID = coords % localID
    cellIdx = coords % cellIdx

    if (localID > size(self % cells)) then
      call fatalError(Here, 'Particle is in undefined local cell. Local ID: '//numToChar(localID))
    end if

    ! Search all cells to find the one with matching localID and unique cell IDX
    do i = 1, size(self % cells)
        if (self % cells(i) % localID == localID .and. self % cells(i) % idx == cellIdx) then
        ! Calculate distance
        call self % cells(localID) % ptr % distance(d, surfIdx, coords % r, coords % dir)
        return
      end if

    end do

    ! If not found return infinite distance and undefined surface
    d = INF
    surfIdx = 0

  end subroutine distance

  !!
  !! Cross between local cells
  !!
  !! See universe_inter for details.
  !!
  !! Note: Introduces extra movement to the particle to push it over boundary
  !!   for more efficent search. Distance is NUGDE.
  !!
  subroutine cross(self, coords, surfIdx)
    class(cellUniverse), intent(inout) :: self
    type(coord), intent(inout)         :: coords
    integer(shortInt), intent(in)      :: surfIdx

    ! NUDGE position slightly forward to escape surface tolerance
    ! and avoid calculating normal and extra dot-products
    coords % r = coords % r + coords % dir * NUDGE

    ! Find cell
    ! TODO: Some cell neighbout list
    call self % findCell(coords % localID, &
                         coords % cellIdx, &
                         coords % r,       &
                         coords % dir)

  end subroutine cross

  !!
  !! Return offset for the current cell
  !!
  !! See universe_inter for details.
  !!
  function cellOffset(self, coords) result (offset)
    class(cellUniverse), intent(in) :: self
    type(coord), intent(in)         :: coords
    real(defReal), dimension(3)     :: offset

    ! There is no cell offset
    offset = ZERO

  end function cellOffset

  !!
  !! Return to uninitialised state
  !!
  elemental subroutine kill(self)
    class(cellUniverse), intent(inout) :: self

    ! SUperclass
    call kill_super(self)

    ! Local
    if(allocated(self % cells)) deallocate(self % cells)

  end subroutine kill


end module cellUniverse_class
