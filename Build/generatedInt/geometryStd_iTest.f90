module geometryStd_iTest

  use numPrecision
  use universalVariables
  use dictionary_class,  only : dictionary
  use charMap_class,     only : charMap
  use dictParser_func,   only : fileToDict
  use coord_class,       only : coordList
  use geometryStd_class, only : geometryStd
  use pFUnit_mod

  implicit none


contains

  !!
  !! Geometry integration test -> Simple 2x2 lattice
  !!
!@Test
  subroutine test_lattice_geom()
    type(geometryStd)           :: geom
    character(*), parameter     :: path = './IntegrationTestFiles/Geometry/test_lat'
    type(charMap)               :: mats
    integer(shortInt)           :: i, idx, matIdx, uniqueID, event
    type(dictionary)            :: dict
    real(defReal), dimension(3) :: r, u, r_ref, u_ref
    type(dictionary),pointer    :: tempDict
    character(nameLen)          :: name
    type(coordList)             :: coords
    character(nameLen), dimension(:), allocatable :: keys
    integer(shortInt), dimension(10,10)           :: img
    real(defReal), dimension(6)                   :: aabb
    real(defReal)                                 :: maxDist
    real(defReal), parameter :: TOL = 1.0E-7_defReal

    ! Load dictionary
    call fileToDict(dict, path)

    ! Load materials
    tempDict => dict % getDictPtr('nuclearData')
    tempDict => tempDict % getDictPtr('materials')
    call tempDict % keys(keys, 'dict')
    do i = 1, size(keys)
      call mats % add(keys(i), i)
    end do

    ! Build geometry
    call geom % init(dict, mats, silent=.true.)

    ! Get material at few locations
    name = 'water'
    idx = mats % get(name)
    call geom % whatIsAt(matIdx, uniqueID, [ZERO, ZERO, ZERO])
#line 55 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 55) )
  if (anyExceptions()) return
#line 56 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    name = 'mox43'
    idx = mats % get(name)
    r = [0.63_defReal, -0.09_defReal, 0.0_defReal]
    u = [ZERO, -ONE, ZERO]
    call geom % whatIsAt(matIdx, uniqueID, r, u)
#line 62 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 62) )
  if (anyExceptions()) return
#line 63 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Place coordinates
    r = [0.1_defReal, 0.1_defReal, 0.0_defReal]
    u = [ZERO, ZERO, ONE]
    call coords % init(r, u)
    call geom % placeCoord(coords)

    ! Verify positions
#line 71 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 71) )
  if (anyExceptions()) return
#line 72 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 72 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r, coords % lvl(2) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 72) )
  if (anyExceptions()) return
#line 73 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 73 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r - [0.63_defReal, 0.63_defReal, 0.0_defReal], coords % lvl(3) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 73) )
  if (anyExceptions()) return
#line 74 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Verify directions
#line 76 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 76) )
  if (anyExceptions()) return
#line 77 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 77 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u, coords % lvl(2) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 77) )
  if (anyExceptions()) return
#line 78 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 78 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u, coords % lvl(3) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 78) )
  if (anyExceptions()) return
#line 79 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Slice plot -> Material
    call geom % slicePlot(img, [ZERO, ZERO, ZERO], 'z', 'material')

    ! Verify some pixels
    name = 'water'
    idx = mats % get(name)
#line 86 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, img(1, 1), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 86) )
  if (anyExceptions()) return
#line 87 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 87 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, img(2, 6), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 87) )
  if (anyExceptions()) return
#line 88 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    name = 'mox43'
    idx = mats % get(name)
#line 91 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, img(3, 7), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 91) )
  if (anyExceptions()) return
#line 92 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    name = 'uox'
    idx = mats % get(name)
#line 95 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, img(3, 3), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 95) )
  if (anyExceptions()) return
#line 96 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Slice plot -> UniqueID
    r = [-0.63_defReal, -0.63_defReal, 0.0_defReal]
    call geom % slicePlot(img, r, 'z', 'uniqueID', [1.26_defReal, 1.26_defReal])

    ! Verify some pixels
    ! Note that this test depends on universe leyout order in gromGraph
    ! If it changes this test fill fail
#line 104 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(2, img(5,5), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 104) )
  if (anyExceptions()) return
#line 105 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 105 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(3, img(1,1), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 105) )
  if (anyExceptions()) return
#line 106 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Verify bounds
    aabb = [-1.26_defReal, -1.26_defReal, 0.0_defReal, 1.26_defReal, 1.26_defReal, 0.0_defReal]
#line 109 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(aabb, geom % bounds(), TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 109) )
  if (anyExceptions()) return
#line 110 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    !*** Test teleport movement
    r = [ZERO, ZERO, ZERO]
    u = [-ONE, -TWO, ZERO]
    u = u/norm2(u)
    call coords % init(r, u)

    call geom % teleport(coords, 3.0_defReal)

    r_ref = [-1.1783592_defReal, -0.1632816_defReal, ZERO]
    u_ref = [ONE, -TWO, ZERO]
    u_ref = u_ref / norm2(u_ref)
    name = 'water'
    idx = mats % get(name)

#line 125 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 125) )
  if (anyExceptions()) return
#line 126 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 126 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 126) )
  if (anyExceptions()) return
#line 127 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 127 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 127) )
  if (anyExceptions()) return
#line 128 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    !*** Test global movement
    r = [ZERO, ZERO, ZERO]
    u = [ZERO, -ONE, ZERO]
    call coords % init(r, u)

    ! Collosion movement
    maxDist = 1.0_defReal
    call geom % moveGlobal(coords, maxDist, event)

    r_ref = [ZERO, -1.0_defReal, ZERO]
    u_ref = u
    name = 'water'
    idx = mats % get(name)

#line 143 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 143) )
  if (anyExceptions()) return
#line 144 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 144 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 144) )
  if (anyExceptions()) return
#line 145 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 145 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(COLL_EV, event, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 145) )
  if (anyExceptions()) return
#line 146 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 146 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 146) )
  if (anyExceptions()) return
#line 147 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 147 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(1.0_defReal, maxDist, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 147) )
  if (anyExceptions()) return
#line 148 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Boundary Hit
    maxDist = 1.0_defReal
    call geom % moveGlobal(coords, maxDist, event)

    r_ref = [ZERO, 1.26_defReal, ZERO]
    u_ref = u_ref
    name = 'water'
    idx = mats % get(name)

#line 158 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 158) )
  if (anyExceptions()) return
#line 159 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 159 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 159) )
  if (anyExceptions()) return
#line 160 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 160 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(BOUNDARY_EV, event, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 160) )
  if (anyExceptions()) return
#line 161 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 161 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 161) )
  if (anyExceptions()) return
#line 162 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 162 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(0.26_defReal, maxDist, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 162) )
  if (anyExceptions()) return
#line 163 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    !*** Normal Movment (easy case)
    r = [-0.63_defReal, -0.63_defReal, 0.0_defReal]
    u = [ZERO, -ONE, ZERO]
    call coords % init(r, u)
    call geom % placeCoord(coords)

    ! Local cell crossing
    maxDist = 1.0_defReal
    call geom % move(coords, maxDist, event)

    r_ref = [-0.63_defReal, -1.13_defReal, ZERO]
    u_ref = u_ref
    name = 'water'
    idx = mats % get(name)

#line 179 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 179) )
  if (anyExceptions()) return
#line 180 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 180 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 180) )
  if (anyExceptions()) return
#line 181 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 181 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(CROSS_EV, event, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 181) )
  if (anyExceptions()) return
#line 182 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 182 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 182) )
  if (anyExceptions()) return
#line 183 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 183 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(0.5_defReal, maxDist, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 183) )
  if (anyExceptions()) return
#line 184 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Boundary Hit
    maxDist = 1.0_defReal
    call geom % move(coords, maxDist, event)

    r_ref = [-0.63_defReal, 1.26_defReal, ZERO]
    u_ref = u_ref
    name = 'water'
    idx = mats % get(name)

#line 194 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 194) )
  if (anyExceptions()) return
#line 195 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 195 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 195) )
  if (anyExceptions()) return
#line 196 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 196 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(BOUNDARY_EV, event, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 196) )
  if (anyExceptions()) return
#line 197 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 197 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 197) )
  if (anyExceptions()) return
#line 198 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 198 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(0.13_defReal, maxDist, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 198) )
  if (anyExceptions()) return
#line 199 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Collision
    maxDist = 0.08_defReal
    call geom % move(coords, maxDist, event)

    r_ref = [-0.63_defReal, 1.18_defReal, ZERO]
    u_ref = u_ref
    name = 'water'
    idx = mats % get(name)

#line 209 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(r_ref, coords % lvl(1) % r, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 209) )
  if (anyExceptions()) return
#line 210 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 210 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(u_ref, coords % lvl(1) % dir, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 210) )
  if (anyExceptions()) return
#line 211 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 211 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(COLL_EV, event, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 211) )
  if (anyExceptions()) return
#line 212 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 212 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idx, coords % matIdx, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 212) )
  if (anyExceptions()) return
#line 213 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 213 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(0.08_defReal, maxDist, TOL, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 213) )
  if (anyExceptions()) return
#line 214 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Kill geometry
    call geom % kill()

  end subroutine test_lattice_geom

  !!
  !! Test geometry with tilted cylinder
  !!
!@Test
  subroutine test_tilted_cylinder()
    type(geometryStd)           :: geom
    character(*), parameter     :: path = './IntegrationTestFiles/Geometry/test_cyl'
    type(charMap)               :: mats
    integer(shortInt)           :: idxW, idxF, i
    type(dictionary)            :: dict
    type(dictionary),pointer    :: tempDict
    character(nameLen)          :: name
    character(nameLen), dimension(:), allocatable :: keys
    integer(shortInt), dimension(20,20)    :: img
    integer(shortInt), dimension(20,20,20) :: img3
    real(defReal), dimension(3)          :: r

    ! Load dictionary
    call fileToDict(dict, path)

    ! Load materials
    tempDict => dict % getDictPtr('nuclearData')
    tempDict => tempDict % getDictPtr('materials')
    call tempDict % keys(keys, 'dict')
    do i = 1, size(keys)
      call mats % add(keys(i), i)
    end do

    ! Build geometry
    call geom % init(dict, mats, silent=.true.)

    ! Get fuel and water index
    name = 'water'
    idxW = mats % get(name)

    name = 'mox43'
    idxF = mats % get(name)

    !*** Test slice normal to x & y
    ! X-axis at 1.0
    r = [1.0_defReal, 0.0_defReal, 0.0_defReal]
    call geom % slicePlot(img, r, 'x', 'material')

    ! Test some pixels
#line 264 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxW, img(8, 11), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 264) )
  if (anyExceptions()) return
#line 265 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 265 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxW, img(17, 3), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 265) )
  if (anyExceptions()) return
#line 266 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 266 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxF, img(10, 10), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 266) )
  if (anyExceptions()) return
#line 267 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 267 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxF, img(18, 1), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 267) )
  if (anyExceptions()) return
#line 268 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Y-axis at 3.0
    r = [0.0_defReal, 3.0_defReal, 0.0_defReal]
    call geom % slicePlot(img, r, 'y', 'material')

#line 273 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxW, img(15, 1), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 273) )
  if (anyExceptions()) return
#line 274 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 274 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxW, img(13, 4), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 274) )
  if (anyExceptions()) return
#line 275 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 275 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxF, img(13, 3), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 275) )
  if (anyExceptions()) return
#line 276 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
#line 276 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxF, img(14, 2), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 276) )
  if (anyExceptions()) return
#line 277 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    !*** Test voxel plot
    ! Full plot
    ! Value of r is irrelevant
    call geom % voxelPlot(img3, r, 'material')

    ! Checksome against 2D plot
    r = [0.0_defReal, 2.75_defReal, 0.0_defReal]
    call geom % slicePlot(img, r, 'y', 'material')

#line 287 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(img, img3(:,16,:), &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 287) )
  if (anyExceptions()) return
#line 288 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

    ! Small box all inside fuel
    r = [ 1.0_defReal, 0.0_defReal, 0.0_defReal]
    call geom % voxelPlot(img3, r, 'material', [0.5_defReal, 0.5_defReal, 0.5_defReal])

#line 293 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"
  call assertEqual(idxF, img3, &
 & location=SourceLocation( &
 & 'geometryStd_iTest.f90', &
 & 293) )
  if (anyExceptions()) return
#line 294 "/home/mskrette/SCONE_cambridge_fork/SCONE/Geometry/Tests/geometryStd_iTest.f90"

  end subroutine test_tilted_cylinder


end module geometryStd_iTest

module WrapgeometryStd_iTest
   use pFUnit_mod
   use geometryStd_iTest
   implicit none
   private

contains


end module WrapgeometryStd_iTest

function geometryStd_iTest_suite() result(suite)
   use pFUnit_mod
   use geometryStd_iTest
   use WrapgeometryStd_iTest
   type (TestSuite) :: suite

   suite = newTestSuite('geometryStd_iTest_suite')

   call suite%addTest(newTestMethod('test_lattice_geom', test_lattice_geom))

   call suite%addTest(newTestMethod('test_tilted_cylinder', test_tilted_cylinder))


end function geometryStd_iTest_suite

