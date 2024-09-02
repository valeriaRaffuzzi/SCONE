module baseMgNeutronDatabase_iTest

  use numPrecision
  use endfConstants
  use pFUnit_mod
  use universalVariables
  use dictionary_class,   only : dictionary
  use dictParser_func,    only : charToDict
  use particle_class,     only : particle

  ! Nuclear Data Objects & Interfaces
  use baseMgNeutronDatabase_class, only : baseMgNeutronDatabase, baseMgNeutronDatabase_CptrCast, &
                                          baseMgNeutronDatabase_TptrCast
  use baseMgNeutronMaterial_class, only : baseMgNeutronMaterial, baseMgNeutronMaterial_CptrCast, &
                                          baseMgNeutronMaterial_TptrCast
  use fissionMG_class,             only : fissionMG, fissionMG_TptrCast
  use multiScatterMG_class,        only : multiScatterMG, multiScatterMG_CptrCast, &
                                          multiScatterMG_TptrCast
  use multiScatterP1MG_class,      only : multiScatterP1MG, multiScatterP1MG_TptrCast
  use materialMenu_mod,            only : mm_init => init, mm_kill => kill
  use nuclearDatabase_inter,       only : nuclearDatabase
  use materialHandle_inter,        only : materialHandle
  use nuclideHandle_inter,         only : nuclideHandle
  use neutronXsPackages_class,     only : neutronMacroXSs
  use reactionHandle_inter,        only : reactionHandle



  implicit none

  ! Material definitions
  character(*),parameter :: MAT_INPUT_STR = "   &
  mat1 { temp 273;                              &
         composition {                          &
         1001.03 5.028E-02;                     &
         8016.03 2.505E-02;                     &
         }                                      &
         xsFile ./IntegrationTestFiles/mgMat1;  &
       }                                        &
  mat2  { temp 1;                               &
          composition {                         &
          92233.03 2.286E-02;                   &
          8016.03  4.572E-02;                   &
          }                                     &
          xsFile ./IntegrationTestFiles/mgMat2; &
        }"


contains

  !!
  !! Monster test to build and verify data in baseMgNeutronDatabase with P0 scattering
  !!
!@Test
  subroutine testBaseMgNeutronDatabaseWithP0()
    type(baseMgNeutronDatabase), target  :: database
    class(nuclearDatabase), pointer      :: data_ptr
    type(dictionary)                     :: databaseDef
    type(dictionary)                     :: matMenuDict
    type(particle)                       :: p
    type(neutronMacroXSs)                :: xss
    type(baseMgNeutronMaterial),pointer  :: mat
    class(baseMgNeutronMaterial),pointer :: matClass
    class(reactionHandle), pointer       :: reac
    real(defReal),parameter :: TOL = 1.0E-6_defReal


    data_ptr => database

    ! Load materialMenu
    call charToDict(matMenuDict, MAT_INPUT_STR)
    call mm_init(matMenuDict )

    ! Build database
    call databaseDef % init(1)
    call databaseDef % store('PN','P0')
    call database % init(databaseDef, data_ptr, silent = .true.)
    call database % activate([1], silent = .true.)

    ! Varify number of groups
#line 81 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(4, database % nGroups(), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 81) )
  if (anyExceptions()) return
#line 82 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Transport XS
    p % G = 1
#line 85 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getTrackingXS(p, 1, MATERIAL_XS), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 85) )
  if (anyExceptions()) return
#line 86 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Total XS
    p % G = 1
#line 89 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, database % getTotalMatXS(p, 2), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 89) )
  if (anyExceptions()) return
#line 90 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    p % G = 3
#line 92 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(6.0_defReal, database % getTotalMatXS(p, 1), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 92) )
  if (anyExceptions()) return
#line 93 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Majorant
    p % G = 1
#line 96 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getMajorantXS(p), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 96) )
  if (anyExceptions()) return
#line 97 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 97 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getTrackingXS(p, 1, MAJORANT_XS), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 97) )
  if (anyExceptions()) return
#line 98 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get a material and verify macroXSS
    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(2))
#line 101 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(mat), "Type Ptr Cast has failed", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 101) )
  if (anyExceptions()) return
#line 102 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
    call mat % getMacroXSs(xss, 1, p % pRNG)

    ! Check that is fissile
#line 105 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(mat % isFissile(), "Is not fissile but should", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 105) )
  if (anyExceptions()) return
#line 106 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

#line 107 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, xss % total, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 107) )
  if (anyExceptions()) return
#line 108 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 108 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(ZERO, xss % elasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 108) )
  if (anyExceptions()) return
#line 109 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 109 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.1_defReal, xss % inelasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 109) )
  if (anyExceptions()) return
#line 110 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 110 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.0_defReal, xss % capture, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 110) )
  if (anyExceptions()) return
#line 111 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 111 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.0_defReal, xss % fission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 111) )
  if (anyExceptions()) return
#line 112 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 112 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.3_defReal, xss % nuFission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 112) )
  if (anyExceptions()) return
#line 113 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    matClass => baseMgNeutronMaterial_CptrCast(database % getMaterial(1))
#line 115 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(matClass), "Type Ptr Cast has failed", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 115) )
  if (anyExceptions()) return
#line 116 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
    call matClass % getMacroXSs(xss, 4, p % pRNG)

#line 118 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(matClass % isFissile(), "Is fissile but should not", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 118) )
  if (anyExceptions()) return
#line 119 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

#line 120 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(7.1_defReal, xss % total, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 120) )
  if (anyExceptions()) return
#line 121 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 121 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(ZERO, xss % elasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 121) )
  if (anyExceptions()) return
#line 122 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 122 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, xss % inelasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 122) )
  if (anyExceptions()) return
#line 123 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 123 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(4.0_defReal, xss % capture, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 123) )
  if (anyExceptions()) return
#line 124 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 124 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(0.0_defReal, xss % fission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 124) )
  if (anyExceptions()) return
#line 125 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 125 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(0.0_defReal, xss % nuFission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 125) )
  if (anyExceptions()) return
#line 126 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get some invalid Materials
    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(0))
#line 129 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 129) )
  if (anyExceptions()) return
#line 130 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(-2))
#line 132 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 132) )
  if (anyExceptions()) return
#line 133 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(3))
#line 135 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 135) )
  if (anyExceptions()) return
#line 136 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get Fission reaction and verify type
    reac => fissionMG_TptrCast(database % getReaction(macroFission, 1))
#line 139 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), "Pointer for the mission reaction is not null", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 139) )
  if (anyExceptions()) return
#line 140 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => fissionMG_TptrCast(database % getReaction(macroFission, 2))
#line 142 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(reac), "Pointer fission reaction is wrong type or null", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 142) )
  if (anyExceptions()) return
#line 143 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get Scattering reaction and verify type
    reac => multiScatterMG_TptrCast(database % getReaction(macroIEScatter, 1))
#line 146 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(reac), "Wrong type of scattering reaction", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 146) )
  if (anyExceptions()) return
#line 147 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get some invalid reactions
    reac => database % getReaction(anyScatter, 0)
#line 150 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 150) )
  if (anyExceptions()) return
#line 151 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyScatter, -1)
#line 153 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 153) )
  if (anyExceptions()) return
#line 154 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyScatter, 3)
#line 156 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 156) )
  if (anyExceptions()) return
#line 157 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyCapture, 1)
#line 159 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 159) )
  if (anyExceptions()) return
#line 160 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! **** Note that anyFission is not present !
    reac => database % getReaction(anyFission, 2)
#line 163 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 163) )
  if (anyExceptions()) return
#line 164 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting nuclide
#line 166 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(database % getNuclide(1)), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 166) )
  if (anyExceptions()) return
#line 167 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Clean up
    call database % kill()
    call mm_kill()
    call matMenuDict % kill()
    call databaseDef % kill()

  end subroutine testBaseMgNeutronDatabaseWithP0

  !!
  !! Monster test to build and verify data in baseMgNeutronDatabase with P1 scattering
  !! *Copy and pasted from the above with only the type of scattering changed
  !!
!@Test
  subroutine testBaseMgNeutronDatabaseWithP1()
    type(baseMgNeutronDatabase), target  :: database
    class(nuclearDatabase), pointer      :: data_ptr
    type(dictionary)                     :: databaseDef
    type(dictionary)                     :: matMenuDict
    type(particle)                       :: p
    type(neutronMacroXSs)                :: xss
    type(baseMgNeutronMaterial),pointer  :: mat
    class(baseMgNeutronMaterial),pointer :: matClass
    class(reactionHandle), pointer       :: reac
    real(defReal),parameter :: TOL = 1.0E-6_defReal


    data_ptr => database

    ! Load materialMenu
    call charToDict(matMenuDict, MAT_INPUT_STR)
    call mm_init(matMenuDict )

    ! Build database
    call databaseDef % init(1)
    call databaseDef % store('PN','P1')
    call database % init(databaseDef, data_ptr, silent = .true.)
    call database % activate([1], silent = .true.)

    ! Varify number of groups
#line 207 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(4, database % nGroups(), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 207) )
  if (anyExceptions()) return
#line 208 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Transport XS
    p % G = 1
#line 211 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getTrackingXS(p, 1, MATERIAL_XS), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 211) )
  if (anyExceptions()) return
#line 212 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Total XS
    p % G = 1
#line 215 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, database % getTotalMatXS(p, 2), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 215) )
  if (anyExceptions()) return
#line 216 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    p % G = 3
#line 218 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(6.0_defReal, database % getTotalMatXS(p, 1), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 218) )
  if (anyExceptions()) return
#line 219 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting Majorant
    p % G = 1
#line 222 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getMajorantXS(p), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 222) )
  if (anyExceptions()) return
#line 223 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 223 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.1_defReal, database % getTrackingXS(p, 1, MAJORANT_XS), TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 223) )
  if (anyExceptions()) return
#line 224 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get a material and verify macroXSS
    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(2))
#line 227 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(mat), "Type Ptr Cast has failed", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 227) )
  if (anyExceptions()) return
#line 228 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
    call mat % getMacroXSs(xss, 1, p % pRNG)

    ! Check that is fissile
#line 231 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(mat % isFissile(), "Is not fissile but should", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 231) )
  if (anyExceptions()) return
#line 232 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

#line 233 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, xss % total, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 233) )
  if (anyExceptions()) return
#line 234 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 234 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(ZERO, xss % elasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 234) )
  if (anyExceptions()) return
#line 235 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 235 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.1_defReal, xss % inelasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 235) )
  if (anyExceptions()) return
#line 236 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 236 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.0_defReal, xss % capture, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 236) )
  if (anyExceptions()) return
#line 237 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 237 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(1.0_defReal, xss % fission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 237) )
  if (anyExceptions()) return
#line 238 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 238 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(2.3_defReal, xss % nuFission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 238) )
  if (anyExceptions()) return
#line 239 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    matClass => baseMgNeutronMaterial_CptrCast(database % getMaterial(1))
#line 241 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(matClass), "Type Ptr Cast has failed", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 241) )
  if (anyExceptions()) return
#line 242 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
    call matClass % getMacroXSs(xss, 4, p % pRNG)

#line 244 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(matClass % isFissile(), "Is fissile but should not", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 244) )
  if (anyExceptions()) return
#line 245 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

#line 246 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(7.1_defReal, xss % total, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 246) )
  if (anyExceptions()) return
#line 247 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 247 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(ZERO, xss % elasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 247) )
  if (anyExceptions()) return
#line 248 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 248 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(3.1_defReal, xss % inelasticScatter, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 248) )
  if (anyExceptions()) return
#line 249 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 249 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(4.0_defReal, xss % capture, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 249) )
  if (anyExceptions()) return
#line 250 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 250 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(0.0_defReal, xss % fission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 250) )
  if (anyExceptions()) return
#line 251 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
#line 251 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertEqual(0.0_defReal, xss % nuFission, TOL, &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 251) )
  if (anyExceptions()) return
#line 252 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get some invalid Materials
    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(0))
#line 255 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 255) )
  if (anyExceptions()) return
#line 256 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(-2))
#line 258 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 258) )
  if (anyExceptions()) return
#line 259 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    mat => baseMgNeutronMaterial_TptrCast(database % getMaterial(3))
#line 261 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(mat), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 261) )
  if (anyExceptions()) return
#line 262 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get Fission reaction and verify type
    reac => fissionMG_TptrCast(database % getReaction(macroFission, 1))
#line 265 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), "Pointer for the mission reaction is not null", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 265) )
  if (anyExceptions()) return
#line 266 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => fissionMG_TptrCast(database % getReaction(macroFission, 2))
#line 268 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(reac), "Pointer fission reaction is wrong type or null", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 268) )
  if (anyExceptions()) return
#line 269 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get Scattering reaction and verify type
    reac => multiScatterP1MG_TptrCast(database % getReaction(macroIEScatter, 1))
#line 272 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertTrue(associated(reac), "Wrong type of scattering reaction", &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 272) )
  if (anyExceptions()) return
#line 273 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Get some invalid reactions
    reac => database % getReaction(anyScatter, 0)
#line 276 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 276) )
  if (anyExceptions()) return
#line 277 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyScatter, -1)
#line 279 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 279) )
  if (anyExceptions()) return
#line 280 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyScatter, 3)
#line 282 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 282) )
  if (anyExceptions()) return
#line 283 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    reac => database % getReaction(anyCapture, 1)
#line 285 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 285) )
  if (anyExceptions()) return
#line 286 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! **** Note that anyFission is not present !
    reac => database % getReaction(anyFission, 2)
#line 289 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(reac), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 289) )
  if (anyExceptions()) return
#line 290 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Test getting nuclide
#line 292 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"
  call assertFalse(associated(database % getNuclide(1)), &
 & location=SourceLocation( &
 & 'baseMgNeutronDatabase_iTest.f90', &
 & 292) )
  if (anyExceptions()) return
#line 293 "/home/mskrette/SCONE_cambridge_fork/SCONE/NuclearData/mgNeutronData/baseMgNeutron/Tests/baseMgNeutronDatabase_iTest.f90"

    ! Clean up
    call database % kill()
    call mm_kill()
    call matMenuDict % kill()
    call databaseDef % kill()

  end subroutine testBaseMgNeutronDatabaseWithP1



end module baseMgNeutronDatabase_iTest

module WrapbaseMgNeutronDatabase_iTest
   use pFUnit_mod
   use baseMgNeutronDatabase_iTest
   implicit none
   private

contains


end module WrapbaseMgNeutronDatabase_iTest

function baseMgNeutronDatabase_iTest_suite() result(suite)
   use pFUnit_mod
   use baseMgNeutronDatabase_iTest
   use WrapbaseMgNeutronDatabase_iTest
   type (TestSuite) :: suite

   suite = newTestSuite('baseMgNeutronDatabase_iTest_suite')

   call suite%addTest(newTestMethod('testBaseMgNeutronDatabaseWithP0', testBaseMgNeutronDatabaseWithP0))

   call suite%addTest(newTestMethod('testBaseMgNeutronDatabaseWithP1', testBaseMgNeutronDatabaseWithP1))


end function baseMgNeutronDatabase_iTest_suite

