module collNumMap_test
  use numPrecision
  use pFUnit_mod
  use particle_class,          only : particleState
  use dictionary_class,        only : dictionary
  use dictParser_func,         only : charToDict
  use outputFile_class,        only : outputFile

  use collNumMap_class,        only : collNumMap

  implicit none


!@testCase
  type, extends(TestCase) :: test_collNumMap
    private
    type(collNumMap) :: map
  contains
    procedure :: setUp
    procedure :: tearDown
  end type test_collNumMap

  !!
  !! Test parameters
  !!
  integer(shortInt), dimension(*), parameter :: COLL_NUMS = [0, 1, 2, 5, 10, 50, 81]


contains

  !!
  !! Sets up test_collNumMap object we can use in a number of tests
  !!
  subroutine setUp(this)
    class(test_collNumMap), intent(inout) :: this
    type(dictionary)                      :: dict

    ! Initialise dictionary and build map
    call dict % init(1)

    ! Build material map definition
    call dict % store('collNumbers', COLL_NUMS)
    call this % map % init(dict)

  end subroutine setUp

  !!
  !! Kills test_collNumMap object
  !!
  subroutine tearDown(this)
    class(test_collNumMap), intent(inout) :: this

    call this % map % kill()

  end subroutine tearDown

!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!! PROPER TESTS BEGIN HERE
!!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  !!
  !! Mapping test
  !!
!@Test
  subroutine testMapping(this)
    class(test_collNumMap), intent(inout)    :: this
    type(particleState)                      :: state
    integer(shortInt)                        :: i
    integer(shortInt),dimension(5)           :: bins
    integer(shortInt),dimension(5),parameter :: EXPECTED_BINS = [2, 3, 0, 0, 4]

    do i = 1,5
      state % collisionN = i
      bins(i) = this % map % map(state)
    end do

#line 77 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
  call assertEqual(EXPECTED_BINS,bins, &
 & location=SourceLocation( &
 & 'collNumMap_test.f90', &
 & 77) )
  if (anyExceptions()) return
#line 78 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"

  end subroutine testMapping

  !!
  !! Test number of bins inquiry
  !!
!@Test
  subroutine testNumberOfBinsInquiry(this)
    class(test_collNumMap), intent(inout) :: this

#line 88 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
  call assertEqual(7, this % map % bins(0), 'Total number of bins', &
 & location=SourceLocation( &
 & 'collNumMap_test.f90', &
 & 88) )
  if (anyExceptions()) return
#line 89 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
#line 89 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
  call assertEqual(7, this % map % bins(1), 'Number of bins in dimension 1', &
 & location=SourceLocation( &
 & 'collNumMap_test.f90', &
 & 89) )
  if (anyExceptions()) return
#line 90 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
#line 90 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
  call assertEqual(0, this % map % bins(2), 'Number of bins in higher dimension', &
 & location=SourceLocation( &
 & 'collNumMap_test.f90', &
 & 90) )
  if (anyExceptions()) return
#line 91 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"

  end subroutine testNumberOfBinsInquiry

  !!
  !! Test correctness of print subroutine
  !! Does not checks that values are correct, but that calls sequence is without errors
  !!
!@Test
  subroutine testPrint(this)
    class(test_collNumMap), intent(inout) :: this
    type(outputFile)                      :: out

    call out % init('dummyPrinter', fatalErrors = .false.)

    call this % map % print(out)
#line 106 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
  call assertTrue(out % isValid(),'For number of collisions map ', &
 & location=SourceLocation( &
 & 'collNumMap_test.f90', &
 & 106) )
  if (anyExceptions()) return
#line 107 "/home/mskrette/SCONE_cambridge_fork/SCONE/Tallies/TallyMaps/Tests/collNumMap_test.f90"
    call out % reset()

  end subroutine testPrint


end module collNumMap_test

module WrapcollNumMap_test
   use pFUnit_mod
   use collNumMap_test
   implicit none
   private

   public :: WrapUserTestCase
   public :: makeCustomTest
   type, extends(test_collNumMap) :: WrapUserTestCase
      procedure(userTestMethod), nopass, pointer :: testMethodPtr
   contains
      procedure :: runMethod
   end type WrapUserTestCase

   abstract interface
     subroutine userTestMethod(this)
        use collNumMap_test
        class (test_collNumMap), intent(inout) :: this
     end subroutine userTestMethod
   end interface

contains

   subroutine runMethod(this)
      class (WrapUserTestCase), intent(inout) :: this

      call this%testMethodPtr(this)
   end subroutine runMethod

   function makeCustomTest(methodName, testMethod) result(aTest)
#ifdef INTEL_13
      use pfunit_mod, only: testCase
#endif
      type (WrapUserTestCase) :: aTest
#ifdef INTEL_13
      target :: aTest
      class (WrapUserTestCase), pointer :: p
#endif
      character(len=*), intent(in) :: methodName
      procedure(userTestMethod) :: testMethod
      aTest%testMethodPtr => testMethod
#ifdef INTEL_13
      p => aTest
      call p%setName(methodName)
#else
      call aTest%setName(methodName)
#endif
   end function makeCustomTest

end module WrapcollNumMap_test

function collNumMap_test_suite() result(suite)
   use pFUnit_mod
   use collNumMap_test
   use WrapcollNumMap_test
   type (TestSuite) :: suite

   suite = newTestSuite('collNumMap_test_suite')

   call suite%addTest(makeCustomTest('testMapping', testMapping))

   call suite%addTest(makeCustomTest('testNumberOfBinsInquiry', testNumberOfBinsInquiry))

   call suite%addTest(makeCustomTest('testPrint', testPrint))


end function collNumMap_test_suite

