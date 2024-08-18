module colours_test
  use numPrecision
  use colours_func, only : rgb24bit
  use pFUnit_mod

  implicit none

contains


!@Test
  subroutine testColourConversions()

    ! Test by comparison with some hex values
#line 15 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"123456"), rgb24bit(18, 52, 86), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 15) )
  if (anyExceptions()) return
#line 16 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"

#line 17 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"000000"), rgb24bit(0, 0, 0), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 17) )
  if (anyExceptions()) return
#line 18 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
#line 18 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"ffffff"), rgb24bit(255, 255, 255), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 18) )
  if (anyExceptions()) return
#line 19 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"

#line 20 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"ff0000"), rgb24bit(255, 0, 0), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 20) )
  if (anyExceptions()) return
#line 21 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
#line 21 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"00ff00"), rgb24bit(0, 255, 0), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 21) )
  if (anyExceptions()) return
#line 22 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
#line 22 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"
  call assertEqual(int(z"0000ff"), rgb24bit(0, 0, 255), &
 & location=SourceLocation( &
 & 'colours_test.f90', &
 & 22) )
  if (anyExceptions()) return
#line 23 "/home/mskrette/SCONE_cambridge_fork/SCONE/SharedModules/Tests/colours_test.f90"

  end subroutine testColourConversions

end module colours_test

module Wrapcolours_test
   use pFUnit_mod
   use colours_test
   implicit none
   private

contains


end module Wrapcolours_test

function colours_test_suite() result(suite)
   use pFUnit_mod
   use colours_test
   use Wrapcolours_test
   type (TestSuite) :: suite

   suite = newTestSuite('colours_test_suite')

   call suite%addTest(newTestMethod('testColourConversions', testColourConversions))


end function colours_test_suite

