!!
!! This module contains codes used in tallies to indentify diffrent events
!!
module tallyCodes

  use numPrecision

  implicit none
  private


  ! List of codes for different reports
  integer(shortInt),parameter,public :: inColl_CODE      = 1000 ,&
                                        outColl_CODE     = 1001 ,&
                                        path_CODE        = 1002 ,&
                                        trans_CODE       = 1003 ,&
                                        hist_CODE        = 1004 ,&
                                        cycleStart_CODE  = 1005 ,&
                                        cycleEnd_CODE    = 1006 ,&
                                        temporalPop_CODE = 1007

  ! List of codes for different particle fates
  integer(shortInt),parameter,public :: no_FATE   = 5000, &
                                        abs_FATE  = 5001, &
                                        leak_FATE = 5002, &
                                        lost_FATE = 5003, &
                                        aged_FATE = 5004

end module tallyCodes
