! dagmc fortran interface
module dagmc_mod
  use iso_c_binding

  interface dagmc1
    function dagmc_ptr() result(rval) bind(C, name = "create_dagmc_ptr")
      use, intrinsic :: iso_c_binding, only : c_int
      implicit none
      integer(c_int) :: rval
    end function dagmc_ptr
  end interface dagmc1

  interface dagmc2
      function load_file_c(cfile) result(rval) bind(C, name = "load_file")
      use, intrinsic :: iso_c_binding, only : c_char, c_int
      implicit none
      character(c_char), intent(in) :: cfile
      integer(c_int)                :: rval
    end function load_file_c
  end interface dagmc2
  
  interface dagmc3
    function init_obb_c() result(rval) bind(C, name = "init_obb")
      use, intrinsic :: iso_c_binding, only : c_char, c_int
      implicit none
      integer(c_int)                :: rval
    end function init_obb_c
  end interface dagmc3
contains 

  !! todo maybe dont even need to expose the ptr 
   function create_dagmc_ptr() result(rval)
    use, intrinsic :: iso_c_binding, only : c_int
    rval = dagmc_ptr()
   end function create_dagmc_ptr

  !! load file
   function load_file(cfile) result(rval)
     use, intrinsic :: iso_c_binding, only : c_char, c_int
     implicit none
     character(c_char), intent(in) :: cfile
     integer(c_int)                :: rval
     rval = load_file_c(cfile)
   end function load_file

  !! load file
   function init_obb() result(rval)
    implicit none
    integer(c_int)                :: rval
    rval = init_obb_c()
  end function init_obb

end module dagmc_mod
