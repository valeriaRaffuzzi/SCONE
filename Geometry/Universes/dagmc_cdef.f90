module dagmc_cdef


  interface dagmc_ptr

    function dagmc_c() result(dagmc) bind(C, name = "DagMC")
      use, intrinsic :: iso_c_binding, only : c_ptr
      implicit none
      type(c_ptr)  :: dagmc
    end function dagmc_c

  end interface dagmc_ptr

  interface dagmc_load

    function load_file_c(cfile) result(rval) bind(C, name = "load_file")
      use, intrinsic :: iso_c_binding, only : c_char, c_int
      implicit none
      character(c_char), intent(in) :: cfile
      integer(c_int)                :: rval
    end function load_file_c

  end interface dagmc_load

end module dagmc_cdef
