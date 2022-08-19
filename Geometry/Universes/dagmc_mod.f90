module dagmc
    use iso_c_binding

    include "dagmc_cdef.f90"
  
    type dagmc
      private
      type(c_ptr) :: ptr
    end type
  
    interface dagmc
      procedure create_dagmc
    end interface
  
  contains 
    function create_dagmc
      create_dagmc%ptr = dagmc_c()
    end function  
end module dagmc