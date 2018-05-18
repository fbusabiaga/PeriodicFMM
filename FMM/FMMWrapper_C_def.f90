  ! C functions declaration
  ! Author: Florencio Balboa Usabiaga
  
  interface
     function create_fmm_wrapper_c(mult_order, max_pts, init_depth, pbc) bind(C, name="create_fmm_wrapper")
       use iso_c_binding
       implicit none
       type(c_ptr) :: create_fmm_wrapper_c
       integer(c_int), value :: mult_order
       integer(c_int), value :: max_pts
       integer(c_int), value :: init_depth
       integer(c_int), value :: pbc
     end function create_fmm_wrapper_c

     subroutine delete_fmm_wrapper_c(fmm_wrapper) bind(C, name="delete_fmm_wrapper")
       use iso_c_binding
       implicit none
       type(c_ptr), value :: fmm_wrapper
     end subroutine delete_fmm_wrapper_c

     subroutine fmm_wrapper_FMM_SetBox_c(fmm, xlow, xhigh, ylow, yhigh, zlow, zhigh) bind (C, name="FMM_WRAPPER_FMM_SetBox")
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: fmm
       real(c_double), value :: xlow, xhigh, ylow, yhigh, zlow, zhigh
     end subroutine fmm_wrapper_FMM_SetBox_c

     subroutine fmm_wrapper_FMM_TreeClear_c(fmm) bind (C, name="FMM_WRAPPER_FMM_TreeClear")
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: fmm
     end subroutine fmm_wrapper_FMM_TreeClear_c

     subroutine fmm_wrapper_FMM_DataClear_c(fmm) bind (C, name="FMM_WRAPPER_FMM_DataClear")
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: fmm
     end subroutine fmm_wrapper_FMM_DataClear_c

     subroutine fmm_wrapper_FMM_UpdateTree_c(fmm, trg_coord, src_coord, num_trg, num_src) bind (C, name="FMM_WRAPPER_FMM_UpdateTree")
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: fmm
       real(c_double), intent(in) :: src_coord(*), trg_coord(*)
       integer(c_int), intent(in), value :: num_src, num_trg
     end subroutine fmm_wrapper_FMM_UpdateTree_c

     subroutine fmm_wrapper_FMM_Evaluate_c(fmm, trg_coord, src_coord, num_trg, num_src) bind (C, name="FMM_WRAPPER_FMM_Evaluate")
       use iso_c_binding
       implicit none
       type(c_ptr), intent(in), value :: fmm
       real(c_double), intent(in) :: src_coord(*) 
       real(c_double), intent(inout) :: trg_coord(*)
       integer(c_int), intent(in), value :: num_src, num_trg
     end subroutine fmm_wrapper_FMM_Evaluate_c

  end interface
