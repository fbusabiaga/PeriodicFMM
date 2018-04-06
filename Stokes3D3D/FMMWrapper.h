#include "FMMWrapper.hpp"

extern "C" {

  class FMM_Wrapper;
  typedef FMM_Wrapper FMM_WRAPPER;

  FMM_WRAPPER* create_fmm_wrapper(int mult_order = 8, int max_pts = 1024, int init_depth = 0, FMM_Wrapper::PAXIS pbc = FMM_Wrapper::PAXIS::NONE);

  void delete_fmm_wrapper(FMM_WRAPPER* fmm_wrapper);

  void FMM_WRAPPER_FMM_SetBox(FMM_WRAPPER* fmm, double xlow, double xhigh, double ylow, double yhigh, double zlow, double zhigh);

  void FMM_WRAPPER_FMM_TreeClear(FMM_WRAPPER* fmm);

  void FMM_WRAPPER_FMM_DataClear(FMM_WRAPPER* fmm);

  void FMM_WRAPPER_FMM_UpdateTree(FMM_WRAPPER* fmm, const double* trg_coor, const double* src_coord, const int num_trg, const int num_src);

  void FMM_WRAPPER_FMM_Evaluate(FMM_WRAPPER* fmm, double *trg_value, const double *src_value, const int num_trg, const int num_src);

}
