#include "FMMWrapper.h"
#include "FMMWrapper.hpp"

#include <iostream>
using namespace std;


FMM_WRAPPER* create_fmm_wrapper(int mult_order, int max_pts, int init_depth, FMM_Wrapper::PAXIS pbc){
    return new FMM_Wrapper(mult_order, max_pts, init_depth, pbc);
}

//void delete_fmm_wrapper(FMM_WRAPPER* fmm_wrapper);
void delete_fmm_wrapper(FMM_WRAPPER* fmm_wrapper){
    delete fmm_wrapper;
}
    
void FMM_WRAPPER_FMM_SetBox(FMM_WRAPPER* fmm, double xlow, double xhigh, double ylow, double yhigh, double zlow, double zhigh){
  fmm->FMM_SetBox(xlow, xhigh, ylow, yhigh, zlow, zhigh);
}

void FMM_WRAPPER_FMM_TreeClear(FMM_WRAPPER* fmm){
  fmm->FMM_TreeClear();
}

void FMM_WRAPPER_FMM_DataClear(FMM_WRAPPER* fmm){
  fmm->FMM_DataClear();
}

void FMM_WRAPPER_FMM_UpdateTree(FMM_WRAPPER* fmm, const double* trg_coord, const double* src_coord, const int num_trg, const int num_src){
  // Copy arrays to vectors
  std::vector<double> trg_coord_vec(trg_coord, trg_coord + 3*num_trg);
  std::vector<double> src_coord_vec(src_coord, src_coord + 3*num_src);

  // Call method to update Tree
  fmm->FMM_UpdateTree(src_coord_vec, trg_coord_vec);
}

void FMM_WRAPPER_FMM_Evaluate(FMM_WRAPPER* fmm, double *trg_value, const double *src_value, const int num_trg, const int num_src){
  std::vector<double> trg_value_vec(num_trg * 3);

  // Copy array to vector
  std::vector<double> src_value_vec(src_value, src_value + 3*num_src);

  // Call method to evaluate FMM
  fmm->FMM_Evaluate(trg_value_vec, num_trg, &src_value_vec);
  
  // Copy vector to array
  std::copy(trg_value_vec.begin(), trg_value_vec.end(), trg_value);
}

