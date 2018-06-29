#include <boost/python.hpp>
#include <boost/python/numpy.hpp>
#include <stdio.h>
#include <iostream>
#include <math.h>
#include <vector>

#include "FMM/FMMWrapper.h"

namespace p = boost::python;
namespace np = boost::python::numpy;


void FMM_SetBox(FMM_Wrapper* fmm, double xlow, double xhigh, double ylow, double yhigh, double zlow, double zhigh){
  fmm->FMM_SetBox(xlow, xhigh, ylow, yhigh, zlow, zhigh);
}

void FMM_UpdateTree(FMM_Wrapper* fmm, np::ndarray trg_coord, np::ndarray src_coord){

  // Transform ndarray to std::vectors
  int num_trg = trg_coord.shape(0);
  double * trg_iter = reinterpret_cast<double*>(trg_coord.get_data());
  std::vector<double> trg_coord_vec(trg_iter, trg_iter + num_trg * 3);
  int num_src = src_coord.shape(0);
  double * src_iter = reinterpret_cast<double*>(src_coord.get_data());
  std::vector<double> src_coord_vec(src_iter, src_iter + num_src * 3);
  
  // Call method
  fmm->FMM_UpdateTree(src_coord_vec, trg_coord_vec);

  return;
}

void FMM_Evaluate(FMM_Wrapper* fmm, np::ndarray trg_value, np::ndarray src_value){

  // Transform ndarray to std::vectors
  int num_trg = trg_value.shape(0);
  std::vector<double> trg_value_vec(num_trg * 3);
  int num_src = src_value.shape(0);
  double* src_iter = reinterpret_cast<double*>(src_value.get_data());
  std::vector<double> src_value_vec(src_iter, src_iter + num_src * 3);

  // Call method
  fmm->FMM_Evaluate(trg_value_vec, num_trg, &src_value_vec);
  
  // Copy std::vector to ndarray
  std::copy(trg_value_vec.begin(), trg_value_vec.end(), reinterpret_cast<double*>(trg_value.get_data()));

  return;
}

void FMM_TreeClear(FMM_Wrapper* fmm){
  fmm->FMM_TreeClear();
}

void FMM_DataClear(FMM_Wrapper* fmm){
  fmm->FMM_DataClear();
}


BOOST_PYTHON_MODULE(periodic_fmm)
{
  using namespace boost::python;
  
  // Initialize numpy
  Py_Initialize();
  np::initialize();

  // Class FMM_Wrapper
  enum_<FMM_Wrapper::PAXIS>("PAXIS")
    .value("NONE", FMM_Wrapper::NONE)
    .value("PX", FMM_Wrapper::PX)
    .value("PY", FMM_Wrapper::PY)
    .value("PZ", FMM_Wrapper::PZ)
    .value("PXY", FMM_Wrapper::PXY)
    .value("PXZ", FMM_Wrapper::PXZ)
    .value("PYZ", FMM_Wrapper::PYZ)
    .value("PXYZ", FMM_Wrapper::PXYZ);
  class_<FMM_Wrapper>("FMM_Wrapper", init<int, int, int, FMM_Wrapper::PAXIS>());
  
  // Define functions
  def("FMM_SetBox", FMM_SetBox);
  def("FMM_UpdateTree", FMM_UpdateTree);
  def("FMM_Evaluate", FMM_Evaluate);
  def("FMM_TreeClear", FMM_TreeClear);
  def("FMM_DataClear", FMM_DataClear);
}


