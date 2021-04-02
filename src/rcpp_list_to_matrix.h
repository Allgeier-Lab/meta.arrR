//#ifndef RCPP_LIST_TO_MATRIX
//#define RCPP_LIST_TO_MATRIX

#include "Rcpp.h"

using namespace Rcpp;

Rcpp::NumericMatrix rcpp_list_to_matrix(List x, int n, int pop_n);

//#endif // RCPP_LIST_TO_MATRIX
