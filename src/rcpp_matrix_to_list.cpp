#include "rcpp_matrix_to_list.h"

//' rcpp_matrix_to_list
//'
//' @description Rcpp list to matrix
//'
//' @param x List
//'
//' @details
//' Convert a list to a list
//'
//' @return matrix
//'
//' @aliases rcpp_matrix_to_list
//' @rdname rcpp_matrix_to_list
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix x, int n) {

  // // creating final matrix with local systems times pop_n rows and one 13 + 1 col id
  //
  // Rcpp::NumericMatrix result(3, 13)
  //
  // List result = List::create(result);
  //
  // return result;
}

/*** R
mat <- rcpp_list_to_matrix(x = fishpop_values, n = metasyst$n,
                           pop_n = metasyst$starting_values$pop_n)

rcpp_matrix_to_list(x = mat, n = metasyst$n)

*/
