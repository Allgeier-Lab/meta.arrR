#include "rcpp_get_unique_values.h"

//' rcpp_get_unique_values
//'
//' @description Rcpp get unique values
//'
//' @param x vector
//'
//' @details
//' Returns unique values from a vector
//'
//' @return matrix
//'
//' @aliases rcpp_get_unique_values
//' @rdname rcpp_get_unique_values
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_get_unique_values(Rcpp::NumericVector x) {

  // create set of integers
  std::set<int> unique_set;

  // loop through all values
  for(int i = 0; i < x.length(); i++) {

    // insert current value to set
    unique_set.insert(x(i));

  }

  // convert set to vector
  Rcpp::NumericVector result(unique_set.begin(), unique_set.end());

  return result;
}

/*** R
rcpp_get_unique_values(c(1, 1, 2, 1, 5, 6.5))
*/
