#include <Rcpp.h>
#include "rcpp_get_table.h"

using namespace Rcpp;

//' rcpp_get_table
//'
//' @description
//' Rcpp get table.
//'
//' @param x Vector with values.
//' @param n Integer with maximum number of classes.
//'
//' @details
//' Returns table with value count. Only values x > 0 are allowed.
//'
//' @return vector
//'
//' @aliases rcpp_get_table
//' @rdname rcpp_get_table
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_get_table(Rcpp::NumericVector x, int n) {

  // create vector with n elements to store count
  Rcpp::IntegerVector x_table(n, 0L);

  // loop through all possible id
  for (int i = 0; i < x.length(); i++) {

    int value_temp = x[i] - 1;

    // increase count
    x_table(value_temp) += 1;

  }

  return(x_table);
}

/*** R
rcpp_get_table(x = c(1, 1, 2, 1, 1), n = 3)
rcpp_get_table(x = c(1, 1, 2, 3), n = 4)
rcpp_get_table(x = c(2, 3, 3), n = 3)
*/
