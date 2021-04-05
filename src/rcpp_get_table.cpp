#include "rcpp_get_table.h"

//' rcpp_get_table
//'
//' @description Rcpp get table
//'
//' @param x List
//'
//' @details
//' Returns table with value count
//'
//' @return vector
//'
//' @aliases rcpp_get_table
//' @rdname rcpp_get_table
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_get_table(Rcpp::NumericVector x, int n) {

  // create vector with n elements to store count
  Rcpp::IntegerVector x_table(n, 0L);

  // loop through all possible id
  for (int i = 0; i < x.length(); i ++) {

    int value_temp = x(i) - 1;

    // increase count
    x_table(value_temp) += 1;

  }

  return(x_table);
}


/*** R
rcpp_get_table(x = c(1, 1, 2, 1, 1), n = metasyst$n)
rcpp_get_table(x = c(1, 1, 2, 3), n = metasyst$n)
rcpp_get_table(x = 2, n = metasyst$n)
*/
