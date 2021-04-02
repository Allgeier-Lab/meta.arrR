#include "rcpp_list_to_matrix.h"

//' rcpp_list_to_matrix
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
//' @aliases rcpp_list_to_matrix
//' @rdname rcpp_list_to_matrix
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_list_to_matrix(Rcpp::List x, int n, int pop_n) {

  // creating final matrix with local systems times pop_n rows and one 13 + 1 col id
  Rcpp::NumericMatrix result(n * pop_n, 14);

  // counter to fill rows
  int k = 0;

  // loop through all list elements
  for (int i = 0; i < x.length(); i ++) {

    // get current temp element
    Rcpp::NumericMatrix input_temp = x[i];

    // loop through all rowis of temp element
    for (int j = 0; j < input_temp.nrow(); j++) {

      // get current row
      Rcpp::NumericVector row_temp = input_temp(j, _);

      // add meta id
      row_temp.insert (13, i + 1);

      // write into current row
      result(k, _) = row_temp;

      // increase counter
      k += 1;

    }
  }

  return result;
}

/*** R
rcpp_list_to_matrix(x = fishpop_values, n = metasyst$n,
                    pop_n = metasyst$starting_values$pop_n)
*/
