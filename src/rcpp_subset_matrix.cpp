#include <Rcpp.h>
#include "rcpp_subset_matrix.h"

using namespace Rcpp;

//' rcpp_subset_matrix
//'
//' @description
//' Rcpp subset matrix.
//'
//' @param fishpop Matrix with fishpop values.
//' @param rows Vector with row ids
//'
//' @details
//' Returns matrix with only subset of rows of \code{fishpop} specified by \code{rows}.
//'
//' @return matrix
//'
//' @aliases rcpp_subset_matrix
//' @rdname rcpp_subset_matrix
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_subset_matrix(Rcpp::NumericMatrix fishpop, Rcpp::IntegerVector rows) {

  // creating final matrix
  Rcpp::NumericMatrix result(rows.length(), fishpop.ncol());

  // loop through all id
  for (int i = 0; i < rows.length(); i++) {

    // stop if row not present
    if (rows[i] > (fishpop.nrow() - 1)) Rcpp::stop("At least one 'rows' not present.");

    // save row into submatrix
    result(i, _) = fishpop(rows[i], _);

  }

  // set col names
  colnames(result) = colnames(fishpop);

  return result;
}

/*** R
fishpop_mat <- rcpp_list_to_matrix(fishpop = fishpop,
                                   pop_n_sum = sum(metasyst$starting_values$pop_n),
                                   id = FALSE)

rcpp_subset_matrix(fishpop = fishpop_mat, rows = c(4, 0, 1))
*/
