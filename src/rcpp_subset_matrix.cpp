#include <Rcpp.h>
#include "rcpp_subset_matrix.h"

using namespace Rcpp;

//' rcpp_subset_matrix
//'
//' @description Rcpp list to matrix
//'
//' @param fishpop List with individuals within metaecosystems.
//' @param pop_n_sum Integer with number of indiviuals.
//'
//' @details
//' Converts list with individuals within metaecosystems to one matrix. Adds coloumn
//' with metasystem id
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

    int row_temp = rows(i);

    if (row_temp > (fishpop.nrow() - 1)) Rcpp::stop("At least one 'rows' not present.");

    result(i, _) = fishpop(row_temp, _);

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
