#include <Rcpp.h>
#include "rcpp_list_to_matrix.h"

using namespace Rcpp;

//' rcpp_list_to_matrix
//'
//' @description
//' Rcpp list to matrix.
//'
//' @param fishpop List with individuals within local metaecosystems.
//' @param pop_n_sum Integer with number of individuals.
//' @param id If TRUE, column with metaecosystem id is added.
//'
//' @details
//' Converts list with individuals within a local metaecosystem to one matrix. If
//' \code{id = TRUE} a column identifying the metaecosystem is added.
//'
//' @return matrix
//'
//' @aliases rcpp_list_to_matrix
//' @rdname rcpp_list_to_matrix
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::NumericMatrix rcpp_list_to_matrix(Rcpp::List fishpop, int pop_n_sum, bool id) {

  // number of col
  int n_col = 17;

  // set col names
  // MH: Set automaticall using colnames(fishpop);
  Rcpp::CharacterVector col_names = Rcpp::CharacterVector::create("id", "age", "x", "y",
    "heading", "length", "weight", "activity", "respiration", "reserves", "reserves_max",
    "behavior", "consumption", "excretion", "died_consumption", "died_background",
    "residence");

  // one more col needed for id
  if (id) {

    n_col = 18;

    col_names.push_back("metasystem");

  }

  // creating final matrix with local systems times pop_n rows and additional col for id
  Rcpp::NumericMatrix result(pop_n_sum, n_col);

  // counter to fill rows
  int k = 0;

  // loop through all list elements
  for (int i = 0; i < fishpop.length(); i++) {

    // get current temp element
    Rcpp::NumericMatrix fishpop_temp = fishpop[i];

    // loop through all rows of temp element
    for (int j = 0; j < fishpop_temp.nrow(); j++) {

      if (all(Rcpp::is_na(fishpop_temp(j, _)))) continue;

      // get current row
      Rcpp::NumericVector row_temp = fishpop_temp(j, _);

      if (id) {

        // adding metaecosyst id at last position
        row_temp.push_back(i + 1);

      }

      // add to result matrix
      result(k, _) = row_temp; // result(k, _) = fishpop_temp(j, _);
      // result(k, 17) = i + 1; // this is faster but kind of weird

      // increase counter
      k += 1;

    }
  }

  // set col names
  colnames(result) = col_names;

  return result;
}

/*** R
fishpop <- lapply(metasyst$fishpop, function(i) as.matrix(i, xy = TRUE))

rcpp_list_to_matrix(fishpop = fishpop, pop_n_sum = sum(metasyst$starting_values$pop_n),
                    id = TRUE)

rcpp_list_to_matrix(fishpop = fishpop, pop_n_sum = sum(metasyst$starting_values$pop_n),
                    id = FALSE)
*/
