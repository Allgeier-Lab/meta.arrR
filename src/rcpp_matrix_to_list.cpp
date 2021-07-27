#include <Rcpp.h>
#include "rcpp_matrix_to_list.h"
#include "rcpp_get_table.h"
#include "rcpp_matrix_to_list.h"

using namespace Rcpp;

//' rcpp_matrix_to_list
//'
//' @description
//' Rcpp matrix to list.
//'
//' @param fishpop Matrix with fish population.
//' @param n Integer with number of total metaecosystems.
//'
//' @details
//' Converts individual matrix to a list split by metaecosystems.
//'
//' @return list
//'
//' @aliases rcpp_matrix_to_list
//' @rdname rcpp_matrix_to_list
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix fishpop, int n) {

  // create empty list to store results
  Rcpp::List result(n);

  // get table count of individuals within each metaecosystem
  Rcpp::IntegerVector id_table = rcpp_get_table(fishpop(_, 17), n);

  // loop through all meta ecosystems
  for (int i = 0; i < n; i++) {

    // init counter to fill rows
    int k = 0;

    // get number of individuals in metaecosystem
    int nrow_temp = id_table[i];

    // still need one row for NA if no individual is present
    if (nrow_temp == 0) nrow_temp = 1;

    // create temp matrix with nrow according to table count
    Rcpp::NumericMatrix fishpop_temp(nrow_temp, 17);

    // individuals present in current metaecosystem
    if (id_table[i] > 0) {

      // loop through all individuals
      for (int j = 0; j < fishpop.nrow(); j++) {

      // get current metaecosystem id
      int id_temp = fishpop(j, 17);

      // check if current id is current metaecosystem
      if (id_temp == i + 1) {

        // get current row
        Rcpp::NumericVector row_temp = fishpop(j, _);

        // remove metaecosystem id
        row_temp.erase(17);

        // write into matrix
        fishpop_temp(k, _) = row_temp;

        // increase row counter
        k++;
      }
    }

    // no individual in current metaecosystem
    } else {

      // get currrent row
      Rcpp::NumericVector row_temp(17, NA_REAL);

      // write into matrix
      fishpop_temp(k, _) = row_temp;

    }

    // set col names
    // MH: Set automaticall using colnames(fishpop);
    colnames(fishpop_temp) = Rcpp::CharacterVector::create("id", "age", "x", "y", "heading",
             "length", "weight", "activity", "respiration", "reserves", "reserves_max",
             "behavior", "consumption", "excretion", "died_consumption", "died_background",
             "stationary");

    // return result
    result[i] = fishpop_temp;

  }

  return result;
}

/*** R
fishpop <- lapply(metasyst$fishpop, function(i) as.matrix(i, xy = TRUE))

mat <- rcpp_list_to_matrix(fishpop = fishpop,
                           pop_n_sum = sum(metasyst$starting_values$pop_n), id = TRUE)

mat[5, 18] <- 3

rcpp_matrix_to_list(fishpop = mat, n = metasyst$n)
*/
