#include "rcpp_get_table.h"
#include "rcpp_matrix_to_list.h"

//' rcpp_matrix_to_list
//'
//' @description Rcpp matrix to list
//'
//' @param x Matrix with fish population.
//' @param n Integer with number of metaecosystems.
//'
//' @details
//' Converts a matrix with fishpopulation to a list split by metaecosystem.
//'
//' @return list
//'
//' @aliases rcpp_matrix_to_list
//' @rdname rcpp_matrix_to_list
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix x, int n) {

  // create empty list to store results
  Rcpp::List result(n);

  // get table count of individuals within each metaecosystem
  Rcpp::IntegerVector id_table = rcpp_get_table(x(_, 13), n);

  // IntegerVector nrow_temp = ifelse(id_table == 0, 1, id_table);

  // loop through all meta ecosystems
  for (int i = 0; i < id_table.length(); i++) {

    // init counter to fill rows
    int k = 0;

    // get number of individuals in metaecosyst
    int nrow_temp = id_table(i);

    // still need one row for NA if no individual is present
    if (nrow_temp == 0) nrow_temp = 1;

    // create temp matrix with nrow according to table count
    Rcpp::NumericMatrix fishpop_temp(nrow_temp, 13);

    // individiuals present in current metaecosys
    if (id_table(i) > 0) {

      // loop through all individuals
      for (int j = 0; j < x.nrow(); j++) {

      // get current metaecosystem id
      int id_temp = x(j, 13);

      // check if current id is current metaecosystem
      if (id_temp == i + 1) {

        // get currrent row
        Rcpp::NumericVector row_temp = x(j, _);

        // remove metaecosystem id
        row_temp.erase(13);

        // write into matrix

        fishpop_temp(k, _) = row_temp;

        // increase row counter
        k++;
      }
    }

    // no individual in current metaecosystem
    } else {

      // get currrent row
      Rcpp::NumericVector row_temp(13, NA_REAL);

      // write into matrix
      fishpop_temp(k, _) = row_temp;

    }

    // set col names
    colnames(fishpop_temp) = CharacterVector::create("id", "age", "x", "y", "heading",
             "length", "weight", "reserves", "reserves_max",
             "activity", "respiration",
             "died_consumption", "died_background");

    // return result
    result[i] = fishpop_temp;

  }

  return result;
}

/*** R
mat <- rcpp_list_to_matrix(x = fishpop_values, n = metasyst$n,
                           pop_n = metasyst$starting_values$pop_n)

mat[1:3, 14] <- 3

lst <- rcpp_matrix_to_list(x = mat, n = metasyst$n)
*/
