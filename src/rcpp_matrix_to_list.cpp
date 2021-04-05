#include "rcpp_get_table.h"
#include "rcpp_matrix_to_list.h"

//' rcpp_matrix_to_list
//'
//' @description Rcpp list to matrix
//'
//' @param x List
//'
//' @details
//' Convert a matrix to a list
//'
//' @return list
//'
//' @aliases rcpp_matrix_to_list
//' @rdname rcpp_matrix_to_list
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::List rcpp_matrix_to_list(Rcpp::NumericMatrix x, int n) {

  Rcpp::List result(n);

  Rcpp::IntegerVector id_table = rcpp_get_table(x(_, 13), n);

  // Rcout << id_table << std::endl;

  for (int i = 0; i < n; i++) {

    Rcpp::NumericMatrix fishpop_temp(id_table(i), 13);

    int k = 0;

    for (int j = 0; j < x.nrow(); j++) {

      int id_temp = x(j, 13);

      if (id_temp == i + 1) {

        Rcpp::NumericVector row_temp = x(j, _);

        row_temp.erase(13);

        fishpop_temp(k, _) = row_temp;

        k++;

      }
    }

    result[i] = fishpop_temp;

  }

  return result;
}

/*** R
mat <- rcpp_list_to_matrix(x = fishpop_values, n = metasyst$n,
                           pop_n = metasyst$starting_values$pop_n)

mat[1:2, 14] <- 3

rcpp_matrix_to_list(x = mat, n = metasyst$n)
*/
