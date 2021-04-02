#include "rcpp_move_meta.h"

//' rcpp_move_meta
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
//' @aliases rcpp_move_meta
//' @rdname rcpp_move_meta
//'
//' @keywords export
// [[Rcpp::export]]
void rcpp_move_meta(Rcpp::List fishpop_values, int n, int pop_n, double prob_move) {

  Rcpp::NumericMatrix mat = rcpp_list_to_matrix(fishpop_values, n, pop_n);

  for (int i = 0; i < mat.nrow(); i ++) {

    double prob_temp = runif(1, 0.0, 1.0)(0);

    Rcout << "prob_temp: " << prob_temp << std::endl;

    if (prob_temp < prob_move) {

      int meta_temp = mat(i, 13);

      Rcout << "meta_temp: " << meta_temp << std::endl;

      // sample from x = 1:n but without meta_temp --> must be int!

      // change mat(i, 13) to new sampled value

    }
  }
}

/*** R
rcpp_move_meta(fishpop_values = fishpop_values, n = metasyst$n,
               pop_n = metasyst$starting_values$pop_n, prob_move = 0.5)
*/
