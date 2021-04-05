#include "rcpp_move_meta.h"
#include "rcpp_get_unique_values.h"
#include "rcpp_matrix_to_list.h"

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

  // connvert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop_values, n, pop_n);

  Rcpp::NumericVector id_meta = rcpp_get_unique_values(fishpop_mat(_, 13));

  // loop through all individuuals
  for (int i = 0; i < fishpop_mat.nrow(); i ++) {

    // get random number between 0 and 1
    double prob_temp = runif(1, 0.0, 1.0)(0);

    // move if probability is below random number
    if (prob_temp < prob_move) {

      // get current id
      int id_temp = fishpop_mat(i, 13);

      // get all id not currently in
      Rcpp::NumericVector id_new = id_meta[id_meta != id_temp];

      // sample new random id
      int id_random =  Rcpp::as<int>(Rcpp::sample(id_new, 1));

      // update meta id
      fishpop_mat(i, 13) = id_random;

    } else {

      continue;

    }
  }

  // convert matrix to list
  // MH: Modify in place not possible I think
  fishpop_values = rcpp_matrix_to_list(fishpop_mat, n);

}

/*** R
rcpp_move_meta(fishpop_values = fishpop_values, n = metasyst$n,
               pop_n = metasyst$starting_values$pop_n, prob_move = 0.5)

*/
