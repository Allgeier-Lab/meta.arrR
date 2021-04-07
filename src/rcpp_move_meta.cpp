#include "rcpp_move_meta.h"
#include "rcpp_matrix_to_list.h"

//' rcpp_move_meta
//'
//' @description Rcpp list to matrix
//'
//' @param fishpop_values List with fish population.
//' @param n Integer with number of metaecosystems.
//' @param pop_n Integer with number of individuals.
//' @param prob_move Double with probability to move betwenn metaecosystems.
//' @param extent Spatial extent of the seafloor raster.
//'
//' @details
//' Simulate movement across local metaecosystem. Individuals move to a new local
//' metaecosystem with a certain probability each timestep.
//'
//' @return void
//'
//' @aliases rcpp_move_meta
//' @rdname rcpp_move_meta
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::List rcpp_move_meta(Rcpp::List fishpop_values, int n, int pop_n, double prob_move,
                          Rcpp::NumericVector extent) {

  // convert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop_values, n, pop_n);

  // get unique metasyst id
  Rcpp::IntegerVector id_meta = Rcpp::seq(1, n);

  // loop through all individuuals
  for (int i = 0; i < fishpop_mat.nrow(); i ++) {

    // get random number between 0 and 1
    double prob_temp = runif(1, 0.0, 1.0)(0);

    // MH: This should change with stationary
    // prob_move

    // move if probability is below random number
    if (prob_temp < prob_move) {

      // get current id
      int id_temp = fishpop_mat(i, 13);

      // get all id not currently in
      Rcpp::IntegerVector id_new = id_meta[id_meta != id_temp];

      // sample new random id
      int id_random = Rcpp::sample(id_new, 1)(0);

      // update meta id
      fishpop_mat(i, 13) = id_random;

      // random x coord
      fishpop_mat(i, 2) = Rcpp::runif(1, extent(0), extent(1))(0);

      // random y coord
      fishpop_mat(i, 3) = Rcpp::runif(1, extent(2), extent(3))(0);


    } else {

      continue;

    }
  }

  // convert matrix to list
  return rcpp_matrix_to_list(fishpop_mat, n);

}

/*** R
rcpp_move_meta(fishpop_values = fishpop_values, n = metasyst$n,
               pop_n = metasyst$starting_values$pop_n, prob_move = 0.25)

*/
