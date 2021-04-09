#include "rcpp_move_meta.h"
#include "rcpp_matrix_to_list.h"

//' rcpp_move_meta
//'
//' @description Rcpp list to matrix
//'
//' @param fishpop_values List with fish population.
//' @param n Integer with number of metaecosystems.
//' @param pop_n Integer with number of individuals.
//' @param fishpop_stationary Matrix with stationary value for each individual.
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
Rcpp::List rcpp_move_meta(Rcpp::List fishpop_values, int n, int pop_n,
                          Rcpp::NumericMatrix fishpop_stationary,
                          Rcpp::NumericVector extent) {

  // convert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop_values, n, pop_n);

  // get unique metasyst id
  Rcpp::IntegerVector id_meta = Rcpp::seq(1, n);

  // get stationary values
  Rcpp::NumericVector stationary_vals = fishpop_stationary(_, 1);

  // loop through all individuuals
  for (int i = 0; i < fishpop_mat.nrow(); i++) {

    // get id of current individual
    Rcpp::LogicalVector id_fish_temp = fishpop_stationary(_, 0) == fishpop_mat(i, 0);

    // get stationary value
    double stationary_vals_temp = as<double>(stationary_vals[id_fish_temp]);

    // prob_move
    double prob_move = fishpop_mat(i, 13) / stationary_vals_temp;

    // get random number between 0 and 1
    double prob_temp = runif(1, 0.0, 1.0)(0);

    // move if probability is below random number
    if (prob_temp < prob_move) {

      // get current id
      int id_meta_temp = fishpop_mat(i, 14);

      // get all id not currently in
      Rcpp::IntegerVector id_new = id_meta[id_meta != id_meta_temp];

      // sample new random id
      int id_random = Rcpp::sample(id_new, 1)(0);

      // update meta id
      fishpop_mat(i, 14) = id_random;

      // random x coord
      fishpop_mat(i, 2) = Rcpp::runif(1, extent(0), extent(1))(0);

      // random y coord
      fishpop_mat(i, 3) = Rcpp::runif(1, extent(2), extent(3))(0);

      // set stationary to zero
      fishpop_mat(i, 13) = 0;


    } else {

      // increase stationary by one
      fishpop_mat(i, 13) += 1;

    }
  }

  // convert matrix to list
  return rcpp_matrix_to_list(fishpop_mat, n);

}

/*** R
rcpp_move_meta(fishpop_values = fishpop_values,
               n = n, pop_n = pop_n,
               fishpop_stationary = fishpop_stationary,
               extent = as.vector(extent, mode = "numeric"))
*/
