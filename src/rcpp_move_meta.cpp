#include <Rcpp.h>
#include "rcpp_move_meta.h"
#include "rcpp_list_to_matrix.h"
#include "rcpp_matrix_to_list.h"
#include "rcpp_which.h"

using namespace Rcpp;

//' rcpp_move_meta
//'
//' @description Rcpp list to matrix
//'
//' @param fishpop List with fish population.
//' @param pop_n_sum Integer with total number of individuals.
//' @param fishpop_attributes Matrix with stationary value for each individual.
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
Rcpp::List rcpp_move_meta(Rcpp::List fishpop, int pop_n_sum,
                          Rcpp::IntegerVector id_attr, Rcpp::NumericVector stationary_values,
                          Rcpp::IntegerVector id_meta, Rcpp::NumericVector extent) {

  // convert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop, pop_n_sum, TRUE);

  // loop through all individuals
  for (int i = 0; i < fishpop_mat.nrow(); i++) {

    // get row id of current individual
    Rcpp::IntegerVector id_fish_temp = rcpp_which(id_attr, Rcpp::IntegerVector::create(fishpop_mat(i, 0)));

    // prob_move
    double prob_move = fishpop_mat(i, 16) / as<double>(stationary_values[id_fish_temp]);

    // get random number between 0 and 1
    double prob_random = runif(1, 0.0, 1.0)(0);

    // move if probability is below random number
    if (prob_random < prob_move) {

      // get current id
      int id_meta_temp = fishpop_mat(i, 17);

      // get all id not currently in
      Rcpp::IntegerVector id_new = id_meta[id_meta != id_meta_temp];

      // sample new random id
      int id_random = Rcpp::sample(id_new, 1)(0);

      // update meta id
      fishpop_mat(i, 17) = id_random;

      // random x coord
      fishpop_mat(i, 2) = Rcpp::runif(1, extent(0), extent(1))(0);

      // random y coord
      fishpop_mat(i, 3) = Rcpp::runif(1, extent(2), extent(3))(0);

      // set stationary to zero
      fishpop_mat(i, 16) = 0;

    // fish stay in current metasyst
    } else {

      // increase stationary by one
      fishpop_mat(i, 16) += 1;

    }
  }

  // convert matrix to list
  fishpop = rcpp_matrix_to_list(fishpop_mat, fishpop.length());

  return fishpop;

}

/*** R
rcpp_move_meta(fishpop = fishpop,
               pop_n_sum = sum(metasyst$starting_values$pop_n),
               fishpop_attributes = metasyst$fishpop_attributes,
               extent = extent)
*/
