// [[Rcpp::depends(arrR)]]

#include <Rcpp.h>
#include <arrR.h>

#include "rcpp_move_meta.h"
#include "rcpp_list_to_matrix.h"
#include "rcpp_matrix_to_list.h"

using namespace Rcpp;

//' rcpp_move_meta
//'
//' @description
//' Rcpp move meta.
//'
//' @param fishpop List with fish population.
//' @param seafloor_probs NumericMatrix with local ecosystems probabilities.
//' @param fishpop_attr NumericMatrix with residence and reserves_thres values for each individual.
//' @param extent NumericVector with spatial extent of the seafloor raster.
//'
//' @details
//' Simulate movement across local metaecosystem. Individuals move to a new local
//' metaecosystem with a certain probability each timestep. The probability increases
//' depending on the residence value and how long individuals already stayed on local
//' metaecosystem. To avoid this movement set \code{parameters$move_residence <= 0}.
//'
//' @return list
//'
//' @aliases rcpp_move_meta
//' @rdname rcpp_move_meta
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_move_meta(Rcpp::List fishpop, Rcpp::NumericMatrix seafloor_probs,
                          Rcpp::NumericMatrix fishpop_attr, Rcpp::NumericVector extent) {

  // convert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop, fishpop_attr.nrow(),
                                                        true);

  // create vector with all possible meta ids
  Rcpp::IntegerVector meta_ids = Rcpp::seq(1, seafloor_probs.nrow());

  // loop through all individuals
  for (int i = 0; i < fishpop_mat.nrow(); i++) {

    // get row id of current individual
    int id_attr = arrR::rcpp_which(fishpop_mat(i, 0), fishpop_attr(_, 0));

    // prob_move
    double prob_move = fishpop_mat(i, 16) / fishpop_attr(id_attr, 2);

    // get random number between 0 and 1
    double prob_random = arrR::rcpp_runif(0.0, 1.0);

    // fish stay in current metasystem
    if ((fishpop_attr(id_attr, 2) == 0.0) || (prob_random > prob_move)) {

      // increase residence by one
      fishpop_mat(i, 16) += 1;

    // fish move to new metaecosystem
    } else {

      // get current id
      int meta_temp = fishpop_mat(i, 17);

      // get probs of starting ecosystem
      Rcpp::NumericVector p = seafloor_probs(_, meta_temp - 1);

      // sample new random id
      int id_random = Rcpp::sample(meta_ids, 1, false, p)(0);

      // update meta id
      fishpop_mat(i, 17) = id_random;

      // random x coord
      fishpop_mat(i, 2) = arrR::rcpp_runif(extent[0], extent[1]);

      // random y coord
      fishpop_mat(i, 3) = arrR::rcpp_runif(extent[0], extent[1]);

      // set residence to zero
      fishpop_mat(i, 16) = 0;

    }
  }

  // convert matrix to list
  fishpop = rcpp_matrix_to_list(fishpop_mat, fishpop.length());

  return fishpop;

}

/*** R
rcpp_move_meta(fishpop = fishpop, seafloor_probs = seafloor_probs,
               fishpop_attr = fishpop_attr,
               extent = extent)
*/
