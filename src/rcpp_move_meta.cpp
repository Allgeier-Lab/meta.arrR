// [[Rcpp::depends(arrR)]]

#include <Rcpp.h>
#include <arrR.h>
#include <chrono>

#include "rcpp_move_meta.h"
#include "rcpp_list_to_matrix.h"
#include "rcpp_sample.h"
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
//' Simulate movement across local metaecosystem. Individuals move if their residence
//' counter equals the maximum residence time specified for each individual in the
//' attributes table. To avoid this movement set \code{parameters$move_residence_mean <= 0}.
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

    // MH: Add function argument here to switch between fixed and probability meta movement

    // // prob_move
    // double prob_move = fishpop_mat(i, 16) / fishpop_attr(id_attr, 2);
    //
    // // get random number between 0 and 1
    // double prob_random = arrR::rcpp_runif(0.0, 1.0);
    //
    // bool flag_move = prob_random > prob_move;

    // check if residence counter is smaller than residence threshold
    bool flag_stay = fishpop_mat(i, 16) < fishpop_attr(id_attr, 2);

    // fish stay in current metasystem
    if ((fishpop_attr(id_attr, 2) == 0.0) || flag_stay) {

      // increase residence by one
      fishpop_mat(i, 16) += 1;

    // fish move to new metaecosystem
    } else {

      // get current id
      int meta_temp = fishpop_mat(i, 17);

      // get probs of starting ecosystem
      Rcpp::NumericVector probs = seafloor_probs(_, meta_temp - 1);

      // sample new random id
      int id_random = rcpp_sample(meta_ids, probs);

      // update meta id
      fishpop_mat(i, 17) = id_random;

      // random x coord
      fishpop_mat(i, 2) = arrR::rcpp_runif(extent[0], extent[1]);

      // random y coord
      fishpop_mat(i, 3) = arrR::rcpp_runif(extent[0], extent[1]);

      // set residence to zero
      fishpop_mat(i, 16) = 1;

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
