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
//' @param fishpop_behavior NumericMatrix with behavior state.
//' @param fishpop_attr NumericMatrix with reserves_thres and prob_move values for each individual.
//' @param seafloor_probs NumericMatrix with local ecosystems probabilities.
//' @param extent NumericVector with spatial extent of the seafloor raster.
//'
//' @details
//' Simulate movement across local metaecosystem. Individuals move if their residence
//' counter equals the maximum residence time specified for each individual in the
//' attributes table. To avoid this movement set \code{move_meta_mean = 0} & \code{move_meta_sd = 0}.
//'
//' @return list
//'
//' @aliases rcpp_move_meta
//' @rdname rcpp_move_meta
//'
//' @keywords internal
// [[Rcpp::export]]
Rcpp::List rcpp_move_meta(Rcpp::List fishpop, Rcpp::NumericMatrix fishpop_behavior, Rcpp::NumericMatrix fishpop_attr,
                          Rcpp::NumericMatrix seafloor_probs, Rcpp::NumericVector extent) {

  // convert list to matrix
  Rcpp::NumericMatrix fishpop_mat = rcpp_list_to_matrix(fishpop, fishpop_behavior.nrow(), true);

  // create vector with all possible meta ids
  Rcpp::NumericVector meta_id (seafloor_probs.nrow());

  // fill vector with 1 to n ids
  std::iota (std::begin(meta_id), std::end(meta_id), 1);

  // loop through all individuals
  for (int i = 0; i < fishpop_mat.nrow(); i++) {

    // get row id of current individual; order of fishpop_behavior and fishpop_attr identical
    int id_i = arrR::rcpp_which(fishpop_mat(i, 0), fishpop_behavior(_, 0));

    // get recent and current behavior
    int behavior_recent = fishpop_behavior(id_i, 1);

    int behavior_current = fishpop_mat(i, 11);

    // individual foraged last timestep and is now either returning or sheltering
    if (behavior_recent == 3 && (behavior_current == 1 || behavior_current == 2)) {

      // get movement probability from attribute table
      double prob_attr = fishpop_attr(id_i, 2);

      // drawn random number between 0 and 1 to check movement probability against
      double prob_random = arrR::rcpp_runif(0.0, 1.0);

      // individual stays in current metasystem
      if (prob_random > prob_attr) {

        continue;

      // individual moves to new metaecosystem
      } else {

        // get current id
        int meta_current = fishpop_mat(i, 17);

        // get probs of starting ecosystem normalized by number of possible target ecosystems
        Rcpp::NumericVector probs = seafloor_probs(_, meta_current - 1) / (seafloor_probs.nrow() - 1);

        double meta_new = rcpp_sample(meta_id, probs);

        // sample new random id
        fishpop_mat(i, 17) = meta_new;

        // random x coord
        fishpop_mat(i, 2) = arrR::rcpp_runif(extent[0], extent[1]);

        // random y coord
        fishpop_mat(i, 3) = arrR::rcpp_runif(extent[0], extent[1]);

        // increase counter moved by one
        fishpop_mat(i, 16) += 1;

      }

    // individual stays in current metasystem
    } else {

      continue;

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
