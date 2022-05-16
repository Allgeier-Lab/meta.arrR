#include <Rcpp.h>
#include <arrR.h>

#include "rcpp_update_behavior.h"

using namespace Rcpp;

//' rcpp_update_behavior
//'
//' @description
//' Rcpp update behavior.
//'
//' @param fishpop NumericMatrix with fish population.
//' @param fishpop_behavior NumericMatrix with fishpop behavior
//'
//' @details
//' Updates behavior column in fishpop behavior matrix.
//'
//' @return void
//'
//' @aliases rcpp_update_behavior
//' @rdname rcpp_update_behavior
//'
//' @keywords internal
// [[Rcpp::export]]
void rcpp_update_behavior(Rcpp::NumericMatrix fishpop, Rcpp::NumericMatrix fishpop_behavior) {

  // loop through all individuals
  for (int i = 0; i < fishpop.nrow(); i++) {

    // update behavior state of current individual
    if (!NumericVector::is_na(fishpop(i, 0))) {

      // get row id in fishpop_attr of current individual
      int id_temp = arrR::rcpp_which(fishpop(i, 0), fishpop_behavior(_, 0));

      // update behavior in fishpop_attr
      fishpop_behavior(id_temp, 1) = fishpop(i, 11);

    // no individual present
    } else {

      continue;

    }
  }
}

/*** R
rcpp_update_behavior(fishpop, fishpop_behavior)
*/
