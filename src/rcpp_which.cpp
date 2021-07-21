#include <Rcpp.h>
#include "rcpp_which.h"

using namespace Rcpp;

//' rcpp_which
//'
//' @description Rcpp which
//'
//' @param x Vector with values.
//' @param y Integer with maximum number of classes.
//'
//' @details
//' ADD DETAILS
//'
//' @return vector
//'
//' @aliases rcpp_which
//' @rdname rcpp_which
//'
//' @keywords export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_which(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {

  // init iterator
  IntegerVector::iterator itr;

  // init position
  int position = -1;

  // init vector to store results of all which elements
  Rcpp::IntegerVector result(y.length(), 0);

  // loop through all which elements
  for (int i = 0; i < y.length(); i++) {

    // find current y element in x
    itr = std::find(x.begin(), x.end(), y(i));

    // y was present in x
    if (itr != x.end()) {

      // calculate position of iterator
      position = (itr - x.begin());

    // y was not present
    } else {

      // set to NA
      position = NA_INTEGER;

    }

    // save in result vector
    result(i) = position;

  }

  return result;

}

/*** R
rcpp_which(x = c(4, 2, 6, 1), y = c(2, 1, 10))
*/
