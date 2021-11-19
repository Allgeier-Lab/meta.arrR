#include <Rcpp.h>
#include "rcpp_which.h"

using namespace Rcpp;

//' rcpp_which
//'
//' @description
//' Rcpp which.
//'
//' @param x Vector with values.
//' @param y Vector values to find position.
//'
//' @details
//' Returns index of all elements of \code{y} vector within \code{x} vector.
//' The index of the first element is 0. If element is not present within  \code{x} vector
//' \code{NA} is returned.
//'
//' @return vector
//'
//' @aliases rcpp_which
//' @rdname rcpp_which
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_which(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {

  // init iterator
  IntegerVector::iterator itr;

  // init position
  int position;

  // init vector to store results of all which elements
  Rcpp::IntegerVector result(y.length(), 0);

  // loop through all which elements
  for (int i = 0; i < y.length(); i++) {

    // find current y element in x
    itr = std::find(x.begin(), x.end(), y[i]);

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
rcpp_which(x = c(4, 2, 6, 1), y = c(2, 1, 10, 1, 1))
*/
