#include <Rcpp.h>

#include "rcpp_find.h"

using namespace Rcpp;

//' rcpp_find
//'
//' @description
//' Rcpp find.
//'
//' @param x NumericVector with with values to find.
//' @param y NumericVector with values to find all elements of x in.
//'
//' @details
//' Returns the index of all elements of the \code{x} object within the \code{y} object.
//' The index of the first element is 0. If an element is not present within \code{y} object,
//' \code{NA} is returned. Returns only the first index of an object if present multiple
//' times in the \code{y} object.
//'
//' @return NumericVector
//'
//' @aliases rcpp_find
//' @rdname rcpp_find
//'
//' @export
// [[Rcpp::export]]
Rcpp::IntegerVector rcpp_find(NumericVector x, Rcpp::NumericVector y) {

  // init iterator
  NumericVector::iterator itr;

  // init position
  int position;

  // init vector to store results of all which elements
  Rcpp::IntegerVector result(x.length(), NA_INTEGER);

  for (int i = 0; i < x.length(); i++) {

    // find current x element in y
    itr = std::find(y.begin(), y.end(), x(i));

    // x was present in y
    if (itr != y.end()) {

      // calculate position of iterator
      position = (itr - y.begin());

    // x was not present in y
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
rcpp_find(x = 4, y = c(2, 1, 10, 4, 8))
rcpp_find(x = c(1, 10, 2), y = c(2, 1, 10, 4, 8))
*/
