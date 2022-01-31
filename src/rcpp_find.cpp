#include <Rcpp.h>

#include "rcpp_find.h"

using namespace Rcpp;

//' rcpp_find
//'
//' @description
//' Rcpp find.
//'
//' @param x Integer with with value to find.
//' @param y NumericVector with values to find all elements of x in.
//'
//' @details
//' Returns the index of the \code{x} object within the \code{y} object.
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
int rcpp_find(int x, Rcpp::NumericVector y) {

  // init iterator
  NumericVector::iterator itr;

  // init position
  int position;

  // find current x element in y
  itr = std::find(y.begin(), y.end(), x);

  // x was present in y
  if (itr != y.end()) {

    // calculate position of iterator
    position = (itr - y.begin());

  // x was not present in y
  } else {

    // set to NA
    position = NA_INTEGER;

  }

  return position;
}

/*** R
rcpp_find(x = 10, y = c(2, 1, 10, 4, 8))
rcpp_find(x = 23, y = c(2, 1, 10, 4, 8))
*/
