// [[Rcpp::depends(arrR)]]

#include <Rcpp.h>
#include <arrR.h>
#include <chrono>
#include <random>

#include "rcpp_sample.h"

using namespace Rcpp;

//' rcpp_sample
//'
//' @description
//' Rcpp sample vector
//'
//' @param x NumericVector with values.
//' @param probs NumericVector with relative probabilities.
//'
//' @details
//' Samples one value from provided vector with given probabilities. The probabilities
//' must sum up to sum(probs) == 1.
//'
//' @references
//' How to use time-based seed based on <http://www.cplusplus.com/reference/algorithm/shuffle/>
//'
//' @return double
//'
//' @aliases rcpp_sample
//' @rdname rcpp_sample
//'
//' @keywords internal
// [[Rcpp::export]]
double rcpp_sample(Rcpp::NumericVector x, Rcpp::NumericVector probs) {

  // check if each element of x has probs
  if (x.length() != probs.length()) {

    Rcpp::stop("Length of 'x' must equal length of 'probs'.");

  }

  // init object to return
  int rand = NA_REAL;

  // create vector with all possible meta ids
  Rcpp::NumericVector x_shuffle = arrR::rcpp_shuffle(x, false);

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  // init random number generator
  std::mt19937 rng(seed);

  // init uniform distribution
  std::uniform_real_distribution<double> distribution(0.0, 1.0);

  // check if rand is still NA because no element was sampled
  while (Rcpp::IntegerVector::is_na(rand)) {

    // loop through all vector elements
    for (int i = 0; i < x.length(); i++) {

      int i_temp = x_shuffle[i] - 1;

      // draw from distribution using generator
      double check = distribution(rng);

      // if random is below probability, sample current object
      if (check < probs[i_temp]) {

        rand = x[i_temp];

        break;

      }
    }
  }

  return rand;
}

/*** R
x = seq(from = 11, to = 15, by = 1)
probs_a <- rep(x = 1 / 5, times = 5)
probs_b <- c(0.25, 0.1666667, 0.1666667, 0.1666667, 0.25)
probs_c <- c(0.25, 0.0, 0.25, 0.25, 0.25)

result_a <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_a))
result_b <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_b))
result_c <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_c))

plot(density(result_c), col = "#3C9BED", main = "Density", xlim = c(11, 15))
lines(density(result_b), col = "#EC579A")
lines(density(result_a), col = "#be8de0")
*/

