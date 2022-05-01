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
//' @param x IntegerVector with values.
//' @param probs NumericVector with relative probabilities.
//'
//' @details
//' Samples one value from provided vector with given probabilities. The probabilities
//' must sum up to sum(probs) == 1.
//'
//' @return double
//'
//' @aliases rcpp_sample
//' @rdname rcpp_sample
//'
//' @keywords internal
// [[Rcpp::export]]
int rcpp_sample(Rcpp::IntegerVector x, Rcpp::NumericVector probs) {

  // check if each element of x has probs
  if (x.length() != probs.length()) {

    Rcpp::stop("Length of 'x' must equal length of 'p'.");

  }

  // obtain a time-based seed
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();

  // init random number generator
  std::mt19937 generator(seed);

  // init uniform distribution
  std::uniform_real_distribution<double> distribution(0.0, 1.0);

  // init object to return
  int rand = NA_REAL;

  // create vector with all possible meta ids
  Rcpp::IntegerVector shuffle_id = arrR::rcpp_shuffle(0, x.length() - 1);

  // check if rand is assigned
  while (Rcpp::IntegerVector::is_na(rand)) {

    for (int i = 0; i < shuffle_id.length(); i++) {

      // get current shuffled id
      int id_temp = shuffle_id(i);

      // draw from distribution using generator
      double check = distribution(generator);

      // if random is below probability, return current object
      if (check < probs(id_temp)) {

        rand = x(id_temp);

        break;

      }
    }
  }

  return rand;
}

/*** R
x = 1:5
probs_a <- rep(x = 1 / 5, times = 5)
probs_b <- c(0.25, 0.1666667, 0.1666667, 0.1666667, 0.25)
probs_c <- c(0.25, 0.0, 0.25, 0.25, 0.25)

result_a <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_a))
result_b <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_b))
result_c <- purrr::map_dbl(1:1000000, function(i) rcpp_sample(x = x, probs = probs_c))

plot(density(result_c), col = "#3C9BED", main = "Density", xlim = c(1, 5))
lines(density(result_b), col = "#EC579A")
lines(density(result_a), col = "#be8de0")
*/

