#' sim_nutr_input_noise
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param freq_mn Numeric number of peaks.
#' @param input_max Numeric with maximum input amount.
#' @param variability Variability of nutrient input
#' @param n_noise Number of noise sine functions
#'
#' @details
#' Simulating nutrient input based on sine curve. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_max} argument quantifies the
#' maximum value of the input "peaks". Both arguments can be multiplied by the \code{variability}
#' argument and used as standard deviation of a normal distribution to add variability across
#' metaecosystems. Variability is added by taking the mean of \code{n_noise} sine curves.
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input_noise(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#'
#' @aliases sim_nutr_input_noise
#' @rdname sim_nutr_input_noise
#'
#' @export
sim_nutr_input_noise <- function(n, max_i, freq_mn, input_max, variability, n_noise = 5) {

  # init list with values for each local metaecosyst
  result_values <- vector(mode = "list", length = n)

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # calculate period for number of input peaks (period = 2 * pi / b)
  freq_mn <- freq_mn / (max_i / (2 * pi))

  # calculate maximum of input peaks, amplitude is "half" of whole sine function
  input_max <- input_max / 2

  # loop through all metaecosystems
  for (i in seq_along(result_values)) {

    # init list for noise sine curves
    input_temp <- vector(mode = "list", length = n_noise)

    # loop through noise sine curves
    for (j in 1:n_noise) {

      # sample mean period and amplitude from random norm dist
      period_temp <- stats::rnorm(n = 1, mean = freq_mn, sd = freq_mn * variability)

      # amplitude must be positive
      amplitude_temp <- abs(stats::rnorm(n = 1, mean = input_max, sd = input_max * variability))

      # simulate sine curve: amplitude * sin(period * (x + phase)) + vertical
      input_temp[[j]] <- amplitude_temp * sin(period_temp * timestep ) + amplitude_temp

      # check if any is negative
      if (any(input_temp[[j]] < 0)) {

        stop("Negative input value created. Please check arguments.")

      }
    }

    # cbind data to matrix (rows: timestep, cols = noise) and take mean of rows
    input_temp <- apply(X = do.call("cbind", input_temp), MARGIN = 1, FUN = "mean")

    # store results in values list of metaecosystem
    result_values[[i]] <- input_temp

  }

  # store results in final list
  result_list <- list(values = result_values, freq_mn = freq_mn, input_max = input_max,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
