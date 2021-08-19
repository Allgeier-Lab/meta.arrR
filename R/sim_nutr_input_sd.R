#' sim_nutr_input_sd
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param freq_mn Numeric number of peak.
#' @param input_max Numeric with maximum input amount.
#' @param variability Variability of nutrient input
#' @param phase Numeric with sine curve parameters.
#'
#' @details
#' Simulating nutrient input based on sine curve. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_max} argument quantifies the
#' maximum value of the input "peaks". Both arguments can be multiplied by the \code{*_sd}
#' argument and used as standard deviation of a normal distribution to add variability across
#' metaecosystems.
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input_sd(n = 3, max_i = 4380, freq_mn = 3, freq_sd = 0.1,
#' input_max = 1, input_sd = 0.5)
#'
#' @aliases sim_nutr_input_sd
#' @rdname sim_nutr_input_sd
#'
#' @export
sim_nutr_input_sd <- function(n, max_i, freq_mn, input_max, variability) {

  # create empty list
  result_values <- vector(mode = "list", length = n)

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # calculate period for number of input peaks (period = 2 * pi / b)
  freq_mn <- freq_mn / (max_i / (2 * pi))

  # calculate maximum of input peaks, amplitude is "half" of whole sine function
  input_max <- input_max / 2

  # loop through all metaecosystems
  for (i in seq_along(result_values)) {

    # sample mean period and amplitude from random norm dist
    period_temp <- stats::rnorm(n = 1, mean = freq_mn, sd = freq_mn * variability)

    # amplitude must be positive
    amplitude_temp <- abs(stats::rnorm(n = 1, mean = input_max, sd = input_max * variability))

    # sample random phase shift
    phase_temp <- stats::rnorm(n = 1, mean = max_i / 2, sd = (max_i / 2) * variability)

    # amplitude * sin(period * (x + phase)) + vert
    # adding amplitude_temp again to make sure input >= 0.0
    input_temp <- amplitude_temp * sin(period_temp * (timestep + phase_temp)) + amplitude_temp

    # check if any is negativ
    if (any(input_temp < 0)) {

      stop("Negative input value created. Please check arguments.")

    }

    # store results in data.frame
    result_values[[i]] <- input_temp

  }

  # store results in final list
  result_list <- list(values = result_values, freq_mn = freq_mn, input_max = input_max,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
