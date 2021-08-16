#' simulate_nutr_input_jitter
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param freq_mn Numeric number of peaks.
#' @param input_max Numeric with maximum input amount.
#' @param variability Variability of nutrient input
#'
#' @details
#' Simulating nutrient input based on sine curve. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_max} argument quantifies the
#' maximum value of the input "peaks". Both arguments can be multiplied by the \code{*_var}
#' argument and used as standard deviation of a normal distribution to add variability across
#' metaecosystems.
#'
#' @return vector
#'
#' @examples
#' nutr_input <- simulate_nutr_input_jitter(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#'
#' @aliases simulate_nutr_input_jitter
#' @rdname simulate_nutr_input_jitter
#'
#' @export
simulate_nutr_input_jitter <- function(n, max_i, freq_mn, input_max, variability) {

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

    # sample jitter noise
    # MH: runif
    noise <- stats::rnorm(n = length(timestep), mean = 0, sd = variability)

    # simulate sine curve: amplitude * sin(period * (x + phase)) + vertical
    values_temp <- input_max * sin(freq_mn * (timestep + noise)) + (input_max * noise)

    # shift vertically above 0
    values_temp <- values_temp + abs(min(values_temp))

    # check if any is negative
    if (any(values_temp < 0)) {

      stop("Negative input value created. Please check arguments.")

    }

    # save values
    result_values[[i]] <- values_temp

  }

  # store results in final list
  result_list <- list(values = result_values, freq_mn = freq_mn, input_max = input_max,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
