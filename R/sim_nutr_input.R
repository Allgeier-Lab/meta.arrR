#' sim_nutr_input
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param input_mn,freq_mn Numeric with mean input amount and frequency.
#' @param variability Variability of nutrient input.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on sine curves. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_mn} argument quantifies the
#' mean input values.
#'
#' If two \code{variability} parameters are provided, the first one is used for \code{input_mn},
#' the second one for \code{freq_mn}.
#'
#' @return nutr_input
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#'
#' @aliases sim_nutr_input
#' @rdname sim_nutr_input
#'
#' @export
sim_nutr_input <- function(n, max_i, input_mn, freq_mn, variability = c(0.0, 0.0),
                           verbose = TRUE) {

  # init list with values for each local metaecosyst
  result_values <- vector(mode = "list", length = n)

  # set names
  names(result_values) <- paste0("Meta_", 1:n)

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # check if only one variability parameter is provided
  if (length(variability) == 1) {

    variability <- rep(variability, times = 2)

  }

  # draw random amplitudes
  amplitude_rand <- abs(stats::rnorm(n = n, mean = input_mn, sd = input_mn * variability[1]))

  # MH: This wouldn't allow equal total input
  # draw random frequency
  freq_rand <- abs(stats::rnorm(n = n, mean = freq_mn, sd = freq_mn * variability[2]))

  # calculate period for number of input peaks (period = 2 * pi / b)
  period_rand <- freq_rand / (max_i / (2 * pi))

  # draw random phase shift
  phase_rand <- rep(x = 0, times = max_i)
  # phase_rand <- stats::runif(n = n, min = 0, max = max_i * variability[2])

  # loop through all metaecosystems
  for (i in seq_along(result_values)) {

    # simulate sine curve: amplitude * sin(period * (x + phase)) + vertical
    input_temp <- amplitude_rand[i] * sin(period_rand[i] * (timestep + phase_rand[i])) +
      max(amplitude_rand)

    # check if any is negative
    if (any(input_temp < 0)) {

      warning("Negative input value created. Please check arguments.", call. = FALSE)

    }

    # store results in data.frame
    result_values[[i]] <- input_temp

  }

  # store results in final list
  result_list <- list(values = result_values, n = n, max_i = max_i,
                      input_mn = input_mn, freq_mn = freq_mn,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
