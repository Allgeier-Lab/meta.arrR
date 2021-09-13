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

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # check if only one variability parameter is provided
  if (length(variability) == 1) {

    variability <- rep(variability, times = 2)

  }

  # warning if variability values are outside boundary
  if (any(variability > 1.0) || any(variability < 0.0)) {

    warning("'variability' values should be 0 <= x <= 1.", call. = FALSE)

  }

  # calculate period for number of input peaks (period = 2 * pi / b)
  period <- freq_mn / (max_i / (2 * pi))

  values_input <- lapply(1:n, function(i) {

    # sample random amplitude
    amplitude_temp <- input_mn * stats::runif(n = 1, min = -variability[1],
                                                   max = variability[1])

    # sample random period
    # MH: This changes the sum of all input values
    period_temp <- period * (1 + stats::runif(n = 1, min = -variability[2],
                                              max = variability[2]))

    # calculate sine curve; vertical shift to make sure x > 0
    # amplitude * sin(period * (x + phase)) + vertical
    values_temp <- amplitude_temp * sin(period_temp * timestep) + input_mn

      if (any(values_temp < 0)) {

        warning("Negative input value created. Please check arguments.", call. = FALSE)

      }

    return(values_temp)

  })

  # set names
  names(values_input) <- paste0("Meta_", 1:n)

  # store results in final list
  result_list <- list(values = values_input, n = n, max_i = max_i,
                      input_mn = input_mn, freq_mn = freq_mn,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
