#' sim_nutr_input
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param freq_mn Numeric number of peak.
#' @param input_max Numeric with maximum input amount.
#' @param variability Variability of nutrient input
#' @param method String to specify how variability is simulated ('noise' or 'sd').
#' @param n_noise Integer with number of sine curves used for noise.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on sine curve. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_max} argument quantifies the
#' maximum value of the input "peaks".
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#'
#' @aliases sim_nutr_input
#' @rdname sim_nutr_input
#'
#' @export
sim_nutr_input <- function(n, max_i, freq_mn, input_max, variability,
                           method = "noise", n_noise = NULL, verbose = TRUE) {

  # init list with values for each local metaecosyst
  result_values <- vector(mode = "list", length = n)

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # calculate period for number of input peaks (period = 2 * pi / b)
  freq_mn <- freq_mn / (max_i / (2 * pi))

  # calculate maximum of input peaks, amplitude is "half" of whole sine function
  input_max <- input_max / 2

  if (method == "noise") {

    # set default n_noise
    if (is.null(n_noise)) {

      n_noise <- 3

    }

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

        # sample random phase shift
        phase_temp <- stats::rnorm(n = 1, mean = max_i / 2, sd = (max_i / 2) * variability)

        # simulate sine curve: amplitude * sin(period * (x + phase)) + vertical
        input_temp[[j]] <- amplitude_temp * sin(period_temp * (timestep + phase_temp)) + amplitude_temp

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

  } else if (method == "sd") {

    # return warning
    if (!is.null(n_noise) && verbose) {

      warning("'n_noise' is used for method = 'noise' only.", call. = FALSE)

    }

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

  } else {

    stop("Please select either method = 'noise' or method = 'sd'.", call. = FALSE)

  }

  # store results in final list
  result_list <- list(values = result_values, freq_mn = freq_mn, input_max = input_max,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
