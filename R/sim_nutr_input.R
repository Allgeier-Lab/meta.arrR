#' sim_nutr_input
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param input_mn,freq_mn Numeric with mean input amount and frequency.
#' @param variability Variability of nutrient input.
#' @param amplitude_mod Numeric to modifiy amplituded (instead of random variability).
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
#' variability = c(1.0, 1.0))
#'
#' @aliases sim_nutr_input
#' @rdname sim_nutr_input
#'
#' @export
sim_nutr_input <- function(n, max_i, input_mn, freq_mn, variability = c(0.0, 0.0),
                           amplitude_mod = NULL, verbose = TRUE) {

  # create vector from 1 to max_i for nutrient input
  timesteps <- 1:max_i

  # check if only one variability parameter is provided
  if (length(variability) == 1) {

    variability <- rep(x = variability, times = 2)

  }

  if (length(amplitude_mod) == 1 && n != 1) {

    amplitude_mod <- rep(x = amplitude_mod, times = n)

  } else if (!is.null(amplitude_mod) && length(amplitude_mod) != n) {

    stop("'amplitude_mod' must have the same length as n.", call. = FALSE)

  }

  # warning if variability values are outside boundary
  if (any(variability > 1.0) || any(variability < 0.0)) {

    warning("'variability' values should be 0 <= x <= 1.", call. = FALSE)

  }

  # init results
  values_input <- vector(mode = "list", length = n)

  amplitude_i <- vector(mode = "numeric", length = n)

  phase_i <- vector(mode = "numeric", length = n)

  # calculate period for number of input peaks (period = 2 * pi / b)
  period <- freq_mn / (max_i / (2 * pi))

  for (i in 1:n) {

    # sample random amplitude
    if (is.null(amplitude_mod)) {

      amplitude_temp <- input_mn * (1 - stats::runif(n = 1, min = 0.0,
                                                     max = variability[1]))

    # use amplitude_mod
    } else {

      amplitude_temp <- input_mn * amplitude_mod[i]

    }

    # sample random phase shift
    phase_temp <- stats::runif(n = 1, min = 0, max = max_i * variability[2])

    # calculate sine curve; vertical shift to make sure x > 0
    # amplitude * sin(period * (x + phase)) + vertical
    values_temp <- amplitude_temp * sin(period * (timesteps + phase_temp)) + input_mn

    # save values for resulting object
    values_input[[i]] <- values_temp

    amplitude_i[i] <- amplitude_temp

    phase_i[i] <- phase_temp

    if (any(values_temp < 0)) {

      warning("Negative input value created. Please check arguments.", call. = FALSE)

    }
  }

  if (!is.null(amplitude_mod) && variability[1] != 0 && verbose) {

    warning("Using 'amplitude_mod' instead of variability.", call. = FALSE)

  }

  # set names
  names(values_input) <- paste0("Meta_", 1:n)

  names(amplitude_i) <- paste0("Meta_", 1:n)

  names(phase_i) <- paste0("Meta_", 1:n)

  # store results in final list
  result_list <- list(values = values_input, n = n, max_i = max_i, freq_mn = freq_mn,
                      amplitude_i = amplitude_i, phase_i = phase_i, variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
