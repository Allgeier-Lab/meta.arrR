#' simulate_nutr_input
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param frequency Numeric frequency.
#' @param input_mn Numeric with mean input amount and frequency.
#' @param amplitude_mn,phase_mn Numeric with mean amplitude and phase.
#' @param amplitude_sd,phase_sd Numeric with sd amplitude and phase.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on sine curves. The \code{frequency} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_mn} argument quantifies the
#' mean input values. Ampliude and phase mean and sd can be controlled by the corresponding
#' arguments.
#'
#' @return nutr_input
#'
#' @examples
#' nutrients_input <- simulate_nutr_input(n = 3, max_i = 4380, frequency = 5, input_mn = 1)
#'
#' @aliases simulate_nutr_input
#' @rdname simulate_nutr_input
#'
#' @export
simulate_nutr_input <- function(n, max_i, frequency = 0.0, input_mn = 0.0,
                                amplitude_mn = 0.0, phase_mn = 0.0,
                                amplitude_sd = 0.0, phase_sd = 0.0, verbose = TRUE) {

  # check amplitude argument
  if (amplitude_mn < 0.0 || amplitude_mn > 1.0) {

    stop("'amplitude_mn' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # check phase_mn argument
  if (phase_mn < 0.0 || phase_mn > 1.0) {

    stop("'phase_mn' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # create vector from 1 to max_i for nutrient input
  timesteps <- seq(from = 1, to = max_i, by = 1)

  # init results
  values_input <- vector(mode = "list", length = n)

  amplitude_i <- vector(mode = "numeric", length = n)

  phase_i <- vector(mode = "numeric", length = n)

  # calculate period for number of input peaks (period = 2 * pi / b)
  period <- frequency / (max_i / (2 * pi))

  for (i in 1:n) {

    # draw random numbers from norm distribution
    amplitude_mod <- arrR:::rcpp_rnorm(mean = amplitude_mn, sd = amplitude_sd,
                                       min = 0.0, max = 1.0)

    phase_mod <- arrR:::rcpp_rnorm(mean = phase_mn, sd = phase_sd,
                                   min = 0.0, max = 1.0)

    # calculate values for sine curve
    amplitude_temp <- input_mn * amplitude_mod

    phase_temp <- ((2 * pi) / period) * phase_mod

    # set phase to 0 if NaN
    phase_temp <- ifelse(test = is.finite(phase_temp), yes = phase_temp, no = 0)

    # calculate sine curve; vertical shift to make sure x > 0
    # amplitude * sin(period * (x + phase)) + vertical
    values_temp <- amplitude_temp * sin(period * (timesteps + phase_temp)) + input_mn

    # save values for resulting object
    values_input[[i]] <- data.frame(timestep = timesteps,
                                    input = values_temp)

    amplitude_i[i] <- amplitude_mod

    phase_i[i] <- phase_mod

    if (any(values_temp < 0) && verbose) {

      stop("Negative input value created. Please check arguments.", call. = FALSE)

    }
  }

  # set names
  names(values_input) <- paste0("meta_", 1:n)

  names(amplitude_i) <- paste0("meta_", 1:n)

  names(phase_i) <- paste0("meta_", 1:n)

  # store results in final list
  result_list <- list(values = values_input, n = n, max_i = max_i, frequency = frequency,
                      amplitude = amplitude_i, phase = phase_i)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
