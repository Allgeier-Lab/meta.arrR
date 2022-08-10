#' simulate_nutrient_sine
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
#' @param noise_sd Numeric with noise added to sine functions.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on sine curves. The \code{frequency} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_mn} argument quantifies the
#' mean input values. Amplitude and phase mean and sd can be controlled by the corresponding
#' arguments.
#'
#' @return nutr_input
#'
#' @examples
#' nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, frequency = 5, input_mn = 1,
#' amplitude_mn = 0.5)
#' plot(nutrients_input)
#'
#' @aliases simulate_nutrient_sine
#' @rdname simulate_nutrient_sine
#'
#' @export
simulate_nutrient_sine <- function(n, max_i, frequency = 0.0, input_mn = 0.0,
                                   amplitude_mn = 0.0, phase_mn = 0.0,
                                   amplitude_sd = 0.0, phase_sd = 0.0,
                                   noise_sd = 0.0, verbose = TRUE) {

  # check amplitude argument
  if (amplitude_mn < 0.0 || amplitude_mn > 1.0) {

    stop("'amplitude_mn' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # check phase_mn argument
  if (phase_mn < 0.0 || phase_mn > 1.0) {

    stop("'phase_mn' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # check noise_sd argument
  if (noise_sd < 0.0 || noise_sd > 1.0) {

    stop("'noise_sd' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # create vector from 1 to max_i for nutrient input
  timesteps <- seq(from = 1, to = max_i, by = 1)

  # init results
  values_input <- vector(mode = "list", length = n)

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

    # calculate phase
    phase_temp <- ifelse(test = period == 0, yes = 0.0,
                         no = ((2 * pi) / period) * phase_mod)

    # calculate sine curve; vertical shift to make sure x > 0
    # amplitude * sin(period * (x + phase)) + vertical
    values_temp <- amplitude_temp * sin(period * (timesteps + phase_temp)) + input_mn

    if (noise_sd > 0) {

      # calculate number of values of full frequency
      frequency_full <- max_i / frequency

      # first ald last full frequency are shifted based on phase; create id vector
      frequency_id <- rep(x = c(1:frequency, 1),
                          times = c(ceiling(frequency_full * (1 - phase_mod)),
                                    rep(x = frequency_full, times = frequency - 1),
                                    floor(frequency_full * phase_mod)))

      # split max_i into frequency chunks
      chunks <- split(timesteps, frequency_id)

      # loop through all chunks
      for (j in seq_along(chunks)) {

        # get values of chunk
        values_chunk <- values_temp[chunks[[j]]]

        noise_max <- (1.0 - amplitude_mod) /  amplitude_mod

        # sample random noise modifier
        noise_rnd <- stats::runif(n = 1, min = -noise_sd, max = ifelse(test = noise_sd > noise_max,
                                                                       yes = noise_max, no = noise_sd))

        # calculate curve factor (values_temp - input_mn) / (max(values_temp) - input_mn)
        noise_factor <- (values_chunk - input_mn) / (max(values_chunk) - input_mn)

        # change values of current chunk
        values_temp[chunks[[j]]] <- values_chunk + (amplitude_temp * (noise_rnd * noise_factor))

      }
    }

    # check if any values are below zero or above amplitude
    if (any(values_temp < 0) && verbose) {

      stop("Negative input value created. Please check arguments.", call. = FALSE)

    }

    # save values for resulting object
    values_input[[i]] <- data.frame(timestep = timesteps, input = values_temp)

  }

  # set names
  names(values_input) <- paste0("meta_", 1:n)

  # store results in final list
  result_list <- list(values = values_input, n = n, max_i = max_i)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
