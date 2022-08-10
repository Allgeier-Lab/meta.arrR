#' simulate_nutrient_noise
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param frequency Numeric frequency.
#' @param input_mn Numeric with mean input amount and frequency.
#' @param amplitude_mn Numeric with mean amplitude and phase.
#' @param noise_sd Numeric with noise function sd.
#' @param n_noise Integer with number of noise functions
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on Perlin noise.
#'
#' @return nutr_input
#'
#' @examples
#' nutrients_input <- simulate_nutrient_noise(n = 3, max_i = 4380, frequency = 5, input_mn = 1,
#' amplitude_mn = 0.5, noise_sd = 1.0)
#' plot(nutrients_input)
#'
#' @aliases simulate_nutrient_noise
#' @rdname simulate_nutrient_noise
#'
#' @export
simulate_nutrient_noise <- function(n, max_i, frequency = 0.0, input_mn = 0.0,
                                    amplitude_mn = 0.0, noise_sd = 0.0, n_noise = 3,
                                    verbose = TRUE) {

  # check amplitude argument
  if (amplitude_mn < 0.0 || amplitude_mn > 1.0) {

    stop("'amplitude_mn' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # check noise_sd argument
  if (noise_sd < 0.0 || noise_sd > 1.0) {

    stop("'noise_sd' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # create vector from 1 to max_i for nutrient input
  timesteps <- seq(from = 1, to = max_i, by = 1)

  # init results
  values_input <- vector(mode = "list", length = n)

  for (i in 1:n) {

    # init list for noise sine curves
    input_temp <- vector(mode = "list", length = n_noise)

    for (j in 1:n_noise) {

      # draw random numbers from norm distribution
      amplitude_mod <- arrR:::rcpp_rnorm(mean = amplitude_mn, sd = noise_sd, min = 0.0, max = 1.0)

      period_mod <- arrR:::rcpp_rnorm(mean = 1.0, sd = noise_sd, min = 0.0, max = Inf)

      # calculate period for number of input peaks (period = 2 * pi / b)
      period_temp <- (frequency / (max_i / (2 * pi))) * period_mod

      # calculate values for sine curve
      amplitude_temp <- input_mn * amplitude_mod

      # calculate phase
      phase_temp <- 0.0 # ifelse(test = period_temp == 0, yes = 0.0, no = ((2 * pi) / period_temp) * phase_mod)

      # calculate sine curve; vertical shift to make sure x > 0
      # amplitude * sin(period * (x + phase)) + vertical
      input_temp[[j]] <- amplitude_temp * sin(period_temp * (timesteps + phase_temp)) + input_mn

    }

    # get mean of noise curves
    values_temp <- apply(X = do.call("cbind", input_temp), MARGIN = 1, FUN = "mean")

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
