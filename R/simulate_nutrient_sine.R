#' simulate_nutrient_sine
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param frequency Numeric frequency.
#' @param input_mn Numeric with mean input amount and frequency.
#' @param noise Numeric with noise function sd.
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
#' nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, frequency = 5,
#' input_mn = 1, noise = 0.5)
#' plot(nutrients_input)
#'
#' @aliases simulate_nutrient_sine
#' @rdname simulate_nutrient_sine
#'
#' @export
simulate_nutrient_sine <- function(n, max_i, frequency = 0.0, input_mn = 0.0,
                                   noise = 0.0, verbose = TRUE) {

  # check noise argument
  if (noise < 0.0 || noise > 1.0) {

    warning("'noise' should be 0.0 <= x <= 1.0", call. = FALSE)

  }

  # create vector from 1 to max_i for nutrient input
  timesteps <- seq(from = 1, to = max_i, by = 1)

  # init results
  values_input <- vector(mode = "list", length = n)

  for (i in 1:n) {

    # draw random numbers from norm distribution to modify amplitude
    amplitude_mod <- stats::rnorm(n = 1, mean = 0.0, sd = noise) # *
    # sample(x = c(-1, 1), size = 1, prob = c(noise * (0.5 / 1), 1 - noise * (0.5 / 1)))

    # calculate values for sine curve
    amplitude_temp <- input_mn * amplitude_mod

    # draw random number from norm distribution to modify phase
    period_mod <- stats::rnorm(n = 1, mean = 1.0, sd = noise)

    # calculate period for number of input peaks (period = 2 * pi / b)
    period_temp <- (frequency / (max_i / (2 * pi))) * period_mod

    # calculate phase
    phase_temp <- 0.0
    # ifelse(test = period_temp == 0, yes = 0.0, no = ((2 * pi) / period_temp) * phase_mod)

    # calculate sine curve; vertical shift to make sure x > 0
    # amplitude * sin(period * (x + phase)) + vertical
    # values_temp <- amplitude_temp * sin(period * (timesteps + phase_temp)) + input_mn
    values_temp <- amplitude_temp * sin(period_temp * (timesteps + phase_temp)) + input_mn

    # check if any values are below zero or above amplitude
    if (any(values_temp < 0) || any(values_temp > 2 * input_mn)) {

      values_temp <- scales::rescale(values_temp, to = c(0, 2 * input_mn))

      if (verbose) warning("Rescaling nutrient inputs values to be 0 <= x <= 2 * input_mn.", call. = FALSE)

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
