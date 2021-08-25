#' sim_nutr_input
#'
#' @description
#' Simulate nutrient input.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param input_mn,freq_mn Numeric with mean input amount and frequency.
#' @param variability Variability of nutrient input.
#' @param method String to specify how variability is simulated ('noise' or 'sd').
#' @param n_noise Integer with number of sine curves used for noise.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Simulating nutrient input based on sine curves. The \code{freq_mn} argument quantifies
#' how many complete cycles of the sine function are present for a given \code{max_i},
#' i.e., how many "peaks" are present. The \code{input_mn} argument quantifies the
#' maximum value of the input "peaks". The default \code{method = 'noise'} uses the mean
#' of \code{n_noise} sine curves to generate a noise signal.
#'
#' If two \code{variability} parameters are provided, the first one is used for \code{input_mn},
#' the second one for \code{freq_mn}.
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#'
#' @aliases sim_nutr_input
#' @rdname sim_nutr_input
#'
#' @export
sim_nutr_input <- function(n, max_i, input_mn, freq_mn, variability = 0,
                           method = "noise", n_noise = NULL, verbose = TRUE) {

  # init list with values for each local metaecosyst
  result_values <- vector(mode = "list", length = n)

  # set names
  names(result_values) <- paste0("Metaecosystem_", 1:n)

  # create vector from 1 to max_i for nutrient input
  timestep <- 1:max_i

  # check how many variability parameters are provided
  if (length(variability) > 2) {

    stop("Please provide either one or two 'variability' parameters.",
         call. = FALSE)

  # repeate parameter if only one is present
  } else if (length(variability) == 1) {

    variability <- rep(x = variability, each = 2)

  }


  # check if n_noise is required and already provided
  if (method == "noise") {

    # set default n_noise
    if (is.null(n_noise)) {

      n_noise <- 3

    }

    # calculate needed numbers of draws
    n <- n * n_noise

  }

  # draw random amplitudes
  amplitude_rand <- abs(stats::rnorm(n = n, mean = input_mn, sd = input_mn * variability[1]))

  # draw random frequencies
  freq_rand <- abs(stats::rnorm(n = n, mean = freq_mn, sd = freq_mn * variability[2]))

  # calculate period for number of input peaks (period = 2 * pi / b)
  period_rand <- freq_rand / (max_i / (2 * pi))

  # set phase shift to zero
  if (variability[2] == 0) {

    phase_rand <- rep(x = 0, times = max_i)

  # draw random phase shifts
  } else {

    phase_rand <- stats::runif(n = n, min = 0, max = max_i * variability[2])

  }

  # use noise signal
  if (method == "noise") {

    # set counter needed for generated random values
    counter <- 0

    # loop through all metaecosystems
    for (i in seq_along(result_values)) {

      # init list for noise sine curves
      input_temp <- vector(mode = "list", length = n_noise)

      # loop through noise sine curves
      for (j in 1:n_noise) {

        # increase counter
        counter <- counter + 1

        # simulate sine curve: amplitude * sin(period * (x + phase)) + vertical
        input_values <- amplitude_rand[counter] *
          sin(period_rand[counter] * (timestep + phase_rand[counter])) + amplitude_rand[counter]

        input_temp[[j]] <- input_values

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

  # use standard deviation from mean method
  } else if (method == "sd") {

    # print information to console
    if (verbose) {

      warning("'method = sd' is deprecated and should not be used.", call. = FALSE)

      # return warning
      if (!is.null(n_noise)) {

        warning("'n_noise' is used for method = 'noise' only.", call. = FALSE)

      }
    }

    # loop through all metaecosystems
    for (i in seq_along(result_values)) {

      # amplitude * sin(period * (x + phase)) + vert
      # adding amplitude_temp again to make sure input >= 0.0
      input_temp <- amplitude_rand[i] *
        sin(period_rand[i] * (timestep + phase_rand[i])) + amplitude_rand[i]

      # check if any is negative
      if (any(input_temp < 0)) {

        stop("Negative input value created. Please check arguments.")

      }

      # store results in data.frame
      result_values[[i]] <- input_temp

    }

  # method selection is wrong
  } else {

    stop("Please select either method = 'noise' or method = 'sd'.", call. = FALSE)

  }

  # store results in final list
  result_list <- list(values = result_values,
                      input_mn = input_mn, freq_mn = freq_mn,
                      variability = variability)

  # specify class of list
  class(result_list) <- "nutr_input"

  return(result_list)

}
