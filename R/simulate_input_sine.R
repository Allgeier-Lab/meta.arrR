#' simulate_input_sine
#'
#' @description Initiate fish population.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param timestep Vector timesteps
#' @param period_mn,period_sd Numeric with sine curve parameters.
#' @param amplitude_mn,amplitude_sd Numeric variability of parameters.
#' @param phase Numeric with sine curve parameters.
#' @param return_df Logical if data.frame is returned.
#'
#' @details
#' ADD TEXT
#'
#' @return vector
#'
#' @examples
#' # Add example code
#'
#' @aliases simulate_input_sine
#' @rdname simulate_input_sine
#'
#' @export
simulate_input_sine <- function(n, timestep,
                                period_mn, period_sd, amplitude_mn, amplitude_sd,
                                phase = 0, return_df = FALSE) {

  result_list <- vector(mode = "list", length = n)

  for (i in seq_along(result_list)) {

    period_temp <- abs(stats::rnorm(n = 1, mean = period_mn, sd = period_mn * period_sd))

    amplitude_temp <- stats::rnorm(n = 1, mean = amplitude_mn, sd = amplitude_mn * amplitude_sd)

    vert_temp <- amplitude_temp

    # amplitude * sin(period * (x + phase)) + vert
    input_temp <- amplitude_temp * sin(period_temp * (timestep + phase)) + vert_temp

    result_list[[i]] <- data.frame(meta = factor(i), timestep = timestep, input = input_temp)

  }

  if (!return_df) {

    result_list <- lapply(result_list, function(i) i$input)


  } else {

    result_list <- do.call(rbind, result_list)

  }

  return(result_list)

}
