#' simulate_input_sine
#'
#' @description Initiate fish population.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param timestep Vector timesteps
#' @param period_mn,period_sd Numeric with sine curve parameters.
#' @param amplitude_mn,amplitude_sd Numeric variability of parameters.
#' @param phase Numeric with sine curve parameters.
#' @param simplify Logical if only vector should be returned.
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
                                phase = 0, simplify = FALSE) {

  result_list <- vector(mode = "list", length = n)

  for (i in seq_along(result_list)) {

    period_temp <- abs(rnorm(n = 1, mean = period_mn, sd = period_mn * period_sd))

    # print(paste0("period_temp: ", period_temp))

    amplitude_temp <- rnorm(n = 1, mean = amplitude_mn, sd = amplitude_mn * amplitude_sd)

    # print(paste0("amplitude_temp: ", amplitude_temp))

    vert_temp <- amplitude_temp

    # amplitude * sin(period * (x + phase)) + vert
    input_temp <- amplitude_temp * sin(period_temp * (timestep + phase)) + vert_temp

    data_temp <- data.frame(id = factor(i), timestep = timestep, input = input_temp)

    if (simplify) {

      data_temp <- data_temp$input

    }

    result_list[[i]] <- data_temp

  }

  return(result_list)

}
