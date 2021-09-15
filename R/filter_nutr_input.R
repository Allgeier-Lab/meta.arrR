#' filter_nutr_input
#'
#' @description
#' Filter nutrient input
#'
#' @param nutr_input \code{nutr_input} object.
#' @param timesteps Vector with timesteps to return.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Filter the nutrient input of each local metaecosystem to only include timesteps
#' specified by the corresponding function argument. This can be important, if during
#' the model run not all timesteps are returned in resulting object.
#'
#' @return nutr_input
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' filter_nutr_input(nutr_input, timesteps = seq(from = 1, to = 4380, by = 50))
#'
#' @aliases filter_nutr_input
#' @rdname filter_nutr_input
#'
#' @export
filter_nutr_input <- function(nutr_input, timesteps, verbose = TRUE) {

  # check if all timesteps are integer
  if (!is.integer(timesteps)) {

    # convert to integer removing all digits
    timesteps <- as.integer(timesteps)

    # print warning
    if (verbose) {

      warning("'timesteps' are no integer values. All values will be truncated.",
              call. = FALSE)

    }
  }

  # check if all timesteps are within boundaries
  if (any(timesteps <= 0) || any(timesteps > nutr_input$max_i)) {

    stop("'timesteps' is not within 0 <= x < max_i.", call. = FALSE)

  }

  # loop through all metaecosystems
  for (i in 1:nutr_input$n) {

    # get current values
    values_temp <- nutr_input$values[[i]]

    # filter values and replace
    nutr_input$values[[i]] <- values_temp[timesteps]

  }

  # replace timesteps vector
  nutr_input$timesteps <- timesteps

  # replace maximum value
  nutr_input$max_i <- max(timesteps)

  return(nutr_input)
}
