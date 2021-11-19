#' create_attributes
#'
#' @description
#' Create attribute values.
#'
#' @param fishpop List with fish population
#' @param parameters List with parameters
#' @param max_i Integer with maximum number of simulation time steps.
#'
#' @details
#' Creates matrix with id, residence, and reserves threshold value for each
#' individual of fish population.
#'
#' @return matrix
#'
#' @examples
#' \dontrun{
#' create_attributes(fishpop = fishpop_list, parameters = parameters, max_i = 4380)
#' }
#'
#' @aliases create_attributes
#' @rdname create_attributes
#'
#' @keywords internal
create_attributes <- function(fishpop, parameters, max_i) {

  # loop through all metaecosystems
  result <- do.call(rbind, lapply(fishpop, function(i) {

    # at least one fish present
    if (!all(is.na(i))) {

      # get number of individuals
      pop_n <- nrow(i)

      # return 0 for mean is zero
      if (parameters$move_residence == 0) {

        residence <- rep(x = 0.0, times = pop_n)

      # if create random number if mean != 0
      } else {

        # draw from rlognorm with Inf maximum
        residence <- vapply(1:pop_n, function(i)
          arrR:::rcpp_rlognorm(mean = parameters$move_residence,
                               sd = parameters$move_residence * parameters$move_residence_var,
                               min = 0, max = max_i),
          FUN.VALUE = numeric(1))

      }

      # sample random pop_reserves_thres value
      reserves_thres <- stats::runif(n = pop_n, min = parameters$pop_reserves_thres_lo,
                                     max = parameters$pop_reserves_thres_hi)

      # combine to one matrix
      cbind(id = i[, 1], residence = residence, reserves_thres = reserves_thres)

    # no fish present
    } else {

      cbind(id = numeric(), residence = numeric(), reserves_thres = numeric())

    }
  }))

  return(result)
}
