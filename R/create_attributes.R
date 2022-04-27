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
          arrR:::rcpp_rnorm(mean = parameters$move_residence_mean, sd = parameters$move_residence_sd,
                            min = 1.0, max = max_i),
          FUN.VALUE = numeric(1))

      }

      # sample random pop_reserves_thres value
      reserves_thres <- vapply(1:pop_n, function(i) arrR:::rcpp_rnorm(parameters$pop_reserves_thres_mean,
                                                                      parameters$pop_reserves_thres_var, 0.0, 1.0),
                               FUN.VALUE = numeric(1))

      # combine to one matrix
      cbind(id = i[, 1], reserves_thres = reserves_thres, residence = residence)

    # no fish present
    } else {

      cbind(id = numeric(), residence = numeric(), reserves_thres = numeric())

    }
  }))

  return(result)
}
