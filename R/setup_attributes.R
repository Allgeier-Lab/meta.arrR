#' setup_attributes
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
#' setup_attributes(fishpop = fishpop_list, parameters = parameters, max_i = 4380)
#' }
#'
#' @aliases setup_attributes
#' @rdname setup_attributes
#'
#' @keywords internal
setup_attributes <- function(fishpop, parameters, max_i) {

  # loop through all metaecosystems
  result <- do.call(rbind, lapply(fishpop, function(i) {

    # at least one fish present
    if (!all(is.na(i))) {

      # get number of individuals
      pop_n <- nrow(i)

      # draw from rnorm with Inf maximum
      move_prob <- vapply(1:pop_n, function(i)
        arrR:::rcpp_rnorm(mean = parameters$move_meta_mean, sd = parameters$move_meta_sd,
                          min = 0.0, max = 1.0),
        FUN.VALUE = numeric(1))

      # sample random pop_reserves_thres value
      reserves_thres <- vapply(1:pop_n, function(i)
        arrR:::rcpp_rnorm(mean = parameters$pop_reserves_thres_mean, sd = parameters$pop_reserves_thres_sd,
                          min = 0.0, max = 1.0), FUN.VALUE = numeric(1))

      # combine to one matrix
      cbind(id = i[, 1], reserves_thres = reserves_thres, move_prob = move_prob)

    # no fish present
    } else {

      cbind(id = numeric(), reserves_thres = numeric(), move_prob = numeric())

    }
  }))

  return(result)
}
