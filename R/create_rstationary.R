#' create_rstationary
#'
#' @description Create random stationary values
#'
#' @param fishpop_values List with fishpopulation
#' @param mean,sd Numeric with mean and sd of log-norm distribution.
#'
#' @details
#' Creates matrix with id and random stationary value for each individual of fishpopulation.
#'
#' @return matrix
#'
#' @examples
#' # Add example code
#'
#' @aliases create_rstationary
#' @rdname create_rstationary
#'
#' @export
create_rstationary <- function(fishpop_values, mean, sd) {

  # loop through all metaecosyst
  result <- do.call(rbind, lapply(fishpop_values, function(i) {

    # get number of individuals
    pop_n <- nrow(i)

    # return 0 for mean is zero
    if (mean == 0) {

      value_temp <- rep(x = 0, times = pop_n)

    # if create random number if mean != 0
    } else {

      value_temp <- arrR::rlognorm(n = pop_n, mean = mean, sd = sd)

    }

    cbind(id = i[, 1], value = value_temp)}))

  return(result)
}
