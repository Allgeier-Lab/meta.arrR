#' create_attributes
#'
#' @description Create attribute values
#'
#' @param fishpop List with fish population
#' @param parameters List with parameters
#'
#' @details
#' Creates matrix with id and random stationary value for each individual of fishpopulation.
#'
#' @return matrix
#'
#' @examples
#' \dontrun{
#' create_attributes(fishpop = fishpop_list, parameters = parameters)
#' }
#'
#' @aliases create_attributes
#' @rdname create_attributes
#'
#' @export
create_attributes <- function(fishpop, parameters) {

  # loop through all metaecosyst
  result <- do.call(rbind, lapply(fishpop, function(i) {

    # get number of individuals
    pop_n <- nrow(i)

    # return 0 for mean is zero
    if (parameters$move_stationary == 0) {

      stationary <- rep(x = 0.0, times = pop_n)

    # if create random number if mean != 0
    } else {

      stationary <- vapply(1:pop_n, function(i)
        arrR::rcpp_rlognorm(mean = parameters$move_stationary,
                            sd = sqrt(parameters$move_stationary_var),
                            min = 0, max = Inf),
        FUN.VALUE = numeric(1))

    }

    reserves_thres <- stats::runif(n = pop_n, min = parameters$pop_reserves_thres_lo,
                                   max = parameters$pop_reserves_thres_hi)



    cbind(id = i[, 1], stationary = stationary, reserves_thres = reserves_thres)}))

  return(result)
}
