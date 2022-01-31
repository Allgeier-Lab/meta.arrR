#' get_global_range
#'
#' @description
#' Get global range
#'
#' @param result List with \code{meta_rn} objects simulated with \code{sim_nutr_input_*}.
#' @param value Character with coloumn of seafloor to summarize.
#'
#' @details
#' Returns the minimum and maximum of selected seafloor value of several model run results
#' created with \code{run_simulation_meta}.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' get_global_range(result = list(result_rand, result_rand), value = "ag_biomass")
#' }
#'
#' @aliases get_global_range
#' @rdname get_global_range
#'
#' @export
get_global_range <- function(result, value) {

  # check input
  if (!inherits(x = result, what = "list")) {

    stop("Please provide list with 'meta_rn' objects.", call. = FALSE)

  }

  if (!all(vapply(result, FUN = inherits, FUN.VALUE = logical(1), what = "meta_rn"))) {

    stop("Please provide list with 'meta_rn' objects.", call. = FALSE)

  }

  # get min max values
  min_max <- range(lapply(result, function(i) {

    lapply(i$seafloor, function(j) {

      range(j[, value], na.rm = TRUE)

    })
  }))

  return(min_max)
}
