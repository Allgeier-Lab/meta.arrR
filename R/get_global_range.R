#' get_global_range
#'
#' @description
#' Get global range
#'
#' @param result List with \code{meta_rn} objects simulated with \code{sim_nutr_input_*}.
#' @param value Character with coloumn of seafloor to summarize.
#' @param timestep Numeric with time step to select
#'
#' @details
#' Returns the minimum and maximum of selected seafloor value of several model run results
#' created with \code{run_meta}.
#'
#' @return vector
#'
#' @examples
#' \dontrun{
#' get_global_range(result = result_rand)
#' }
#'
#' @aliases get_global_range
#' @rdname get_global_range
#'
#' @export
get_global_range <- function(result, value, timestep = NULL) {

  # check input
  if (!inherits(x = result, what = "list")) {

    stop("Please provide list with meta_rn objects.", call. = FALSE)

  }

  if (!all(vapply(result, FUN = inherits, FUN.VALUE = logical(1), what = "meta_rn"))) {

    stop("Please provide list with meta_rn objects.", call. = FALSE)

  }

  # get maximum max_i
  if (is.null(timestep)) {

    # get timestep to filter later
    timestep_slctd <- result[[1]]$max_i

  }

  # check if timestep is present
  if (all(vapply(result, FUN = function(i) timestep_slctd %% i$save_each != 0,
                 FUN.VALUE = logical(1)))) {

    stop("'timestep' was not saved during all model runs.",
         call. = FALSE)

  }

  # get min max values
  min_max <- range(lapply(result, function(i) {

    lapply(i$seafloor, function(j) {

      range(subset(j, timestep == timestep_slctd, select = value), na.rm = TRUE)

    })
  }))

  return(min_max)
}
