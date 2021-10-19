#' filter_metarn
#'
#' @description
#' Filter model run for specific timestep.
#'
#' @param result meta_rn object of simulation run.
#' @param filter Vector with timesteps to select
#'
#' @details
#' This functions allows to return only specific timesteps of a \code{meta_rn} object
#' created with \code{\link{run_meta}}. The function ensures that the object will
#' still be a \code{meta_rn} object.
#'
#' @return meta_rn
#'
#' @examples
#'  \dontrun{
#' filter_metarn(result = result_rand, timestep = 10200)
#' }
#'
#' @aliases filter_metarn
#' @rdname filter_metarn
#'
#' @export
filter_metarn <- function(result, filter = c(0, result$max_i)) {

  # check if mdl_rn is provided
  if (!inherits(x = result, what = "meta_rn")) {

    stop("Please provide 'meta_rn' object created with 'run_simulation'.", call. = FALSE)

  }

  # repeat filter
  if (length(filter == 1)) {

    filter <- rep(x = filter, times = 2)

  }

  # # check if i can be divided by save_each without reminder
  # if (timestep_slctd %% x$save_each != 0 || timestep_slctd > x$max_i) {
  #
  #   stop("'timestep' was not saved during model run.",
  #        call. = FALSE)
  #
  # }

  for (i in 1:result$n) {

    # get row id if timesteps that are selected
    seafloor_id <- which(result$seafloor[[i]]$timestep >= filter[1] &
                           result$seafloor[[i]]$timestep <= filter[2])

    # get row id if timesteps that are selected
    fishpop_id <- which(result$fishpop[[i]]$timestep >= filter[1] &
                          result$fishpop[[i]]$timestep <= filter[2])

    # subset data.frame
    result$seafloor[[i]] <- result$seafloor[[i]][seafloor_id, ]

    # subset data.frame
    result$fishpop[[i]] <- result$fishpop[[i]][fishpop_id, ]

  }

  # replace elements
  result$max_i <- filter[2]

  return(result)
}
