#' filter_meta
#'
#' @description
#' Filter meta objects
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param filter Vector with timesteps (\code{nutr_input}) or
#' min/max timesteps (\code{nutr_input}) to return.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Filter the nutrient input of each local metaecosystem to only include timesteps
#' specified by the corresponding function argument. This can be important, if during
#' the model run not all timesteps are returned in resulting object.
#'
#' To filter a simulated metaecosystem object, the minimum and maximum of the timesteps
#' to be returned needs to be provided.
#'
#' @return nutr_input, meta_rn
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' filter_meta(nutr_input, filter = seq(from = 4380 / 2, to = 4380, by = 20))
#'
#' \dontrun{
#' filter_metarn(result = result_rand, filter = c(4380 / 2, 4380))
#' }
#'
#' @aliases filter_meta
#' @rdname filter_meta
#'
#' @export
filter_meta <- function(x, filter, verbose) UseMethod("filter_meta")

#' @name filter_meta
#' @export
filter_meta.nutr_input <- function(x, filter, verbose = TRUE) {

  # check if all timesteps are integer
  if (!is.integer(filter)) {

    # convert to integer removing all digits
    filter <- as.integer(filter)

    # print warning
    if (verbose) {

      warning("'timesteps' are no integer values. All values will be truncated.",
              call. = FALSE)

    }
  }

  # check if all timesteps are within boundaries
  if (any(filter <= 0) || any(filter > x$max_i)) {

    stop("'timesteps' is not within 0 <= x < max_i.", call. = FALSE)

  }

  # loop through all metaecosystems
  for (i in 1:x$n) {

    # filter values and replace
    x$values[[i]] <- x$values[[i]][filter ,]

  }

  # replace maximum value
  x$max_i <- max(filter)

  return(x)
}

#' @name filter_meta
#' @export
filter_meta.meta_rn <- function(x, filter, verbose = NULL) {

  # check if mdl_rn is provided
  if (!inherits(x = x, what = "meta_rn")) {

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

  for (i in 1:x$n) {

    # get row id if timesteps that are selected
    seafloor_id <- which(x$seafloor[[i]]$timestep >= filter[1] &
                           x$seafloor[[i]]$timestep <= filter[2])

    # get row id if timesteps that are selected
    fishpop_id <- which(x$fishpop[[i]]$timestep >= filter[1] &
                          x$fishpop[[i]]$timestep <= filter[2])

    # subset data.frame
    x$seafloor[[i]] <- x$seafloor[[i]][seafloor_id, ]

    # subset data.frame
    x$fishpop[[i]] <- x$fishpop[[i]][fishpop_id, ]

  }

  # replace elements
  x$max_i <- filter[2]

  return(x)
}

