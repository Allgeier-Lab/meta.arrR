#' filter_meta
#'
#' @description
#' Filter meta objects
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param filter Vector with timesteps (\code{nutr_input}) or
#' min/max timesteps (\code{nutr_input}) to return.
#' @param reset Logical if TRUE, cumulative seafloor values are reduced by value
#' before filter minimum.
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
filter_meta <- function(x, filter, reset, verbose) UseMethod("filter_meta")

#' @name filter_meta
#' @export
filter_meta.nutr_input <- function(x, filter, reset = NULL, verbose = TRUE) {

  # repeat filter
  if (length(filter == 1)) {

    filter <- rep(x = filter, times = 2)

  }

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
filter_meta.meta_rn <- function(x, filter, reset = FALSE, verbose = TRUE) {

  # repeat filter
  if (length(filter == 1)) {

    filter <- rep(x = filter, times = 2)

  }

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

  # prepare some things for cols reset
  if (reset && filter[1] > 0) {

    # create vector with timesteps i
    timestep_full <- seq(from = 0, to = x$max_i, by = x$save_each)

    # get last timestep before filter
    timestep_last <- timestep_full[max(which(timestep_full < filter[1]))]

    # create vector with seafloor cols
    cols_seafloor <- c("x", "y", "ag_production", "bg_production", "ag_slough", "bg_slough",
                       "ag_uptake", "bg_uptake", "consumption", "excretion")

    # check if fishpop is present
    if (any(x$starting_values$pop_n != 0)) {

      # create vector with fishpop cols
      cols_fishpop <- c("id", "consumption", "excretion", "died_consumption", "died_background")

      # create look up for fish at timestep last
      fishpop_last <- do.call(what = "rbind", args = lapply(X = x$fishpop, function(i) {
        i[i$timestep == timestep_last, cols_fishpop]}))

      # order rows
      fishpop_last <- fishpop_last[order(fishpop_last$id), ]

    }
  }

  for (i in 1:x$n) {

    # get seafloor value at last timestep
    if (reset && filter[1] > 0) {

      # get values of last timestep
      seafloor_last <- x$seafloor[[i]][x$seafloor[[i]]$timestep == timestep_last,
                                       cols_seafloor]

    }

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

    # subtract all cumulative number until filter cutoff
    if (reset && filter[1] > 0) {

      # get row ids where seafloor_last xy equals seafloor xy
      seafloor_rows <- apply(X = x$seafloor[[i]][, c("x", "y")], MARGIN = 1,
                             FUN = function(j) {which(j[1] == seafloor_last$x &
                                                        j[2] == seafloor_last$y)})

      # update cols seafloor
      x$seafloor[[i]][, cols_seafloor[-c(1, 2)]] <- x$seafloor[[i]][, cols_seafloor[-c(1, 2)]] -
        seafloor_last[seafloor_rows, cols_seafloor[-c(1, 2)]]

      # check if fishpop is present
      if (any(x$starting_values$pop_n != 0)) {

        # get row ids where fishpop_last id equals fishop id
        fishpop_rows <- sapply(X = x$fishpop[[i]]$id, function(j) which(j == fishpop_last$id))

        # update cols fishpop
        x$fishpop[[i]][, cols_fishpop[-1]] <- x$fishpop[[i]][, cols_fishpop[-1]] -
          fishpop_last[fishpop_rows, cols_fishpop[-1]]

      }
    }

    # print progress
    if (verbose) {

      message("\r> Progress: ", round(x = i / x$n, digits = 2) * 100, "% \t \t",
              appendLF = FALSE)

    }
  }

  # filter input
  x$nutr_input <- filter_meta.nutr_input(x = x$nutr_input, filter = filter,
                                         verbose = FALSE)

  # replace elements
  x$max_i <- max(filter)

  # print new line
  if (verbose) {

    message("")

  }

  return(x)
}

