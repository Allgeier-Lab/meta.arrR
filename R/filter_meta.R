#' filter_meta
#'
#' @description
#' Filter meta objects
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param filter Integer with one or vector with min/max timestep(s) to filter.
#' @param reset Logical if TRUE, cumulative seafloor values are reduced by value
#' before filter minimum.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Filter the nutrient input of each local metaecosystem to only include timesteps
#' specified by the corresponding function argument. This can be important, if during
#' the model run not all timesteps are returned in resulting object.
#'
#' @return nutr_input, meta_rn
#'
#' @examples
#' nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, input_mn = 1,
#' frequency = 3, amplitude_sd = 0.5)
#' filter_meta(x = nutrients_input, filter = c(4380 / 2, 4380))
#'
#' \dontrun{
#' filter_meta(x = result_rand, filter = c(4380 / 2, 4380))
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
  if (length(filter) == 1) {

    filter <- rep(x = filter, times = 2)

  } else if (length(filter) != 2) {

    stop("'filter' must be either one timestep or min/max timesteps.", call. = FALSE)

  }

  # check if all timesteps are integer
  if (!is.integer(filter)) {

    # convert to integer removing all digits
    filter <- as.integer(filter)

    # print warning
    if (verbose) {

      warning("'filter' are no integer values. All values will be truncated.",
              call. = FALSE)

    }
  }

  # check if all timesteps are within boundaries
  if ((filter[1] < 0) || (filter[2] > x$max_i)) {

    stop("'filter' is not within 0 <= x <= max_i.", call. = FALSE)

  }

  # loop through all metaecosystems
  for (i in 1:x$n) {

    # get row id if timesteps that are selected
    nutrient_id <- which(x$values[[i]]$timestep >= filter[1] &
                           x$values[[i]]$timestep <= filter[2])

    # filter values and replace
    x$values[[i]] <- x$values[[i]][nutrient_id ,]

  }

  x$max_i <- unique(vapply(X = x$values, FUN = function(i) max(i$timestep),
                           FUN.VALUE = numeric(1)))

  return(x)
}

#' @name filter_meta
#' @export
filter_meta.meta_rn <- function(x, filter, reset = FALSE, verbose = TRUE) {

  # repeat filter
  if (length(filter) == 1) {

    filter <- rep(x = filter, times = 2)

  } else if (length(filter) != 2) {

    stop("'filter' must be either one timestep or min/max timesteps.", call. = FALSE)

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
  if ((filter[1] < 0) || (filter[2] > x$max_i)) {

    stop("'filter' is not within 0 <= x <= max_i.", call. = FALSE)

  }

  # prepare some things for cols reset
  if (reset && filter[1] > 0) {

    # create vector with seafloor cols
    cols_seafloor <- c("x", "y", "ag_production", "bg_production", "ag_slough", "bg_slough",
                       "ag_uptake", "bg_uptake", "consumption", "excretion")

    # create vector with fishpop cols
    cols_fishpop <- c("id", "consumption", "excretion", "died_consumption", "died_background",
                      "moved")

    # create vector with timesteps i
    timestep_full <- seq(from = 0, to = x$max_i, by = x$save_each)

    # get last timestep before filter
    timestep_last <- timestep_full[max(which(timestep_full < filter[1]))]

    # get row ids where seafloor_last xy equals seafloor xy
    seafloor_rows <- rep(x = seq(from = 1, to = prod(x$dimensions)),
                         times = length(which(timestep_full >= filter[1] &
                                                timestep_full <= filter[2])))

    # get values of last timestep
    seafloor_last <- lapply(X = x$seafloor, function(i)
      i[i$timestep == timestep_last, cols_seafloor])

    # create look up for fish at timestep last
    fishpop_last <- do.call(what = "rbind", args = lapply(X = x$fishpop, function(i) {
      i[i$timestep == timestep_last, cols_fishpop]}))

    # order rows
    fishpop_last <- fishpop_last[order(fishpop_last$id), ]

  }

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

    # subtract all cumulative number until filter cutoff
    if (reset && filter[1] > 0) {

      # update cols seafloor
      x$seafloor[[i]][, cols_seafloor[-c(1, 2)]] <- x$seafloor[[i]][, cols_seafloor[-c(1, 2)]] -
        seafloor_last[[i]][seafloor_rows, -c(1, 2)]

      # get row ids where fishpop_last id equals fishop id
      fishpop_rows <- sapply(X = x$fishpop[[i]]$id, function(j) {

        # returns numeric(0) if row is NA
        row_temp <- which(j == fishpop_last$id)

        # return NA if numeric(0)
        ifelse(test = length(row_temp) == 0, yes = NA, no = row_temp)

      })

      # update cols fishpop
      x$fishpop[[i]][, cols_fishpop[-1]] <- x$fishpop[[i]][, cols_fishpop[-1]] -
        fishpop_last[fishpop_rows, cols_fishpop[-1]]

    }

    # print progress
    if (verbose) {

      message("\r> Progress filtering: ", round(x = i / x$n * 100, digits = 2), "% \t \t",
              appendLF = FALSE)

    }
  }

  # filter input
  x$nutrients_input <- filter_meta.nutr_input(x = x$nutrients_input, filter = filter,
                                              verbose = FALSE)


  # replace elements
  x$max_i <- unique(vapply(X = x$seafloor, FUN = function(i) max(i$timestep),
                           FUN.VALUE = numeric(1)))

  # print new line
  if (verbose) {

    message("")

  }

  return(x)
}

