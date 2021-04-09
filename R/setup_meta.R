#' setup_meta
#'
#' @description Initiate fish population.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param extent Vector with number of rows and columns (spatial extent).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param reefs 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value.
#' @param parameters List with all parameters.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#' @param use_log Logical if TRUE, random log distribution is used
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{\link{raster}}.
#'
#' @details
#' Function to setup the fish population. If no fish shoud be created, set
#' \code{starting_values$pop_n = 0}.
#'
#' @return meta_sys
#'
#' @examples
#' # Add example code
#'
#' @aliases setup_meta
#' @rdname setup_meta
#'
#' @export
setup_meta <- function(n, extent, grain, reefs = NULL, starting_values, parameters,
                       random = 0, use_log = TRUE,
                       verbose = TRUE, ...) {

  # print some information on console
  if (verbose) {

    message("> ...Creating ", n, " metaecosystems...")

    message("> ...Creating seafloor with extent(", extent[1], ", ", extent[2], ")...")

    if (!is.null(reefs)) {

      if (inherits(x = reefs, what = "list")) {

        if (length(reefs) != n) {

          stop("'reefs' must be matrix or list with reef coordinates.", call. = FALSE)

        }

      } else if (inherits(x = reefs, what = "matrix")) {

        reefs <- rep(x = list(reefs), each = n)

      } else {

        stop("'reefs' must be matrix or list with reef coordinates.", call. = FALSE)

      }

      no_reefs <- paste(c(vapply(reefs, FUN = nrow, FUN.VALUE = numeric(1))), collapse = ", ")

      message("> ...Creating ", no_reefs, " artifical reef cells...")


    } else {

      message("> ...No artifical reefs present...")

      reefs <- rep(x = list(NULL), each = n)

    }

    message("> ...Creating ", starting_values$pop_n, " individuals...")
  }

  # init list for objects
  seafloor_list <- vector(mode = "list", length = n)

  fishpop_list <- vector(mode = "list", length = n)

  # loop through all metaecosystems
  for (i in 1:n) {

    # create seafloor
    seafloor <- arrR::setup_seafloor(extent = extent, grain = grain, reefs = reefs[[i]],
                                     starting_values = starting_values, random = random,
                                     verbose = FALSE, ...)

    # save in final list
    seafloor_list[[i]] <- seafloor

    # create fishpop
    fishpop <- arrR::setup_fishpop(seafloor = seafloor, starting_values = starting_values,
                                   parameters = parameters, use_log = use_log,
                                   verbose = FALSE)

    # get number of digits of pop_n to create unique id
    no_digits <- floor(log10(starting_values$pop_n)) + 1

    # create unique id; first number identifies metaecosyst
    fishpop$id <- (i * 10 ^ no_digits) + fishpop$id

    # create col to count movement acroos ecosystems
    fishpop$stationary <- 0

    # save in final list
    fishpop_list[[i]] <- fishpop

  }

  # create look-up table for stationary value
  fishpop_stationary <- create_rstationary(fishpop_values = fishpop_list,
                                           mean = parameters$pop_mean_stationary,
                                           sd = parameters$pop_var_stationary)

  # combine everything to one list
  result_list <- list(seafloor = seafloor_list, fishpop = fishpop_list,
                      fishpop_stationary = fishpop_stationary,
                      starting_values = starting_values, parameters = parameters,
                      n = n, extent = extent, grain = grain, reefs = reefs)

  # specifiy class of list
  class(result_list) <- "meta_syst"

  return(result_list)
}
