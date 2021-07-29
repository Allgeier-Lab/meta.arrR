#' setup_meta
#'
#' @description
#' Setup metaecosystems.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param dimensions Vector with number of rows and columns (spatial dimensions).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param reefs 2-Column matrix with coordinates of artificial reefs.
#' @param starting_values List with all starting value.
#' @param parameters List with all parameters.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#' @param use_log Logical if TRUE, random log distribution is used
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{raster}.
#'
#' @details
#' Function to setup the fish population. If no fish should be created, set
#' \code{starting_values$pop_n = 0}.
#'
#' @return meta_syst
#'
#' @examples
#' \dontrun{
#' metasyst <- setup_meta(n = n, dimensions = dimensions, grain = grain, reefs = reefs,
#' starting_values = starting_values, parameters = parameters)
#' }
#'
#' @aliases setup_meta
#' @rdname setup_meta
#'
#' @export
setup_meta <- function(n, dimensions, grain = c(1, 1), reefs = NULL, starting_values, parameters,
                       random = 0, use_log = TRUE,
                       verbose = TRUE, ...) {

  # print some information on console
  if (verbose) {

    message("> ...Creating ", n, " metaecosystems...")

    message("> ...Creating seafloor with ", dimensions[1], " rows x ", dimensions[2], " cols...")

    # reefs are present
    if (!is.null(reefs)) {

      # reefs are list, i.e., different across local metaecosystems
      if (inherits(x = reefs, what = "list")) {

        # must be same length as number of local metaecosystems
        if (length(reefs) != n) {

          stop("'reefs' must be matrix or list with reef coordinates.", call. = FALSE)

        }

      # reefs are matrix, i.e., the same for each local metaecosystem
      } else if (inherits(x = reefs, what = "matrix")) {

        reefs <- rep(x = list(reefs), each = n)

      # reef argument is wrong
      } else {

        stop("'reefs' must be matrix or list with reef coordinates.", call. = FALSE)

      }

      # get number of reefs for each local metaecosystem
      no_reefs <- paste(c(vapply(reefs, FUN = function(i) {

        ifelse(test = is.null(i), yes = 0, no = nrow(i))

      }, FUN.VALUE = numeric(1))),  collapse = ", ")


      message("> ...Creating ", no_reefs, " artifical reef cells...")

    # no reefs present
    } else {

      message("> ...No artifical reefs present...")

      reefs <- rep(x = list(NULL), each = n)

    }

    message("> ...Creating ", paste(starting_values$pop_n, collapse = ", "), " individuals...")

  }

  # make sure grain is vector xy dimension
  if (length(grain) == 1) {

    grain <- rep(grain, times = 2)

  } else if (length(grain) > 2) {

    stop("Please provide ony one or two 'grain' values.", call. = FALSE)

  }

  # init list for objects
  seafloor_list <- vector(mode = "list", length = n)

  fishpop_list <- vector(mode = "list", length = n)

  # create pop_n vector if length = 1
  if (length(starting_values$pop_n) == 1) {

    starting_values$pop_n <- rep(x = starting_values$pop_n, times = n)

  } else {

    # check if length makes sense
    if (length(starting_values$pop_n) != n) {

      stop("Length of provided 'pop_n' not allowed.", call. = FALSE)

    }

  }

  # loop through all metaecosystems
  for (i in 1:n) {

    # create temp starting values because pop_n can differ
    starting_values_temp <- starting_values

    # get current number of individuals
    starting_values_temp$pop_n <- starting_values$pop_n[[i]]

    # create seafloor
    seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reefs = reefs[[i]],
                                     starting_values = starting_values_temp, random = random,
                                     verbose = FALSE, ...)

    # save in final list
    seafloor_list[[i]] <- seafloor

    # create fishpop
    fishpop <- arrR::setup_fishpop(seafloor = seafloor, starting_values = starting_values_temp,
                                   parameters = parameters, use_log = use_log,
                                   verbose = FALSE)

    # get number of digits of pop_n to create unique id
    no_digits <- floor(log10(starting_values_temp$pop_n)) + 1

    # create unique id; first number identifies metaecosystem
    fishpop$id <- (i * 10 ^ no_digits) + fishpop$id

    # add stationary col
    if (nrow(fishpop) == 0) {

      fishpop <- cbind(fishpop, stationary = numeric(0))

    } else {

      fishpop$stationary <- 0

    }

    # save in final list
    fishpop_list[[i]] <- fishpop

  }

  # get extent
  extent <- raster::extent(x = seafloor_list[[1]])

  # create look-up table for stationary value
  fishpop_attributes <- create_attributes(fishpop = fishpop_list,
                                          parameters = parameters)

  # combine everything to one list
  result_list <- list(seafloor = seafloor_list, fishpop = fishpop_list,
                      n = n, fishpop_attributes = fishpop_attributes,
                      starting_values = starting_values, parameters = parameters,
                      reefs = reefs, extent = extent, grain = grain, dimensions = dimensions)

  # specify class of list
  class(result_list) <- "meta_syst"

  return(result_list)
}
