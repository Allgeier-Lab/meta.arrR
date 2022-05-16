#' setup_meta
#'
#' @description
#' Setup metaecosystems.
#'
#' @param n Integer with number of metaecosystems to setup.
#' @param reef 2-Column matrix with coordinates of artificial reefs.
#' @param seafloor_xy 3-Column matrix with ID and x,y coordinates of local ecosystems.
#' @param dimensions Vector with number of rows and columns (spatial dimensions).
#' @param grain Vector with size of cells in x- and y-direction (spatial grain).
#' @param starting_values List with all starting value.
#' @param parameters List with all parameters.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param random Numeric to randomize input values by 0 = 0 percent to 1 = 100 percent.
#' @param use_log Logical if TRUE, random log distribution is used
#' @param verbose If TRUE, progress reports are printed.
#' @param ... Additional arguments passed on to \code{rast}.
#'
#' @details
#' Function to create metaecosystem including environmental raster grids and fish
#' populations.
#'
#' If \code{seafloor_xy = NULL}, coordinates are simulated random (range: 0-1).
#'
#' @return meta_syst
#'
#' @examples
#' metasyst <- setup_meta(n = 3, max_i = 4380, dimensions = c(50, 50), grain = 1,
#' starting_values = default_starting, parameters = default_parameters)
#'
#' @aliases setup_meta
#' @rdname setup_meta
#'
#' @export
setup_meta <- function(n, reef = NULL, seafloor_xy = NULL, dimensions, grain = 1,
                       starting_values, parameters, max_i, random = 0.0,
                       use_log = TRUE, verbose = TRUE, ...) {

  # print some information on console
  if (verbose) {

    message("> ...Creating ", n, " metaecosystems...")

    message("> ...Creating seafloor with ", dimensions[1] / grain, " rows x ", dimensions[2] / grain, " cols...")

  }

  # reefs are present
  if (!is.null(reef)) {

    # reefs are list, i.e., different across local metaecosystems
    if (inherits(x = reef, what = "list")) {

      # must be same length as number of local metaecosystems
      if (length(reef) != n) {

        stop("'reef' must be matrix or list with reef coordinates.", call. = FALSE)

      }

    # reefs are matrix, i.e., the same for each local metaecosystem
    } else if (inherits(x = reef, what = "matrix")) {

      reef <- rep(x = list(reef), each = n)

    # reef argument is wrong
    } else {

      stop("'reef' must be matrix or list with reef coordinates.", call. = FALSE)

    }

    # get number of reefs for each local metaecosystem
    no_reefs <- paste(c(vapply(reef, FUN = function(i) ifelse(test = is.null(i), yes = 0, no = nrow(i)),
                               FUN.VALUE = numeric(1))),  collapse = ", ")

    if (verbose) {

      message("> ...Creating ", no_reefs, " artifical reef cell(s)...")

    }

  # no reefs present
  } else {

    if (verbose) {

      message("> ...No artifical reef(s) present...")

    }

    reef <- rep(x = list(NULL), each = n)

  }

  # check length of grain argument
  if (length(grain) != 1) {

    stop("Please provide one 'grain' value.", call. = FALSE)

  }

  if (any(dimensions <= 0) || length(dimensions) != 2 || any(dimensions %% 1 != 0)) {

    stop("'dimensions must be a vector with two integer values.", call. = FALSE)

  }

  if (random < 0 || random > 1) {

    stop("'random' must be 0 <= x <= 1", call. = FALSE)

  }

  # init list for objects
  seafloor_list <- vector(mode = "list", length = n)

  fishpop_list <- vector(mode = "list", length = n)

  # create pop_n vector if length = 1
  if (length(starting_values$pop_n) == 1) {

    starting_values$pop_n <- rep(x = starting_values$pop_n, times = n)

    if (verbose) {

      message("> ...Creating ", paste(starting_values$pop_n, collapse = ", "), " individuals...")

    }

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
    seafloor <- arrR::setup_seafloor(dimensions = dimensions, grain = grain, reef = reef[[i]],
                                     starting_values = starting_values_temp, random = random,
                                     verbose = FALSE)

    # save in final list
    seafloor_list[[i]] <- seafloor

    # create fishpop
    fishpop <- arrR::setup_fishpop(seafloor = seafloor, starting_values = starting_values_temp,
                                   parameters = parameters, use_log = use_log,
                                   verbose = FALSE)

    # add NA row if no individuals is present
    if (nrow(fishpop) == 0) {

      fishpop[1, ] <- NA

      # add moved col
      fishpop$moved <- NA

    } else {

      # get number of digits of pop_n to create unique id
      no_digits <- floor(log10(starting_values_temp$pop_n)) + 1

      # create unique id; first number identifies metaecosystem
      fishpop$id <- (i * 10 ^ no_digits) + fishpop$id

      # add initial residence column
      fishpop$moved <- 0

    }

    # save in final list
    fishpop_list[[i]] <- fishpop

  }

  # create random seafloor_xy
  if (is.null(seafloor_xy)) {

    seafloor_xy <- cbind(id = 1:n, x = stats::runif(n = n, min = -1, max = 1),
                         y = stats::runif(n = n, min = -1, max = 1))

  # run checks
  } else {

    # check if nrow = n
    if (nrow(seafloor_xy) != n) stop("Please provide xy coordinate for each local ecosystem.", call. = FALSE)

    # check if ncol = 3
    if (ncol(seafloor_xy) != 3) stop("seafloor_xy must be 3-column matrix.", call. = FALSE)

    # check if id = 1:n
    if (all(seafloor_xy[, 1] != 1:n)) stop("The first column of 'seafloor_xy' must contain ID", call. = FALSE)

    # check if coords are between -1 and 1
    if (any(seafloor_xy[, 2:3] < -1) || any(seafloor_xy[, 2:3] > 1)) stop("'seafloor_xy' must be -1 <= x <= 1",  call. = FALSE)

    # check column names
    if (!all(names(seafloor_xy) != c("id", "x", "y"))) {

      names(seafloor_xy) <- c("id", "x", "y")

      if (verbose) warning("Naming 'seafloor_xy' columns 'id', 'x', and 'y'.", call. = FALSE)

    }
  }

  # create look-up table for residence value
  fishpop_attr <- setup_attributes(fishpop = fishpop_list, parameters = parameters,
                                   max_i = max_i)

  # get extent
  extent <- arrR::get_seafloor_dim(seafloor_list[[1]])$extent

  # combine everything to one list
  result_list <- list(n = n, seafloor = seafloor_list, fishpop = fishpop_list,
                      seafloor_xy = seafloor_xy, fishpop_attr = fishpop_attr,
                      starting_values = starting_values, parameters = parameters,
                      reef = reef, dimensions = dimensions, extent = extent, grain = grain)

  # specify class of list
  class(result_list) <- "meta_syst"

  return(result_list)
}
