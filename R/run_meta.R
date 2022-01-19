#' run_meta
#'
#' @description
#' Run metaecosystems simulation.
#'
#' @param metasyst \code{meta_syst} object created with \code{setup_meta}.
#' @param parameters List with all model parameters.
#' @param nutrients_input \code{nutr_input} with nutrient inputs
#' @param movement String specifying movement algorithm. Either 'rand', 'attr' or 'behav'.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param min_per_i Integer to specify minutes per i.
#' @param burn_in Numeric with time steps used to burn in.
#' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
#' @param save_each Numeric how often data should be saved to return.
#' @param return_burnin If FALSE all time steps < burn_in are not returned.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' This is the core function of the \pkg{meta.arrR} model that allows to easily run the
#' model. Besides running all sub-processes, the function also includes some basic
#' checks to make sure the model does not crash. However, this does not ensure that
#' e.g. all parameter values "make sense". The function returns a \code{meta_rn} object
#' which stores besides the model run results a lot of information about the model run
#' specification and many function that can handle the objects exist (e.g. plotting).
#'
#' The functions is a 'wrapper' around the following sub-processes: i) movement across
#' local metaecosystems (outter loop) (ii) nutrient input, (iii) seagrass growth,
#' (iv) detritus mineralization, (v) movement of individuals, (vi) respiration of
#' individuals, (vii) growth of individuals, (viii) mortality of individuals,
#' (ix) diffusion of nutrients/detritus, and (x) nutrient output (all inner loop).
#'
#' The \code{movement} argument allows to either specify random movement of individuals,
#' attracted movement towards the artificial reef of individuals or a movement behavior based
#' on their biosenergetics.
#'
#' If \code{nutrients_input} is \code{NULL}, no nutrient input is simulated. To also simulate no
#' nutrient output, set the \code{nutrients_loss} parameter to zero.
#'
#' If \code{save_each > 1}, not all iterations are saved in the final \code{meta_rn} object,
#' but only each timestep specified by the object. However, \code{max_i} must be dividable by
#' \code{save_each} without rest. Similar, \code{seagrass_each} allows to simulate all
#' seagrass sub-processes only each specified timestep.
#'
#' If \code{burn_in > 0}, all sub-processes related to fish individuals are not simulated
#' before this timestep is reached.
#'
#' @return meta_rn
#'
#' @examples
#' \dontrun{
#' result_rand <- run_meta(metasyst = metasyst, nutrients_input = nutrients_input,
#' parameters = parameters, movement = "rand", max_i = max_i, seagrass_each = seagrass_each,
#' min_per_i = min_per_i, save_each = save_each, verbose = TRUE)
#' }
#'
#' @aliases run_meta
#' @rdname run_meta
#'
#' @export
run_meta <- function(metasyst, parameters, nutrients_input = NULL, movement = "rand",
                     max_i, min_per_i, burn_in = 0, seagrass_each = 1, save_each = 1,
                     return_burnin = TRUE, verbose = TRUE) {

  # get time at beginning for final print
  if (verbose) {

    t_start <- Sys.time()

  }

  # check input and warnings #

  # MH: Check parameters?

  # check if metasyst is correct class
  if (!inherits(x = metasyst, what = "meta_syst")) {

    stop("Please provide meta_syst object created with setupt_meta().", call. = FALSE)

  }

  # check if max_i can be divided by provided save_each without reminder
  if (max_i %% save_each != 0) {

    stop("'max_i' cannot be divided by 'save_each' without rest.",
         call. = FALSE)

  }

  # check if save_each is whole number
  if (save_each %% 1 != 0) {

    stop("'save_each' must be a whole number.", call. = FALSE)

  }

  # check if burn_in is smaller than max_i or zero
  if (burn_in >= max_i || burn_in < 0) {

    warning("'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0.", call. = FALSE)

  }

  # setup fishpop #

  # MH: Check if actually present!

  # convert seafloor to matrix
  fishpop <- lapply(metasyst$fishpop, as.matrix)

  # calculate maximum movement distance
  mean_temp <- ifelse(test = movement == "behav",
                      yes = parameters$move_return, no = parameters$move_mean)

  var_temp <- ifelse(test = movement == "behav",
                     yes = 1.0, no = parameters$move_var)

  # get max_dist if fishpop is present
  if (any(metasyst$starting_values$pop_n != 0)) {

    # sample from lognorm to get maximum distance
    max_dist <- vapply(X = 1:1000000, FUN = function(i) {
      arrR::rcpp_rlognorm(mean = mean_temp, sd = sqrt(var_temp), min = 0.0, max = Inf)},
      FUN.VALUE = numeric(1))

    # set maximum distance to 95%
    max_dist <- stats::quantile(x = max_dist, probs = 0.95, names = FALSE)

  # no fish present at all
  } else {

    max_dist <- 0.0

    # check if no fishpop is present but movement not rand
    if (movement %in% c("attr", "behav")) {

      movement <- "rand"

      warning("No fishpop present. Setting 'movement' to 'rand'.", call. = FALSE)

    }
  }

  fishpop_track <- vector(mode = "list", length = metasyst$n)

  fishpop_track <- lapply(seq_along(fishpop_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # setup nutrient input #

  # check if nutrients_input has value for each iteration
  if (!is.null(nutrients_input)) {

    # check if nutrients_input is correct class
    if (!inherits(x = nutrients_input, what = "nutr_input")) {

      stop("The nutrients input must be of class 'nutr_input'.", call. = FALSE)

    }

    # check length of nutrient input
    if (!all(vapply(X = nutrients_input$values, function(i) nrow(i),
                    FUN.VALUE = integer(1)) == (max_i / seagrass_each))) {

      stop("There must be a nutrient input value for each timestep.", call. = FALSE)

    }

    # check meta n of input
    if (nutrients_input$n != metasyst$n) {

      stop("There must be nutrient input values for each local ecosystem.", call. = FALSE)

    }

    # set nutrient flag to save results later
    flag_nutr_input <- TRUE

  # create nutrient input if not present
  } else {

    nutrients_input <- sim_nutr_input(n = metasyst$n, max_i = max_i, seagrass_each = seagrass_each,
                                      input_mn = 0.0, freq_mn = 0.0, verbose = FALSE)

    # set nutrient flag to save results later
    flag_nutr_input <- FALSE

  }

  # get list of input values
  nutr_input_list <- lapply(X = nutrients_input$values, function(i) i[, 2])

  # setup seafloor #

  # convert seafloor to matrix
  seafloor <- lapply(metasyst$seafloor, function(i)
    as.matrix(terra::as.data.frame(i, xy = TRUE, na.rm = FALSE)))

  # get cell id of reef cells (needs matrix input)
  cells_reef <- lapply(seafloor, function(i) which(i[, 16] == 1))

  # get coordinates of reef cells (needs matrix input)
  coords_reef <- lapply(seq_along(seafloor),
                        function(i) matrix(data = c(cells_reef[[i]],
                                                    seafloor[[i]][cells_reef[[i]], 1:2]),
                                           ncol = 3))

  # get neighboring cells for each focal cell using torus
  cell_adj <- arrR::get_neighbors(x = metasyst$seafloor[[1]], direction = 8, cpp = TRUE)

  # get extent of environment
  extent <- metasyst$extent

  # get dimensions of environment (nrow, ncol)
  dimensions <- metasyst$dimensions

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = metasyst$n)

  seafloor_track <- lapply(seq_along(seafloor_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # check if no reef is present but movement not rand
  if (all(vapply(coords_reef, nrow, FUN.VALUE = numeric(1)) == 0) &&
      movement %in% c("attr", "behav")) {

    movement <- "rand"

    warning("No reef cells present in any metaecosystem. Thus 'movement' set to 'rand'.", call. = FALSE)

  }

  # print model run characteristics #

  # print some basic information about model run
  if (verbose) {

    message("> ...Starting at <", t_start, ">...")

    message("")

    message("> Metaecosystem with ", metasyst$n, " local ecosystems.")

    message("> Seafloors with ", dimensions[1], " rows x ", dimensions[2], " cols; ",
            paste(vapply(coords_reef, nrow, FUN.VALUE = numeric(1)), collapse = ", "), " reef cells.")

    message("> Populations with ", paste(metasyst$starting_values$pop_n, collapse = ", "), " individuals [movement: '", movement, "'].")

    message("> Simulating ", max_i, " iterations [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations.")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("")

  }

  # run model
  rcpp_sim_meta(seafloor = seafloor, fishpop = fishpop,
                seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                parameters = parameters, movement = movement, max_dist = max_dist,
                n = metasyst$n, pop_n = metasyst$starting_values$pop_n,
                fishpop_attributes = metasyst$fishpop_attributes,
                nutrients_input = nutr_input_list, coords_reef = coords_reef, cell_adj = cell_adj,
                extent = extent, dimensions = dimensions,
                max_i = max_i, min_per_i = min_per_i, save_each = save_each,
                seagrass_each = seagrass_each, burn_in = burn_in,
                verbose = verbose)

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor/fishpop to one dataframe
  for (i in 1:metasyst$n) {

    # combine seafloor #

    seafloor_track[[i]] <- data.frame(do.call(what = "rbind", args = seafloor_track[[i]]))

    # MH: remove col names...not great
    rownames(seafloor_track[[i]]) <- 1:nrow(seafloor_track[[i]])

    # add timestep to  seafloor/fishpop counter
    seafloor_track[[i]]$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                        each = terra::ncell(metasyst$seafloor[[i]]))

    # add burn_in col
    seafloor_track[[i]]$burn_in <- ifelse(test = seafloor_track[[i]]$timestep < burn_in,
                                          yes = "yes", no = "no")

    # combine fishpop #

    # repeat each timestep accoding to number of rows (individuals) each track
    timestep_temp <- rep(x = seq(from = 0, to = max_i, by = save_each),
                         times = vapply(X = fishpop_track[[i]], FUN = nrow, FUN.VALUE = numeric(1)))

    # combine list to data.frame
    fishpop_track[[i]] <- data.frame(do.call(what = "rbind", args = fishpop_track[[i]]))

    # MH: remove col names...not great
    rownames(fishpop_track[[i]]) <- 1:nrow(fishpop_track[[i]])

    # add timestep values
    fishpop_track[[i]]$timestep <- timestep_temp

    # add burn in col
    fishpop_track[[i]]$burn_in <- ifelse(test = fishpop_track[[i]]$timestep < burn_in,
                                           yes = "yes", no = "no")

    # individuals did not move across ecosystems
    if (parameters$move_residence == 0 && all(!is.na(fishpop_track[[i]][, -c(18, 19)]))) {

      fishpop_track[[i]]$residence <- fishpop_track[[i]]$timestep

    }

    # remove all burn_in values
    if (!return_burnin) {

      seafloor_track[[i]] <- seafloor_track[[i]][seafloor_track[[i]]$burn_in == "no", ]

      fishpop_track[[i]] <- fishpop_track[[i]][fishpop_track[[i]]$burn_in == "no", ]

    }
  }

  # no nutrient input was provided; return NA
  if (!flag_nutr_input) {

    nutrients_input <- NA

  }

  # name lists
  names(seafloor_track) <- paste0("Meta_", 1:metasyst$n)

  names(fishpop_track) <- paste0("Meta_", 1:metasyst$n)

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track, n = metasyst$n,
                 fishpop_attributes = metasyst$fishpop_attributes, movement = movement,
                 starting_values = metasyst$starting_values, parameters = parameters,
                 max_dist = max_dist, nutrients_input = nutrients_input, coords_reef = coords_reef,
                 extent = extent, grain = terra::res(metasyst$seafloor[[1]]),
                 dimensions = dimensions, max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
                 seagrass_each = seagrass_each, save_each = save_each)

  # set class of result
  class(result) <- "meta_rn"

  # new line after last progress message
  if (verbose) {

    # get time at end
    t_end <- Sys.time()

    message("")

    message("> ...All done at <", t_end, ">...")

  }

  return(result)
}
