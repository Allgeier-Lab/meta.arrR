#' simulate_meta
#'
#' @description
#' Simulate metaecosystems.
#'
#' @param metasyst \code{meta_syst} object created with \code{setup_meta}.
#' @param parameters List with all model parameters.
#' @param nutr_input List with nutrient input vectors.
#' @param movement String specifying movement algorithm. Either 'rand', 'attr' or 'behav'.
#' @param max_i Integer with maximum number of simulation time steps.
#' @param min_per_i Integer to specify minutes per i.
#' @param seagrass_each Integer how often (each i * x) seagrass dynamics will be simulated.
#' @param save_each Numeric how often data should be saved to return.
#' @param burn_in Numeric with time steps used to burn in.
#' @param return_burnin If FALSE all time steps < burn_in are not returned.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' This is the core function of the \code{meta.arrR} model that allows to easily run the
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
#' If \code{nutr_input} is \code{NULL}, no nutrient input is simulated. To also simulate no
#' nutrient output, set the \code{nutrients_output} parameter to zero.
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
#' result_attr <- simulate_meta(metasyst = metasyst, nutr_input = nutr_input,
#' parameters = parameters, movement = "attr", max_i = max_i, seagrass_each = seagrass_each,
#' min_per_i = min_per_i, save_each = save_each, verbose = TRUE)
#' }
#'
#' @aliases simulate_meta
#' @rdname simulate_meta
#'
#' @export
simulate_meta <- function(metasyst, parameters, nutr_input = NULL, movement = "rand",
                          max_i, min_per_i, seagrass_each = 1, save_each = 1,
                          burn_in = 0, return_burnin = TRUE,
                          verbose = TRUE) {

  # check input and warnings #

  # check if metasyst is correct class
  if (!inherits(x = metasyst, what = "meta_syst")) {

    stop("Please provide meta_syst object created with setupt_meta().", call. = FALSE)

  }

  # MH: Check parameters?

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

  # get time at beginning for final print
  if (verbose) {

    t_start <- Sys.time()

  }

  # setup fishpop #

  # convert seafloor to matrix
  fishpop <- lapply(metasyst$fishpop, function(i) as.matrix(i, xy = TRUE))

  # calculate maximum movement distance
  mean_temp <- ifelse(test = movement == "behav",
                      yes = parameters$move_return, no = parameters$move_mean)

  var_temp <- ifelse(test = movement == "behav",
                     yes = 1.0, no = parameters$move_var)

  # MH: Set to 1000000
  max_dist <- vapply(X = 1:3, FUN = function(i) {
    arrR::rcpp_rlognorm(mean = mean_temp, sd = sqrt(var_temp), min = 0.0, max = Inf)},
    FUN.VALUE = numeric(1))

  max_dist <- stats::quantile(x = max_dist, probs = 0.95, names = FALSE)

  fishpop_track <- vector(mode = "list", length = metasyst$n)

  fishpop_track <- lapply(seq_along(fishpop_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # setup nutrient input #

  # check if nutr_input has value for each iteration
  if (!is.null(nutr_input)) {

    check <- all(vapply(nutr_input, function(i) length(i) == max_i, FUN.VALUE = logical(1)))

    if (!check) {

      stop("'nutr_input' must have input amount for each iteration.", call. = FALSE)

    }

    # set nutrient flag to save results later
    flag_nutr_input <- TRUE

  # create nutrient input if not present
  } else {

    nutr_input <- lapply(1:metasyst$n, function(i) rep(x = 0.0, times = max_i))

    # set nutrient flag to save results later
    flag_nutr_input <- FALSE

  }

  # setup seafloor #

  # convert seafloor to matrix
  seafloor <- lapply(metasyst$seafloor, function(i)
    as.matrix(raster::as.data.frame(i, xy = TRUE)))

  # get cell id of reef cells (needs matrix input)
  cells_reef <- lapply(seafloor, function(i) which(i[, 16] == 1))

  # get coordinates of reef cells (needs matrix input)
  coords_reef <- lapply(seq_along(seafloor),
                        function(i) cbind(id = cells_reef[[i]],
                                          seafloor[[i]][cells_reef[[i]], 1:2]))

  # get neighboring cells for each focal cell using torus
  cell_adj <- arrR::get_neighbors(x = metasyst$seafloor[[1]], direction = 8, cpp = TRUE)

  # get extent of environment
  extent <- as.vector(raster::extent(x = metasyst$seafloor[[1]]))

  # get dimensions of environment (nrow, ncol)
  dimensions <- dim(x = metasyst$seafloor[[1]])[1:2]

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = metasyst$n)

  seafloor_track <- lapply(seq_along(seafloor_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # print model run characteristics #

  # print some basic information about model run
  if (verbose) {

    message("> Metaecosystem with ", metasyst$n, " local ecosystems.")

    message("> Seafloors with ", raster::extent(extent), "; ",
            paste(vapply(coords_reef, nrow, FUN.VALUE = numeric(1)), collapse = ", "), " reef cells.")

    message("> Populations with ", paste(metasyst$starting_values$pop_n, collapse = ", "), " individuals [movement: '", movement, "'].")

    message("> Simulating ", max_i, " iterations [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations.")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("> ...Starting simulation...")

    message("")

  }

  # run model
  rcpp_meta_processes(seafloor = seafloor, fishpop = fishpop,
                      seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                      parameters = parameters, movement = movement, max_dist = max_dist,
                      n = metasyst$n, pop_n = metasyst$starting_values$pop_n,
                      fishpop_attributes = metasyst$fishpop_attributes,
                      nutr_input = nutr_input, max_i = max_i, min_per_i = min_per_i,
                      coords_reef = coords_reef, cell_adj = cell_adj,
                      extent = extent, dimensions = dimensions,
                      save_each = save_each, seagrass_each = seagrass_each, burn_in = burn_in,
                      verbose = verbose)

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor/fishpop to one dataframe
  for (i in 1:metasyst$n) {

    seafloor_track[[i]] <- data.frame(do.call(what = "rbind", args = seafloor_track[[i]]))

    # add timestep to  seafloor/fishpop counter
    seafloor_track[[i]]$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                        each = raster::ncell(metasyst$seafloor[[i]]))

    # add burn_in col
    seafloor_track[[i]]$burn_in <- ifelse(test = seafloor_track[[i]]$timestep < burn_in,
                                          yes = "yes", no = "no")

    # fishpop is present
    if (any(metasyst$starting_values$pop_n > 0)) {

      # repeat each timestep accoding to number of rows (individuals) each track
      timestep_temp <- rep(x = seq(from = 0, to = max_i, by = save_each),
                           times = vapply(X = fishpop_track[[i]], FUN = nrow, FUN.VALUE = numeric(1)))

      # combine list to data.frame
      fishpop_track[[i]] <- data.frame(do.call(what = "rbind", args = fishpop_track[[i]]))

      # add timestep values
      fishpop_track[[i]]$timestep <- timestep_temp

      # add burn in col
      fishpop_track[[i]]$burn_in <- ifelse(test = fishpop_track[[i]]$timestep < burn_in,
                                           yes = "yes", no = "no")

      # individuals did not move across ecosystems
      if (parameters$move_stationary == 0) {

        fishpop_track[[i]]$stationary <- fishpop_track[[i]]$timestep

      }

    # no fish population present
    } else {

      fishpop_track[[i]]$timestep <- numeric(0)

      fishpop_track[[i]]$burn_in <- character(0)

    }

    # remove all burn_in values
    if (!return_burnin) {

      seafloor_track[[i]] <- seafloor_track[[i]][seafloor_track[[i]]$burn_in == "no", ]

      fishpop_track[[i]] <- fishpop_track[[i]][fishpop_track[[i]]$burn_in == "no", ]

    }
  }

  if (!flag_nutr_input) {

    nutr_input <- NA

  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track, movement = movement,
                 n = metasyst$n, fishpop_attributes = metasyst$fishpop_attributes,
                 starting_values = metasyst$starting_values, parameters = parameters,
                 nutr_input = nutr_input, max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
                 save_each = save_each, seagrass_each = seagrass_each,
                 extent = raster::extent(extent), grain = raster::res(metasyst$seafloor[[1]]),
                 coords_reef = coords_reef)

  # set class of result
  class(result) <- "meta_rn"

  # new line after last progress message
  if (verbose) {

    # get time at end
    t_diff <- round(Sys.time() - t_start, digits = 1)

    message("")

    message("> All done (Runtime: ", t_diff," ", units(t_diff), ").")

  }

  return(result)
}
