#' simulate_meta
#'
#' @description Core function to run model.
#'
#' @param metasyst Metaecosystem created with \code{\link{setup_meta}}.
#' @param parameters List with all model parameters.
#' @param reef_attraction If TRUE, individuals are attracted to AR.
#' @param max_i Integer with maximum number of simulation timesteps.
#' @param min_per_i Integer to specify minutes per i.
#' @param save_each Numeric how often data should be saved to return.
#' @param burn_in Numeric with timesteps used to burn in.
#' @param return_burnin If FALSE all timesteps < burn_in are not returned.
#' @param extract Character to specify if only seafloor or fishpop should be returned as data.frame
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Wrapper function to run model. Executes the following sub-processes (i) simulate_seagrass
#' (ii) distribute_detritus (iii) simulate_movement (iv) simulate_movement (v) simulate_respiration
#' (vi) simulate_growth (vii) simulate_mortality and (viii) simulate_diffusion.
#'
#' @return mdl_rn
#'
#' @examples
#' # Add example code
#'
#' @aliases simulate_meta
#' @rdname simulate_meta
#'
#' @export
simulate_meta <- function(metasyst,
                          parameters, nutr_input = NULL, reef_attraction,
                          max_i, min_per_i, save_each = 1, burn_in = 0, return_burnin = TRUE,
                          verbose = TRUE) {

  # check if metasyst is correct class
  if (!inherits(x = metasyst, what = "meta_syst")) {

    stop("Please provide meta_syst object created with setupt_meta().", call. = FALSE)

  }

  # # check parameters
  # param_warnings <- tryCatch(arrR::check_parameters(parameters = parameters, verbose = FALSE),
  #                            warning = function(wrn) wrn)
  #
  # # stop with error
  # if (length(param_warnings$message) > 0) {
  #
  #   stop(param_warnings$message, call. = FALSE)
  #
  # }

  # check if max_i can be divided by provided save_each without reminder
  if (max_i %% save_each != 0) {

    stop("'max_i' cannot be divided by 'save_each' without rest.",
         call. = FALSE)
  }

  # check if save_each is whole number
  if (save_each %% 1 != 0) {

    stop("'save_each' must be a whole number.", call. = FALSE)

  }

  # check if nutr_input has value for each iteration
  if (!is.null(nutr_input)) {

    check <- all(vapply(nutr_input, function(i) length(i) == max_i, FUN.VALUE = logical(1)))

    if (!check) {

      stop("'nutr_input' must have input amount for each iteration.", call. = FALSE)

    }
  }

  if (burn_in >= max_i | burn_in < 0) {

    warning("'burn_in' larger than or equal to 'max_i' or 'burn_in' < 0.", call. = FALSE)

  }

  # convert seafloor to matrix
  seafloor_values <- lapply(metasyst$seafloor, function(i)
    as.matrix(raster::as.data.frame(i, xy = TRUE)))

  # convert seafloor to matrix
  fishpop_values <- lapply(metasyst$fishpop, function(i)
    as.matrix(raster::as.data.frame(i, xy = TRUE)))

  # check if fishpop is NULL
  if (is.null(fishpop_values)) {

    fishpop_values <- rep(x = list(data.frame(id = numeric(), age = numeric(),
                                              x = numeric(), y = numeric(), heading = numeric(),
                                              length = numeric(), weight = numeric(),
                                              reserves = numeric(), reserves_max = numeric(),
                                              activity = numeric(), respiration = numeric(),
                                              died_consumption = numeric(), died_background = numeric())),
                          times = metasyst$n)

  }

  # create nutrient input if not present
  if (is.null(nutr_input)) {

    nutr_input <- rep(list(rep(x = 0, times = max_i)), times = metasyst$n)

  }

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = metasyst$n)

  seafloor_track <- lapply(seq_along(seafloor_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  fishpop_track <- vector(mode = "list", length = metasyst$n)

  fishpop_track <- lapply(seq_along(fishpop_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # get extent of environment
  extent <- raster::extent(metasyst$seafloor[[1]])

  # get dimensions of environment (nrow, ncol)
  dimensions <- dim(metasyst$seafloor[[1]])[1:2]

  # get cell id of reef cells
  cells_reef <- lapply(seafloor_values, function(i) which(i[, 16] == 1))

  # get coordinates of reef cells
  coords_reef <- lapply(seq_along(seafloor_values),
                        function(i) seafloor_values[[i]][cells_reef[[i]], 1:2])

  # get neighboring cells for each focal cell using torus
  cell_adj <- lapply(metasyst$seafloor,
                     function(i) arrR::get_neighbors(x = i, direction = 8, torus = TRUE))

  # save input data in tracking list
  for (i in 1:metasyst$n) {

   seafloor_track[[i]][[1]] <- rlang::duplicate(seafloor_values[[i]])

   fishpop_track[[i]][[1]] <- rlang::duplicate(fishpop_values[[i]])

  }

  # print some basic information about model run
  if (verbose) {

    message("> Metaecosystem with ", metasyst$n, " local ecosystems.")

    message("> Seafloors with ", extent, "; ",
            paste(vapply(coords_reef, nrow, FUN.VALUE = numeric(1)), collapse = ", "), " reef cells.")

    message("> Populations with ", metasyst$starting_values$pop_n, " individuals [reef_attraction: ", reef_attraction, "].")

    message("> Simulating ", max_i, " iterations [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations.")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("")

    message("> ...Starting simulation...")

  }

  # simulate until max_i is reached
  for (i in 1:max_i) {

    for (j in 1:metasyst$n) {

      # get temp_n
      pop_n_temp <- nrow(fishpop_values[[j]])

      # simulate nutrient input
      arrR::simulate_input(seafloor_values = seafloor_values[[j]],
                           nutr_input = nutr_input[[j]],
                           timestep = i)

      # simulate seagrass growth
      arrR::simulate_seagrass(seafloor_values = seafloor_values[[j]],
                              parameters = parameters,
                              cells_reef = cells_reef[[j]],
                              min_per_i = min_per_i)

      # redistribute detritus
      arrR::simulate_mineralization(seafloor_values = seafloor_values[[j]],
                                    parameters = parameters)

      if (i > burn_in & metasyst$starting_values$pop_n != 0) {

        # simulate fish movement
        arrR::simulate_movement(fishpop_values = fishpop_values[[j]],
                                pop_n = pop_n_temp, # !!!
                                seafloor_values = seafloor_values[[j]],
                                extent = extent,
                                dimensions = dimensions,
                                parameters = parameters,
                                reef_attraction = reef_attraction)

        # simulate fish respiration (26°C is mean water temperature in the Bahamas)
        arrR::simulate_respiration(fishpop_values = fishpop_values[[j]],
                                   parameters = parameters,
                                   water_temp = 26,
                                   min_per_i = min_per_i)

        # simulate fishpop growth and including change of seafloor pools
        arrR::simulate_fishpop_growth(fishpop_values = fishpop_values[[j]],
                                      fishpop_track = fishpop_track[[j]][[1]],
                                      pop_n = pop_n_temp,
                                      seafloor = metasyst$seafloor[[j]]$reef,
                                      seafloor_values = seafloor_values[[j]],
                                      parameters = parameters,
                                      min_per_i = min_per_i)

        # simulate mortality
        arrR::simulate_mortality(fishpop_values = fishpop_values[[j]],
                                 fishpop_track = fishpop_track[[j]][[1]],
                                 pop_n = pop_n_temp,
                                 seafloor = metasyst$seafloor[[j]]$reef,
                                 seafloor_values = seafloor_values[[j]],
                                 parameters = parameters,
                                 min_per_i = min_per_i)

      }

      # diffuse values between neighbors
      arrR::simulate_diffusion(seafloor_values = seafloor_values[[j]],
                               cell_adj = cell_adj[[j]],
                               parameters = parameters)

      # remove nutrients from cells
      arrR::simulate_output(seafloor_values = seafloor_values[[j]],
                            parameters = parameters)

      # update tracking list
      if (i %% save_each == 0) {

        seafloor_track[[j]][[i / save_each + 1]] <- rlang::duplicate(seafloor_values[[j]])

        fishpop_track[[j]][[i / save_each + 1]] <- rlang::duplicate(fishpop_values[[j]])

      }

      # print progress
      if (verbose) {

        message("\r> ...Progress: ", floor(i / max_i * 100), "% of total iterations... \t\t\t",
                appendLF = FALSE)

      }
    }
  }

  # new line after last progress message
  if (verbose) {

    message("")

    message("> ...Saving results...")

  }

  # combine seafloor/fishpop to one dataframe
  for (i in 1:metasyst$n) {

    seafloor_track[[i]] <- data.frame(do.call(what = "rbind", args = seafloor_track[[i]]))

    # MH: Add timestep here?
    fishpop_track[[i]] <- data.frame(do.call(what = "rbind", args = fishpop_track[[i]]))

    # add timestep to  seafloor/fishpop counter
    seafloor_track[[i]]$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                        each = raster::ncell(metasyst$seafloor[[i]]))

    # fishpop is present
    if (metasyst$starting_values$pop_n > 0) {

      # MH How to get pop_n for each timestep? Aggregate by unique id ?
      fishpop_track[[i]]$timestep <- rep(x = seq(from = 0, to = max_i, by = save_each),
                                         each = metasyst$starting_values$pop_n)

      # no fish are present
    } else {

      fishpop_track[[i]]$timestep <- numeric(0)

    }

    # add burn_in col
    seafloor_track[[i]]$burn_in <- ifelse(test = seafloor_track[[i]]$timestep < burn_in,
                                          yes = "yes", no = "no")

    fishpop_track[[i]]$burn_in <- ifelse(test = fishpop_track[[i]]$timestep < burn_in,
                                         yes = "yes", no = "no")

    # remove all burn_in values
    if (!return_burnin) {

      seafloor_track[[i]] <- seafloor_track[[i]][seafloor_track[[i]]$burn_in == "no", ]

      fishpop_track[[i]] <- fishpop_track[[i]][fishpop_track[[i]]$burn_in == "no", ]

    }
  }

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track,
                 starting_values = metasyst$starting_values, parameters = parameters,
                 n = metasyst$n, reef_attraction = reef_attraction,
                 max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
                 save_each = save_each, extent = extent, grain = raster::res(metasyst$seafloor[[1]]),
                 coords_reef = coords_reef)

  # set class of result
  class(result) <- "meta_rn"

  # new line after last progress message
  if (verbose) {

    message("")

    message("> All done.")

  }

  return(result)
}
