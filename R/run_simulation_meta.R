#' run_simulation_meta
#'
#' @description
#' Run metaecosystems simulation.
#'
#' @param metasyst \code{meta_syst} object created with \code{setup_meta}.
#' @param parameters List with all model parameters.
#' @param nutrients_input \code{nutr_input} object or numeric with nutrient inputs.
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
#' If \code{nutrients_input = 0.0}, no nutrient input is simulated. To also simulate no
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
#' result_rand <- run_simulation_meta(metasyst = metasyst, nutrients_input = nutrients_input,
#' parameters = parameters, movement = "rand", max_i = max_i, seagrass_each = seagrass_each,
#' min_per_i = min_per_i, save_each = save_each, verbose = TRUE)
#' }
#'
#' @aliases run_simulation_meta
#' @rdname run_simulation_meta
#'
#' @export
run_simulation_meta <- function(metasyst, parameters, nutrients_input = 0.0, movement = "rand",
                     max_i, min_per_i, burn_in = 0, seagrass_each = 1, save_each = 1,
                     return_burnin = TRUE, verbose = TRUE) {

  # get time at beginning for final print
  if (verbose) {

    t_start <- Sys.time()

  }

  # check input and warnings #

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

  # setup seafloor #

  # convert seafloor to matrix
  seafloor_values <- lapply(metasyst$seafloor, function(i)
    as.matrix(terra::as.data.frame(i, xy = TRUE, na.rm = FALSE)))

  # create lists to store results for each timestep
  seafloor_track <- vector(mode = "list", length = metasyst$n)

  seafloor_track <- lapply(seq_along(seafloor_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # setup nutrient input #

  if (!inherits(x = nutrients_input, what = "nutr_input")) {

    if (length(nutrients_input) != 1) {

      stop("'nutrients_input' must either be 'nutr_input' object or single value.",
           call. = FALSE)

    } else {

      nutrients_input <- sim_nutr_input(n = metasyst$n, max_i = max_i, input_mn = nutrients_input,
                                        freq_mn = 0.0, verbose = FALSE)

    }
  }

  # get list of input values
  nutrients_input_values <- lapply(X = nutrients_input$values, function(i) i[, 2])

  # setup fishpop #

  # convert seafloor to matrix
  fishpop_values <- lapply(metasyst$fishpop, as.matrix)

  # check if no fishpop is present but movement not rand
  if ((sum(metasyst$starting_values$pop_n) == 0) && (movement %in% c("attr", "behav"))) {

    movement <- "rand"

    warning("No fishpop present. Setting 'movement' to 'rand'.", call. = FALSE)

  }

  # check if no reef is present but movement not rand
  if ((sum(vapply(metasyst$reef, function(i) ifelse(test = is.null(i), yes = 0, no = nrow(i)),
                  FUN.VALUE = numeric(1))) == 0) && movement %in% c("attr", "behav")) {

    movement <- "rand"

    warning("No reef cells present in any metaecosystem. Setting 'movement = rand'.", call. = FALSE)

  }

  # set behavior column to 3 for rand/attr movement
  if ((sum(metasyst$starting_values$pop_n) > 0) && (movement %in% c("rand", "attr"))) {

    for (i in 1:length(fishpop_values)) {

      fishpop_values[[i]][, "behavior"] <- 3.0

    }
  }

  fishpop_track <- vector(mode = "list", length = metasyst$n)

  fishpop_track <- lapply(seq_along(fishpop_track), function(i)
    vector(mode = "list", length = (max_i / save_each) + 1))

  # setup various #

  # MH: only needed of fishpop present, but also cheap to calculate
  # calculate probability matrix of local ecosystems
  seafloor_probs <- calc_probability(metasyst = metasyst, lambda = parameters$move_lambda,
                                     diag_value = 0.0)

  # print model run characteristics #

  total_time <- round(max_i * min_per_i / 60 / 24, digits = 2)

  total_save_each <- round(save_each * min_per_i / 60 / 24, digits = 2)

  # print some basic information about model run
  if (verbose) {

    message("> ...Starting at <", t_start, ">...")

    message("")

    message("> Metaecosystem with ", metasyst$n, " local ecosystems.")

    message("> Seafloors with ", metasyst$dimensions[1], " rows x ", metasyst$dimensions[2], " cols; ",
            paste(vapply(metasyst$reef, function(i) ifelse(test = is.null(i), yes = 0, no = nrow(i)),
                         FUN.VALUE = numeric(1)), collapse = ", "), " reef cells.")

    message("> Populations with ", paste(metasyst$starting_values$pop_n, collapse = ", "), " individuals [movement: '", movement, "'].")

    message("> Simulating ", max_i, " iterations (", total_time, " days) [Burn-in: ", burn_in, " iter.].")

    message("> Saving each ", save_each, " iterations (", total_save_each, " days).")

    message("> One iteration equals ", min_per_i, " minutes.")

    message("")

  }

  # run model
  rcpp_simulate_meta(seafloor = seafloor_values, fishpop = fishpop_values, nutrients_input = nutrients_input_values,
                     fishpop_attr = metasyst$fishpop_attr, seafloor_probs = seafloor_probs,
                     seafloor_track = seafloor_track, fishpop_track = fishpop_track,
                     parameters = parameters, movement = movement,
                     extent = metasyst$extent, dimensions = metasyst$dimensions,
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

  # name lists
  names(seafloor_track) <- paste0("Meta_", 1:metasyst$n)

  names(fishpop_track) <- paste0("Meta_", 1:metasyst$n)

  # combine result to list
  result <- list(seafloor = seafloor_track, fishpop = fishpop_track, nutrients_input = nutrients_input,
                 n = metasyst$n, movement = movement, fishpop_attr = metasyst$fishpop_attr,
                 parameters = parameters, starting_values = metasyst$starting_values,
                 extent = metasyst$extent, grain = terra::res(metasyst$seafloor[[1]]), dimensions = metasyst$dimensions,
                 max_i = max_i, min_per_i = min_per_i, burn_in = burn_in,
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
