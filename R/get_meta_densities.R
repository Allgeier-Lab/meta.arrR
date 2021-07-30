#' get_meta_densities
#'
#' @description
#' Get metaecosystems densities
#'
#' @param result List with \code{meta_rn} objects simulated with \code{simulate_nutr_input}.
#' @param timestep Numeric with time step to select
#' @param normalize Logical if TRUE count is divided by time steps.
#' @param verbose Logical if warning messages should be printed.
#'
#' @details
#' Returns the densities (i.e., number of fish) within each cell.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_meta_densities(result = result_attr)
#' }
#'
#' @aliases get_meta_densities
#' @rdname get_meta_densities
#'
#' @export
get_meta_densities <- function(result, timestep = result$max_i, normalize = FALSE,
                               verbose = TRUE) {

  # check input
  if (!inherits(x = result, what = "meta_rn")) {

    stop("Please provide meta_rn object.", call. = FALSE)

  }

  # return warning if save_each != 1 because not all occurences are counted
  if (result$save_each != 1 && verbose) {

    warning("Please be aware that 'true' density might be higher because 'save_each' is not one.",
            call. = FALSE)

  }

  # create flag if burin is present
  flag_burnin <- ifelse(test = result$burn_in > 0, yes = TRUE, no = FALSE)

  # rename timestep for filtering
  timestep_slctd <- timestep

  # check if i can be divided by save_each without reminder
  if (timestep_slctd %% result$save_each != 0) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)

  }

  # create empty raster
  density_tmp <- raster::raster(ext = result$extent,
                                resolution = result$grain)

  # count densities within cells
  density_full <- do.call(rbind, lapply(seq_along(result$fishpop), function(i) {

    # get current fishpop
    fishpop_temp <- subset(result$fishpop[[i]],
                           timestep <= timestep_slctd)

    # remove burn_in
    if (flag_burnin) {

      fishpop_temp <- subset(fishpop_temp, burn_in == "no")

    }

    # count fish within each cell
    density_ras <- raster::rasterize(x = fishpop_temp[, c("x", "y")],
                                     y = density_tmp,
                                     fun = "count", background = 0)

    # # convert to data frame
    density_df <- raster::as.data.frame(density_ras, xy = TRUE)

    # add id
    density_df$id <- i

    return(density_df)

  }))

  # normalize by i
  if (normalize) {

    density_full$density <- density_full$density / timestep_slctd

  }

  names(density_full) <- c("x", "y", "density", "id")

  return(density_full)
}
