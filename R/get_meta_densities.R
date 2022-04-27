#' get_meta_densities
#'
#' @description
#' Get metaecosystems densities
#'
#' @param result List with \code{meta_rn} objects simulated with \code{simulate_nutr_input_*}.
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
#' get_meta_densities(result = result_rand)
#' }
#'
#' @aliases get_meta_densities
#' @rdname get_meta_densities
#'
#' @export
get_meta_densities <- function(result, normalize = FALSE, verbose = TRUE) {

  # check input
  if (!inherits(x = result, what = "meta_rn")) {

    stop("Please provide meta_rn object.", call. = FALSE)

  }

  # return warning if save_each != 1 because not all occurrences are counted
  if (result$save_each != 1 && verbose) {

    warning("Please be aware that 'true' density might be higher because 'save_each' != 1",
            call. = FALSE)

  }

  # create flag if burin is present
  flag_burnin <- ifelse(test = result$burn_in > 0, yes = TRUE, no = FALSE)

  # create empty raster
  density_tmp <- terra::rast(ext = terra::ext(result$extent), resolution = result$grain,
                             crs = "", vals = 0.0, names = "density")

  # count densities within cells
  density_full <- do.call(rbind, lapply(seq_along(result$fishpop), function(i) {

    # get current fishpop
    fishpop_temp <- result$fishpop[[i]]

    # remove burn_in
    if (flag_burnin) {

      fishpop_temp <- fishpop_temp[fishpop_temp$burn_in == "no", ]

    }

    fishpop_temp <- as.matrix(fishpop_temp[, c("x", "y")], ncol = 2)

    # count fish within each cell
    density_ras <- terra::rasterize(x = terra::vect(fishpop_temp, crs = ""),
                                    y = density_tmp, fun = "length", background = 0)

    # # convert to data frame
    density_df <- terra::as.data.frame(density_ras, xy = TRUE, na.rm = FALSE)

    # add id
    density_df$id <- i

    return(density_df)

  }))

  # normalize by i
  if (normalize) {

    density_full$density <- density_full$density / result$max_i

  }

  names(density_full) <- c("x", "y", "density", "id")

  return(density_full)
}
