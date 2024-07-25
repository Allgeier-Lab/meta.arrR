#' get_meta_densities
#'
#' @description
#' Get metaecosystems densities
#'
#' @param result List with \code{meta_rn} objects simulated with \code{simulate_nutrient_*}.
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

  # count densities within cells
  density_full <- do.call(rbind, lapply(seq_along(result$fishpop), function(i) {

    # get current fishpop
    fishpop_temp <- result$fishpop[[i]]

    seafloor_temp <- result$seafloor[[i]]

    # remove burn_in
    if (flag_burnin) {

      fishpop_temp <- fishpop_temp[fishpop_temp$burn_in == "no", ]

    }

    # create empty density data.frame
    density_df <- cbind(seafloor_temp[seafloor_temp$timestep == 0, c("x", "y")],
                        density = 0)

    if (nrow(fishpop_temp > 0)) {

      # convert coords to matrix
      xy_mat <- as.matrix(fishpop_temp[, c("x", "y")], ncol = 2)

      # get cell id of fish population
      fish_cell <- vapply(X = 1:nrow(xy_mat), function(i) {

        arrR:::rcpp_cell_from_xy(x = xy_mat[[i, "x"]], y = xy_mat[[i, "y"]],
                          extent = result$extent, dimensions = result$dimensions,
                          rcpp = FALSE)

      }, FUN.VALUE = numeric(1))

      # count number of fish within cells
      density_table <- table(fish_cell)

      # replace density of cells with fish with count
      density_df[as.numeric(names(density_table)), "density"] <- density_table

      # normalize by max_i
      if (normalize) {

        density_df$density <- density_df$density / result$max_i

      }
    }

    density_df <- cbind(meta = i, density_df)

    return(density_df)

  }))

  # normalize by i
  if (normalize) {

    density_full$density <- density_full$density / result$max_i

  }

  # names(density_full) <- c("x", "y", "density", "id")

  return(density_full)
}
