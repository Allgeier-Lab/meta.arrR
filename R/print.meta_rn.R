#' print.meta_rn
#'
#' @description
#' Printing method for meta_rn object.
#'
#' @param x \code{meta_rn} object simulated with \code{run_meta}.
#' @param digits Numeric of decimal places (passed on to \code{round}).
#' @param ... Not used.
#'
#' @details
#' Printing method for metaecosystem model run created with \code{run_meta}.
#' Returns the mean values for several seafloor and fish population values.
#'
#' @examples
#' \dontrun{
#' print(result_rand)
#' }
#'
#' @aliases print.meta_rn
#' @rdname print.meta_rn
#'
#' @export
print.meta_rn <- function(x, digits = 3, ...) {

  # calc biomass values
  biomass <- lapply(x$seafloor, function(seafloor_temp) {

    # filter max_i timestep
    seafloor_maxi <- seafloor_temp[seafloor_temp$timestep == x$max_i, ]

    # calculate total values
    c(bg = round(mean(seafloor_maxi$bg_biomass, na.rm = TRUE), digits = digits),
      ag = round(mean(seafloor_maxi$ag_biomass, na.rm = TRUE), digits = digits),
      nutr = round(mean(seafloor_maxi$nutrients_pool), digits = digits),
      detr = round(mean(seafloor_maxi$detritus_pool), digits = digits))

  })

  # get number of reefs
  no_reefs <- vapply(x$seafloor, function(seafloor_temp) {

    # filter max_i timestep
    seafloor_maxi <- seafloor_temp[seafloor_temp$timestep == x$max_i, ]

    # get number of rows in which reef = 1
    nrow(seafloor_maxi[seafloor_maxi$reef == 1, ])

  }, FUN.VALUE = numeric(1))

  # collapse to charachter string
  no_reefs <- paste(c(no_reefs), collapse = ", ")

  # calculate number of fish
  fish <- lapply(x$fishpop, function(fish_temp) {

    # no individual present, set to 0 and NA
    if (all(is.na(fish_temp[1, -c(18, 19)]))) {

      c(no = 0, length = NA, mort = NA)

    # calculate number of fish, mean length and mortality
    } else {

      # filter max_i timestep
      fishpop_maxi <- fish_temp[fish_temp$timestep == x$max_i, ]

      c(no = length(fishpop_maxi$id),
        length = round(mean(fishpop_maxi$length), digits = digits),
        mort = round(mean(fishpop_maxi$died_background + fishpop_maxi$died_consumption),
                     digits = digits))

    }
  })

  # get minimum timestep
  min_time <- unique(vapply(X = x$seafloor, function(i) min(i$timestep),
                            FUN.VALUE = numeric(1)))

  # calculate total time
  total_time <- round((x$max_i - min_time) * x$min_per_i / 60 / 24, digits = 2)

  # calculate save_each timesteps
  save_time <- round(x$save_each * x$min_per_i / 60 / 24, digits = 2)

  cat(paste0("Total time : ", paste0(c(min_time, x$max_i), collapse = "-"), " iterations (", total_time, " days) [Burn-in: ", x$burn_in, " iter.]\n",
             "Saved each : ", x$save_each, " iterations (", save_time, " days)\n",
             "Seafloor   : ", x$extent, "\n",
             "ARs        : ", no_reefs, " cells (movement: ", x$movement, ")\n\n"))

  cat("Local metaecosystems:\n")
  cat(paste0("ID\tBG\tAG\tNutr\tDetr\tFish\tLength\tMort\n"))

  lapply(1:x$n, function(i) {

    cat(paste0(i, "\t", biomass[[i]][[1]], "\t", biomass[[i]][[2]], "\t",
               biomass[[i]][[3]], "\t", biomass[[i]][[4]], "\t", fish[[i]][[1]],
               "\t", fish[[i]][[2]], "\t", fish[[i]][[3]], "\n"))

  })
}
