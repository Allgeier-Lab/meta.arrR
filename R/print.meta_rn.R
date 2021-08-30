#' print.meta_rn
#'
#' @description
#' Printing method for meta_rn object.
#'
#' @param x \code{meta_rn} object simulated with \code{run_meta}.
#' @param timestep Numeric with timestep to print.
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
print.meta_rn <- function(x, digits = 2, timestep = x$max_i, ...) {

  # get timestep to be printed
  timestep_temp <- timestep

  # calc biomass values
  biomass <- lapply(x$seafloor, function(i) {

    # get selected timestep
    seafloor_temp <- subset(i, timestep == timestep_temp)

    # calculate total values
    c(bg = round(mean(seafloor_temp$bg_biomass, na.rm = TRUE), digits = digits),
      ag = round(mean(seafloor_temp$ag_biomass, na.rm = TRUE), digits = digits),
      nutr = round(mean(seafloor_temp$nutrients_pool), digits = digits),
      detr = round(mean(seafloor_temp$detritus_pool), digits = digits))

  })

  # get number of reefs
  no_reefs <- vapply(x$seafloor, function(i) {

    # only get first timestep because reefs are identical
    seafloor_temp <- subset(i, timestep == 0)

    # get number of rows in which reef = 1
    nrow(seafloor_temp[seafloor_temp$reef == 1, ])}, FUN.VALUE = numeric(1))

  # collapse to charachter string
  no_reefs <- paste(c(no_reefs), collapse = ", ")

  # calculate number of fish
  fish <- lapply(x$fishpop, function(i) {

    # get current fishpop
    fish_temp <- subset(i, timestep == timestep_temp)

    # no individual present, set to 0 and NA
    if (all(is.na(fish_temp[1, -c(18, 19)]))) {

      c(no = 0, length = NA, mort = NA)

    # calculate number of fish, mean length and mortality
    } else {

      c(no = length(fish_temp$id),
        length = round(mean(fish_temp$length), digits = digits),
        mort = round(mean(fish_temp$died_background + fish_temp$died_consumption),
                     digits = digits))
    }
  })

  # calculate total time
  total_time <- round(x$max_i * x$min_per_i / 60 / 24, digits = 2)

  # calculate save_each timesteps
  save_time <- round(x$save_each * x$min_per_i / 60 / 24, digits = 2)

  cat(paste0("Total time : ", timestep_temp, " iterations (", total_time, " days) [Burn-in: ", x$burn_in, " iter.]\n",
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
