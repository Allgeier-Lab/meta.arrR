#' print.meta_rn
#'
#' @description Printing method for meta_rn object
#'
#' @param x meta_rn object.
#' @param timestep Numeric with timestep to print.
#' @param digits Numeric of decimal places (round).
#' @param ... Arguments passed to cat.
#'
#' @details
#' Printing method for metaecosystem model run created with \code{\link{simulate_meta}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases print.meta_rn
#' @rdname print.meta_rn
#'
#' @export
print.meta_rn <- function(x, digits = 2, timestep = x$max_i, ...) {

  timestep_temp <- timestep

  # calc biomass values
  biomass <- lapply(x$seafloor, function(i) {

    seafloor_temp <- subset(i, timestep == timestep_temp)

    c(bg = round(sum(seafloor_temp$bg_biomass, na.rm = TRUE), digits = digits),
      ag = round(sum(seafloor_temp$ag_biomass, na.rm = TRUE), digits = digits),
      nutr = round(sum(seafloor_temp$nutrients_pool), digits = digits))

  })

  no_reefs <- vapply(x$seafloor, function(i) {

    seafloor_temp <- subset(i, timestep == 0)

    nrow(seafloor_temp[seafloor_temp$reef == 1, ])}, FUN.VALUE = numeric(1))

  no_reefs <- paste(c(no_reefs), collapse = ", ")

  # calculate number of fish
  fish <- lapply(x$fishpop, function(i) {

    fish_temp <- subset(i, timestep == timestep_temp)

    if (all(is.na(fish_temp[1, -c(18, 19)]))) {

      c(no = 0, length = NA, mort = NA)

    } else {

      c(no = length(fish_temp$id),
        length = round(mean(fish_temp$length), digits = digits),
        mort = round(mean(fish_temp$died_background + fish_temp$died_consumption),
                     digits = digits))
    }
  })


  total_time <- round(x$max_i * x$min_per_i / 60 / 24, digits = 2)

  save_time <- round(x$save_each * x$min_per_i / 60 / 24, digits = 2)

  cat(paste0("Total time : ", timestep_temp, " iterations (", total_time, " days) [Burn-in: ", x$burn_in, " iter.]\n",
             "Saved each : ", x$save_each, " iterations (", save_time, " days)\n",
             "Seafloor   : ", x$extent, "; ", no_reefs, " reef cells\n",
             "Fishpop    : ", x$starting_values$pop_n, " indiv (reef_attraction: ", x$reef_attraction, ")\n",
             "\n"))

  cat(paste0("ID\tBelowground\tAboveground\tNutrients\tFish\tLength\tMortality\n"))

  lapply(1:x$n, function(i) {

    cat(paste0(i, "\t", biomass[[i]][[1]], "\t", biomass[[i]][[2]], "\t",
               biomass[[i]][[3]], "\t\t", fish[[i]][[1]], "\t", fish[[i]][[2]], "\t",
               fish[[i]][[3]], "\n"))

  })
}
