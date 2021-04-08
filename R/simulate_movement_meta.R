#' simulate_movement_meta
#'
#' @description Initiate fish population.
#'
#' @param fishpop_values List with fishpop matrices.
#' @param n Numeric with number of metaecosystems.
#' @param pop_n Numeric with initial number of fish individuals.
#' @param parameters List with all model parameters.
#' @param fishpop_stationary Matrix with stationary value for each individual.
#' @param extent Spatial extent of the seafloor raster.
#'
#' @details
#' ADD TEXT
#'
#' @return vector
#'
#' @examples
#' # Add example code
#'
#' @aliases simulate_movement_meta
#' @rdname simulate_movement_meta
#'
#' @export
simulate_movement_meta <- function(fishpop_values, n, pop_n,
                                   parameters, fishpop_stationary, extent) {

  fishpop_values <- rcpp_move_meta(fishpop_values = fishpop_values,
                                   n = n, pop_n = pop_n,
                                   fishpop_stationary = fishpop_stationary,
                                   extent = as.vector(extent, mode = "numeric"))

  return(fishpop_values)
}
