#' calc_cv
#'
#' @description
#' Simulate nutrient input.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param timestep Timestep only used for \code{meta_rn} object.
#'
#' @details
#' Calculate CV of provided code{nutr_input} object.
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input_noise(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#' calc_cv(nutr_input)
#'
#' @aliases calc_cv
#' @rdname calc_cv
#'
#' @export
calc_cv <- function(x, timestep, verbose) UseMethod("calc_cv")

#' @name calc_cv
#' @export
calc_cv.nutr_input <- function(x, timestep, verbose = TRUE) {

  # timestep not used
  if (!is.null(timestep) && verbose) {

    warning("'timestep' is only used for mdl_rn object.", call. = FALSE)

  }

  # calc local CVs
  local_cv <- vapply(X = x$values, FUN = function(i) stats::sd(i) / mean(i) * 100,
                     FUN.VALUE = numeric(1))

  # combine values to global vector with all values
  global_values <- do.call(what = "c", args = x$values)

  # calc global cv
  global_cv <- stats::sd(global_values) / mean(global_values) * 100

  # # calc local relative
  # local_rel <- local_cv / global_cv * 100

  # combine to final result list
  result_list <- list(local = local_cv, global = global_cv)

  # return result list
  return(result_list)
}

#' #' @name calc_cv
#' #' @export
#' calc_cv.meta_rn <- function(x, timestep = x$max_i) {
#'
#'   # get selected timestep
#'   timestep_slctd <- timestep
#'
#'   # check if i can be divided by save_each without reminder
#'   if (timestep_slctd %% x$save_each != 0) {
#'
#'     stop("'timestep' was not saved during model run.",
#'          call. = FALSE)
#'
#'   }
#'
#'   # # get data.frame with all seafloor values of selected timestep
#'   # seafloor <- do.call(rbind, lapply(seq_along(x$seafloor), function(i) {
#'   #
#'   #   id <- paste0("Metaecosystem ", i)
#'   #
#'   #   cbind(subset(x$seafloor[[i]], timestep == timestep_slctd,
#'   #                select = c("ag_biomass", "bg_biomass", "nutrients_pool", "detritus_pool")), id)
#'   #
#'   # }))
#'
#'   result_list <- list(local = numeric(1), global = numeric(1))
#'
#'   # return result list
#'   return(result_list)
#' }
