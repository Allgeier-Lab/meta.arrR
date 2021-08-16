#' calc_cv
#'
#' @description
#' Simulate nutrient input.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
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
calc_cv <- function(landscape) UseMethod("calc_cv")

#' @name calc_cv
#' @export
calc_cv.nutr_input <- function(x) {

  # calc local CVs
  local_cv <- vapply(X = x$values, FUN = function(i) stats::sd(i) / mean(i) * 100,
                     FUN.VALUE = numeric(1))

  # combine values to global vector with all values
  global_values <- do.call(what = "c", args = x$values)

  # calc global cv
  global_cv <- stats::sd(global_values) / mean(global_values) * 100

  # combine to final result list
  result_list <- list(local = local_cv, global = global_cv)

  # return result list
  return(result_list)
}
