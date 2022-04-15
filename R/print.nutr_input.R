#' print.nutr_input
#'
#' @description
#' Printing method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{simulate_nutr_input_*}.
#' @param digits Numeric of decimal places (passed on to \code{round}).
#' @param ... Not used.
#'
#' @details
#' Printing method for \code{nutr_input} created with \code{simulate_nutr_input_*}.
#'
#' @examples
#' nutrients_input <- simulate_nutr_input(n = 3, max_i = 4380, input_mn = 1, frequency = 3,
#' amplitude_sd = 0.5)
#' print(nutrients_input)
#'
#' @aliases print.nutr_input
#' @rdname print.nutr_input
#'
#' @export
print.nutr_input <- function(x, digits = NULL, ...) {

  # convert to matrix
  values_mat <- get_input_df(x = x, gamma = FALSE)[, -1, drop = FALSE]

  # no digits argument present
  if (is.null(digits)) {

    # try to estimate number of digits
    digits <- abs(floor(log10(max(values_mat))) + 1) + 1

  }

  # get min input
  min_input <- apply(X = values_mat, MARGIN = 2,
                     FUN = function(i) round(min(i), digits = digits))

  # get mean input
  mean_input <- apply(X = values_mat, MARGIN = 2,
                      FUN = function(i) round(mean(i), digits = digits))

  # get max input
  max_input <- apply(X = values_mat, MARGIN = 2,
                     FUN = function(i) round(max(i), digits = digits))

  if (all(mean_input == 0)) {

    message("> Mean input is zero for all metaecosystems. Check digits argument.")

  }

  # get minimum timestep
  min_time <- unique(vapply(X = x$values, function(i) min(i$timestep),
                            FUN.VALUE = numeric(1)))

  # print message
  cat(paste0(
    "Total time     : ", paste0(c(min_time, x$max_i), collapse = "-"), " iterations [Freq: ", x$freq_mn, "]\n",
    "Metaecosystems : ", x$n, "\n",
    "Amplitude      : ", paste(round(x$amplitude, digits = digits), collapse = ", "), "\n",
    "Phase          : ", paste(round(x$phase, digits = digits), collapse = ", "), "\n",
    "\n",
    "Min input      : ", paste(min_input, collapse = ", "), "\n",
    "Mean input     : ", paste(mean_input, collapse = ", "), "\n",
    "Max input      : ", paste(max_input, collapse = ", "), "\n"))
}
