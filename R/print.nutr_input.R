#' print.nutr_input
#'
#' @description
#' Printing method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param digits Numeric of decimal places (passed on to \code{round}).
#' @param ... Not used.
#'
#' @details
#' Printing method for \code{nutr_input} created with \code{sim_nutr_input_*}.
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' print(nutr_input)
#'
#' @aliases print.nutr_input
#' @rdname print.nutr_input
#'
#' @export
print.nutr_input <- function(x, digits = NULL, ...) {

  # convert to matrix
  values_mat <- do.call("cbind", x$values)

  # no digits argument present
  if (is.null(digits)) {

    # try to estimate number of digits
    digits <- abs(floor(log10(max(x$values[[1]]))) + 1) + 1

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

  # print message
  cat(paste0(
    "Metaecosystems : ", x$n, "\n",
    "Max timesteps  : ", x$max_i, " [Freq: ", x$freq_mn, "]\n",
    "Min input      : ", paste(min_input, collapse = ", "), "\n",
    "Mean input     : ", paste(mean_input, collapse = ", "), "\n",
    "Max input      : ", paste(max_input, collapse = ", "), "\n"))
}
