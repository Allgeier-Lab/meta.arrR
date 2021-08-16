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
#' nutr_input <- sim_nutr_input_noise(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#' print(nutr_input)
#'
#' @aliases print.nutr_input
#' @rdname print.nutr_input
#'
#' @export
print.nutr_input <- function(x, digits = NULL, ...) {

  # get number of metaecosystems
  n <- length(x$values)

  # get maximum timesteps
  n_input <- length(x$values[[1]])

  # no digits argument present
  if (is.null(digits)) {

    # try to estimate number of digits
    digits <- abs(floor(log10(max(x$values[[1]]))) + 1) + 1

  }

  # get min input
  min_input <- vapply(x$values, FUN = function(i) round(min(i), digits = digits),
                      FUN.VALUE = numeric(1))

  # get mean input
  mean_input <- vapply(x$values, FUN = function(i) round(mean(i), digits = digits),
                       FUN.VALUE = numeric(1))

  # get max input
  max_input <- vapply(x$values, FUN = function(i) round(max(i), digits = digits),
                      FUN.VALUE = numeric(1))

  if (all(mean_input == 0)) {

    message("> Mean input is zero for all metaecosystems. Check digits argument.")

  }

  # print message
  cat(paste0(
    "Metaecosystems : ", n, "\n",
    "Total inputs   : ", n_input, " timesteps\n",
    "Min input      : ", paste(min_input, collapse = ", "), "\n",
    "Mean input     : ", paste(mean_input, collapse = ", "),  "\n",
    "Max input      : ", paste(max_input, collapse = ", ")))
}
