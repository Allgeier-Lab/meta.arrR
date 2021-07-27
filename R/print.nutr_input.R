#' print.nutr_input
#'
#' @description
#' Printing method for nutr_input object.
#'
#' @param x nutr_input object.
#' @param digits Numeric of decimal places (round).
#' @param ... Arguments passed to cat.
#'
#' @details
#' Printing method for nutr_input model run created with \code{simulate_input_sine}.
#'
#' @examples
#' # Add example code
#'
#' @aliases print.nutr_input
#' @rdname print.nutr_input
#'
#' @export
print.nutr_input <- function(x, digits = NULL, ...) {

  # get number of metaecosystems
  n <- length(x)

  # get maximum timesteps
  n_input <- unique(vapply(x, FUN = length, FUN.VALUE = numeric(1)))

  # no digits argument present
  if (is.null(digits)) {

    # try to estimate number of digits
    digits <- abs(floor(log10(max(x[[1]]))) + 1) + 1

  }

  # get min input
  min_input <- vapply(x, FUN = function(x) round(min(x), digits = digits),
                      FUN.VALUE = numeric(1))

  # get mean input
  mean_input <- vapply(x, FUN = function(x) round(mean(x), digits = digits),
                       FUN.VALUE = numeric(1))

  # get max input
  max_input <- vapply(x, FUN = function(x) round(max(x), digits = digits),
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
