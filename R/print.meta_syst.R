#' print.meta_syst
#'
#' @description Printing method for mdl_rn object
#'
#' @param x mdl_rn object.
#' @param digits Numeric of decimal places (round).
#' @param ... Arguments passed to cat.
#'
#' @details
#' Printing method for metaecosystem created with \code{\link{setup_meta}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases print.meta_sys
#' @rdname print.meta_syst
#'
#' @export
print.meta_syst <- function(x, digits = 3, ...) {

  # print result
  cat(paste0("Metaecosystems    : ", x$n, "\n",
             "Local extent      : (", extent[1], ", ", extent[2], ")\n",
             "Local AR cells    : ", nrow(input_sys$reefs), "\n",
             "Local individuals : ", x$starting_values$pop_n, "\n"))

}
