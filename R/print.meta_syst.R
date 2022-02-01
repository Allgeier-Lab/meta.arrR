#' print.meta_syst
#'
#' @description
#' Printing method for meta_syst object.
#'
#' @param x \code{meta_syst} object created with \code{setup_meta}.
#' @param digits Numeric of decimal places (passed on to \code{round}).
#' @param ... Not used.
#'
#' @details
#' Printing method for metaecosystem created with \code{setup_meta}.
#'
#' @examples
#' metasyst <- setup_meta(n = 3, max_i = 4380, dimensions = c(100, 100), grain = c(1, 1),
#' starting_values = meta.arrR_starting_values, parameters = meta.arrR_parameters)
#' print(metasyst)
#'
#' @aliases print.meta_sys
#' @rdname print.meta_syst
#'
#' @export
print.meta_syst <- function(x, digits = 3, ...) {

  # get number of reef cells
  no_reefs <- paste(c(vapply(x$reef, FUN = function(i) ifelse(test = is.null(i), yes = 0, no = nrow(i)),
                             FUN.VALUE = numeric(1))),  collapse = ", ")

  # get number of individuals
  no_fish <- paste(c(vapply(x$fishpop, FUN = function(i) ifelse(test = all(is.na(i)), yes = 0, no = nrow(i)),
                            FUN.VALUE = numeric(1))),  collapse = ", ")

  # get extent
  extent <- paste(as.vector(terra::ext(x = x$seafloor[[1]])), collapse = ", ")

  # print result
  cat(paste0("Metaecosystems    : ", x$n, "\n",
             "Environment       : ", extent, " (xmin, xmax, ymin, ymax)\n",
             "ARs               : ", no_reefs, "\n",
             "Individuals       : ", no_fish, "\n"))

}
