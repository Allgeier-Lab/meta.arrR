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
#' nutr_input <- simulate_nutr_input(n = 3, max_i = 4380, freq_mn = 3, freq_var = 0.1,
#' input_max = 0.1, input_var = 0.25)
#'
#' @aliases print.meta_sys
#' @rdname print.meta_syst
#'
#' @export
print.meta_syst <- function(x, digits = 3, ...) {

  # get number of reef cells
  no_reefs <- paste(c(vapply(x$reefs, FUN = function(i) {

    ifelse(test = is.null(i), yes = 0, no = nrow(i))

  }, FUN.VALUE = numeric(1))),  collapse = ", ")

  # get number of individuals
  no_fish <- paste(c(vapply(x$fishpop, FUN = nrow, FUN.VALUE = numeric(1))),  collapse = ", ")

  # get extent
  extent <- raster::extent(x = x$seafloor[[1]])

  # print result
  cat(paste0("Metaecosystems    : ", x$n, "\n",
             "Local extent      : ", extent, "\n",
             "Local AR cells    : ", no_reefs, "\n",
             "Local individuals : ", no_fish, "\n"))

}
