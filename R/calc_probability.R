#' calc_probability
#'
#' @description
#' Calculate probability
#'
#' @param metasyst ADD TEXT
#' @param lambda Distance decay parameter.
#'
#' @details
#' ADD TEXT
#'
#' @return matrix
#'
#' @examples
#' \dontrun{
#' calc_probability(metasyst)
#' }
#'
#' @aliases calc_probability
#' @rdname calc_probability
#'
#' @export
calc_probability <- function(metasyst, lambda = 1) {

  if (!inherits(x = metasyst, what = "meta_syst")) stop("Please provide 'meta_syst' object.", call. = FALSE)

  # calculate distance matrix between local ecosystems
  local_dist <- as.matrix(dist(metasyst$seafloor_xy[, 2:3], diag = TRUE, upper = TRUE))

  local_prob <- exp(-local_dist * lambda)

  return(local_prob)

}
