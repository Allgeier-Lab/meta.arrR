#' calc_probability
#'
#' @description
#' Calculate probability
#'
#' @param metasyst \code{meta_syst} object simulated with \code{setup_meta}.
#' @param lambda Distance decay parameter.
#' @param diag_value Numeric with value to be used on diagonal.
#' @param full Logical if full matrix should be returned.
#'
#' @details
#' Calculate probability matrix for local ecosystems based on distances between them
#' using a negative exponential function I = exp(-d*lambda).
#'
#' @references
#' https://en.wikipedia.org/wiki/Exponential_decay
#'
#' https://en.wikipedia.org/wiki/Distance_decay
#'
#' Nekola, J.C. and White, P.S., 1999. The distance decay of similarity in biogeography
#' and ecology. Journal of biogeography, 26(4), pp.867-878.
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
calc_probability <- function(metasyst, lambda = 1, diag_value = NA, full = TRUE) {

  if (!inherits(x = metasyst, what = "meta_syst")) stop("Please provide 'meta_syst' object.", call. = FALSE)

  # calculate distance matrix between local ecosystems
  local_dist <- as.matrix(stats::dist(metasyst$seafloor_xy[, 2:3], diag = TRUE, upper = TRUE,
                                      method = "euclidean"))

  # MH: Allow different functions here and e.g. all values = 1 for random movement

  # calculate probability
  local_prob <- exp(-local_dist * lambda)

  # replace diag with diag value
  diag(local_prob) <- diag_value

  # set lower tri to NA
  if (!full) local_prob[upper.tri(local_prob)] <- NA

  return(local_prob)

}
