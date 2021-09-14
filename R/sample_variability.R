#' sample_variability
#'
#' @description
#' Sample variability
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param what String specifying which column us used for \code{meta_rn} object.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Samples coefficient of variation (on gamma scale) for increasing number of metaecosystems. For
#' more information see \code{calc_variability}.
#'
#' @return data.frame
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' sample_variability(nutr_input)
#'
#' \dontrun{
#' sample_variability(result)
#' }
#'
#' @aliases sample_variability
#' @rdname sample_variability
#'
#' @export
sample_variability <- function(x, what, verbose) UseMethod("sample_variability")

#' @name sample_variability
#' @export
sample_variability.nutr_input <- function(x, what = NULL, verbose = TRUE) {

  # pre-process data #

  # create empty result df
  result_df <- data.frame(n = numeric(x$n),
                          alpha = numeric(x$n), beta = numeric(x$n),
                          gamma = numeric(x$n), synchrony = numeric(x$n))

  # shuffle id of metaecosystems
  n_total <- sample(x = 1:x$n, size = x$n)

  # convert to matrix
  values_i <- do.call("cbind", x$values)

  # loop through 1...n meteecosystems
  for (i in 1:length(n_total)) {

    # get increasing number of metaecosystems
    values_temp <- matrix(values_i[, n_total[1:i]], nrow = x$max_i, ncol = i)

    # calculate sum of each timestep
    values_m <- apply(X = values_temp, MARGIN = 1, FUN = sum)

    # calculate CV
    cv_temp <- calc_variability_internal(values_i = values_temp, values_m = values_m)

    # save results in data.frame
    result_df[i, ] <-  cbind(i, cv_temp$alpha, cv_temp$beta,
                             cv_temp$gamma, cv_temp$synchrony)

  }

  # return result data.frame
  return(result_df)
}

#' @name sample_variability
#' @export
sample_variability.meta_rn <- function(x, what = "ag_biomass", verbose = TRUE) {

  # pre-process data #

  # create empty result df
  result_df <- data.frame(n = numeric(x$n),
                          alpha = numeric(x$n), beta = numeric(x$n),
                          gamma = numeric(x$n), synchrony = numeric(x$n))

  # shuffle id of metaecosystems
  n_total <- sample(x = 1:x$n, size = x$n)

  # summarize values of each timestep
  seafloor_sum <- lapply(X = x$seafloor, FUN = function(i) {

    # get all values until timestep and selected column
    seafloor_temp <- subset(x = i, select = c("timestep", what))

    # sum for each timestep
    seafloor_temp <- stats::aggregate(x = seafloor_temp[, what],
                                      by = list(timestep = seafloor_temp$timestep),
                                      FUN = "sum")

    return(seafloor_temp[, 2])

  })

  # combine to matrix with local values
  values_i <- do.call("cbind", seafloor_sum)

  # loop through 1...n meteecosystems
  for (i in 1:length(n_total)) {

    # get increasing number of metaecosystems
    values_temp <- matrix(values_i[, n_total[1:i]],
                          nrow = (x$max_i / x$save_each) + 1, ncol = i)

    # calculate sum of each timestep
    values_m <- apply(X = values_temp, MARGIN = 1, FUN = sum)

    # calculate CV
    cv_temp <- calc_variability_internal(values_i = values_temp, values_m = values_m)

    # save results in data.frame
    result_df[i, ] <- cbind(i, cv_temp$alpha, cv_temp$beta,
                            cv_temp$gamma, cv_temp$synchrony)

  }

  # return result data.frame
  return(result_df)
}
