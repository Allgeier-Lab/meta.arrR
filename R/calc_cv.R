#' calc_cv
#'
#' @description
#' Calculate CV
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param what String specifying which column us used for \code{meta_rn} object.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Calculates coefficient of variation on alpha, beta and gamma scale, where \emph{i}
#' refers to values on local scale and \emph{m} refers to the sum of all values
#' in the metaecosystem.
#'
#' \deqn{\alpha_{i} = sd(x_{i}) / mean(x_{i})}
#'
#' \deqn{\alpha = sum(mean(x_{i}) / mean(x_{m}) * \alpha_{i})}
#'
#' \deqn{\gamma = sd(x_{m}) / mean(x_{m})}
#'
#' \deqn{\beta = \alpha / \gamma}
#'
#' \deqn{synchrony = var(x_{m}) / (sum(\alpha_{i}) ^ 2)}
#'
#' @references
#' Wang, S., Loreau, M., 2014. Ecosystem stability in space: α, β and γ
#' variability. Ecol Lett 17, 891–901. \url{https://doi.org/10.1111/ele.12292}
#'
#' Wang, S., Loreau, M., 2016. Biodiversity and ecosystem stability across scales
#' in metacommunities. Ecol Lett 19, 510–518. \url{https://doi.org/10.1111/ele.12582}
#'
#' @return list
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' calc_cv(nutr_input)
#'
#' \dontrun{
#' calc_cv(result)
#' }
#'
#' @aliases calc_cv
#' @rdname calc_cv
#'
#' @export
calc_cv <- function(x, what, verbose) UseMethod("calc_cv")

#' @name calc_cv
#' @export
calc_cv.nutr_input <- function(x, what = NULL, verbose = TRUE) {

  # pre-process values #

  # convert to matrix
  values_i <- do.call("cbind", x$values)

  # calculate sum of each timestep
  values_m <- apply(X = values_i, MARGIN = 1, FUN = sum)

  result_list <- calc_cv_internal(values_i = values_i, values_m = values_m)

  # return result list
  return(result_list)
}

#' @name calc_cv
#' @export
calc_cv.meta_rn <- function(x, what = "ag_biomass", verbose = TRUE) {

  # MH: What about fishpop?

  # pre-process data #

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

  # calculate sum of each timestep
  values_m <- apply(X = values_i, MARGIN = 1, FUN = sum)

  result_list <- calc_cv_internal(values_i = values_i, values_m = values_m)

  # return result list
  return(result_list)

}

calc_cv_internal <- function(values_i, values_m) {

  # alpha scale #

  # calculate sd and mean of local ecosystems i
  alpha_sd_i <- apply(X = values_i, MARGIN = 2, stats::sd)

  alpha_mean_i <- apply(X = values_i, MARGIN = 2, mean)

  # calculate cv of local ecosystems i
  alpha_cv_i <- unname(alpha_sd_i / alpha_mean_i)

  # calculate weighted mean CV on alpha scale
  alpha_cv <- sum(alpha_sd_i) / mean(values_m)

  # gamma scale #

  # calculate global gamma CV
  gamma_cv <- stats::sd(values_m) / mean(values_m)

  # beta scale #

  # calculate beta as ratio of alpha to gamma
  beta_cv <- alpha_cv / gamma_cv

  # check if NaN because division by zero
  beta_cv <- ifelse(test = is.finite(beta_cv),
                    yes = beta_cv, no = 0)

  # synchrony #
  synchrony <- stats::var(values_m) / sum(alpha_sd_i) ^ 2

  # check if NaN because division by zero
  synchrony <- ifelse(test = is.finite(synchrony),
                      yes = synchrony, no = 0)

  # final list #

  # combine to final result list
  result_list <- list(alpha_i = data.frame(Meta = 1:ncol(values_i), cv_i = alpha_cv_i),
                      alpha = alpha_cv, beta = beta_cv, gamma = gamma_cv,
                      synchrony = synchrony)

  return(result_list)
}
