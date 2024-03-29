#' calc_variability
#'
#' @description
#' Calculate variability
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param biomass,production Logical to specifiy if biomass and/or production is summarize.d
#' @param lag Vector with logical. If TRUE, the difference to the previous timestep is returned.
#' The first element refers to biomass, the second element to production.
#' @param verbose Logical if TRUE progress reports are printed.
#' @param ... Arguments passed on to \code{\link{summarize_meta}}.
#'
#' @details
#' Calculates coefficient of variation on alpha, beta and gamma scale, where \emph{i}
#' refers to values on local scale and \emph{m} refers to the sum of all values
#' in the metaecosystem.
#'
#' \deqn{\alpha = sum(sd(x_{i})) / mean(x_{m})}
#'
#' \deqn{\gamma = sd(x_{m}) / mean(x_{m})}
#'
#' \deqn{\beta = \alpha / \gamma}
#'
#' \deqn{synchrony = var(x_{m}) / (sum(\alpha_{i}) ^ 2)}
#'
#' @references
#' Wang, S., Loreau, M., 2014. Ecosystem stability in space: α, β and γ
#' variability. Ecol Lett 17, 891–901. \doi{10.1111/ele.12292}
#'
#' Wang, S., Loreau, M., 2016. Biodiversity and ecosystem stability across scales
#' in metacommunities. Ecol Lett 19, 510–518. \doi{10.1111/ele.12582}
#'
#' @return list
#'
#' @examples
#' nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, input_mn = 1,
#' frequency = 3, noise = 0.5)
#' calc_variability(nutrients_input)
#'
#' \dontrun{
#' calc_variability(result)
#' }
#'
#' @aliases calc_variability
#' @rdname calc_variability
#'
#' @export
calc_variability <- function(x, biomass, production, lag, verbose, ...) UseMethod("calc_variability")

#' @name calc_variability
#' @export
calc_variability.nutr_input <- function(x, biomass = NULL, production = NULL, lag = NULL,
                                        verbose = TRUE, ...) {

  # warning for lag argument
  if ((!is.null(biomass) || !is.null(production) || !is.null(lag)) && verbose) {

    warning("'biomass', 'production' or 'lag' are used for 'meta_rn' objects only.",
            call. = FALSE)

  }

  # pre-process values #

  # convert to matrix
  values_i <- get_input_df(x = x, gamma = FALSE)[, -1, drop = FALSE]

  # calculate sum of each timestep
  values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

  # calc variability
  result <- cbind(part = "input", calc_variability_internal(values_i = values_i, values_m = values_m))

  # return result list
  return(result)
}

#' @name calc_variability
#' @export
calc_variability.meta_rn <- function(x, biomass = TRUE, production = TRUE, lag = c(FALSE, TRUE),
                                     verbose = TRUE, ...) {

  # get sum of total local ecosystems
  result_sum <- summarize_meta(result = x, biomass = biomass, production = production,
                               lag = lag, ...)

  # loop through bg, ag, ttl biomass/prod
  result <- lapply(result_sum, function(i) {

    # check if summarized is null
    if (is.null(i)) {

      return(NULL)

    # calc variability
    } else {

      # get names of summarized parts
      names_parts <- names(i[, -c(1:2)])

      result_temp <- lapply(names_parts, function(j) {

        # get only needed cols
        values_i <- i[, c("meta", "timestep", j)]

        # reshape to wide for internal cv fun
        values_i <- stats::reshape(values_i, idvar = "timestep", timevar = "meta",
                                   direction = "wide")[, -1, drop = FALSE]

        # calculate sum of each timestep
        values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

        cbind(part = j, calc_variability_internal(values_i = values_i, values_m = values_m))

      })

      # combine to one data.frame
      result_temp <- do.call(what = "rbind", args = result_temp)

      # make sure bg comes first
      result_temp[order(result_temp$part), ]

    }
  })

  # return result list
  return(result)

}

calc_variability_internal <- function(values_i, values_m) {

  # gamma scale #

  # calculate sd of meta-ecosystem scale
  gamma_sd <- stats::sd(values_m, na.rm = TRUE)

  # calculate mean of meta-ecosystem scale
  gamma_mn <- mean(values_m, na.rm = TRUE)

  # calculate global gamma CV
  gamma_cv <- gamma_sd / gamma_mn

  # alpha scale #

  # calculate mean of local ecosystem i
  alpha_mn_i <- apply(X = values_i, MARGIN = 2, mean, na.rm = TRUE)

  # calculate sd of local ecosystems i
  alpha_sd_i <- apply(X = values_i, MARGIN = 2, stats::sd, na.rm = TRUE)

  # # calculate cv of each i
  # alpha_cv_i <- alpha_sd_i / alpha_mn_i
  #
  # # calculate alpha scale CV
  # alpha_cv <- sum((alpha_mn_i / gamma_mn) * alpha_cv_i)

  # calculate alpha scale CV
  alpha_cv <- sum(alpha_sd_i) / gamma_mn

  # beta scale #

  # calculate beta as ratio of alpha to gamma
  beta_cv <- ifelse(test = alpha_cv == 0 & alpha_cv == 0,
                    yes = 1, no = alpha_cv / gamma_cv)

  # synchrony #
  synchrony <- ifelse(test = alpha_cv == 0 & alpha_cv == 0,
                       yes = 1, no = stats::var(values_m, na.rm = TRUE) / sum(alpha_sd_i) ^ 2)

  # final list #

  # combine to final result list
  result_df <- data.frame(measure = c("alpha", "beta", "gamma", "synchrony"),
                          value = c(alpha_cv, beta_cv, gamma_cv, synchrony),
                          sd = c(mean(alpha_sd_i), NA, gamma_sd, NA), mean = c(mean(alpha_mn_i), NA, gamma_mn, NA))

  return(result_df)
}
