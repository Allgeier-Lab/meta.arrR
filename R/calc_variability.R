#' calc_variability
#'
#' @description
#' Calculate variability
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param what String specifying which measure to use to calculate variability.
#' @param lag Logical if TRUE, the difference to the previous timestep is returned.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Calculates coefficient of variation on alpha, beta and gamma scale, where \emph{i}
#' refers to values on local scale and \emph{m} refers to the sum of all values
#' in the metaecosystem.
#'
#' The 'what' argument can be either 'biomass', 'production' or 'turnover'.
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
#' calc_variability(nutr_input)
#'
#' \dontrun{
#' calc_variability(result)
#' }
#'
#' @aliases calc_variability
#' @rdname calc_variability
#'
#' @export
calc_variability <- function(x, what, lag, verbose) UseMethod("calc_variability")

#' @name calc_variability
#' @export
calc_variability.nutr_input <- function(x, what = NULL, lag = NULL, verbose = TRUE) {

  # warning for lag argument
  if (!is.null(lag) && verbose) {

    warning("'lag' is used for 'meta_rn' objects only.", call. = FALSE)

  }

  # pre-process values #

  # convert to matrix
  values_i <- do.call("cbind", x$values)

  # calculate sum of each timestep
  values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

  # calc variability
  result <- cbind(part = "input",
                  calc_variability_internal(values_i = values_i, values_m = values_m))

  # return result list
  return(result)
}

#' @name calc_variability
#' @export
calc_variability.meta_rn <- function(x, what = "biomass", lag = TRUE, verbose = TRUE) {


  # calc CV for biomass
  if (what == "biomass") {

    # check lag argument
    if (lag && verbose) {

      warning("'lag' is not used for biomass calculations due to negative numbers.",
              call. = FALSE)

    }

    # calculate variability for what parts
    result <- lapply(c("bg_biomass", "ag_biomass"), function(i) {

      # summarize values of each timestep
      seafloor_sum <- lapply(X = x$seafloor, FUN = function(j) {

        # get all values until timestep and selected column
        seafloor_temp <- subset(x = j, select = c("timestep", i))

        # sum for each timestep
        seafloor_temp <- stats::aggregate(x = seafloor_temp[, i],
                                          by = list(timestep = seafloor_temp$timestep),
                                          FUN = "sum")

        # timestep column is not needed
        seafloor_temp[, -1]

      })

      # combine to matrix with local values
      values_i <- do.call("cbind", seafloor_sum)

      # calculate sum of each timestep
      values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

      cbind(part = i, calc_variability_internal(values_i = values_i,
                                                values_m = values_m))
    })

  # calc variability for production
  } else if (what == "production") {

    # calc turnover
    production <- get_meta_production(result = x, lag = lag, turnover = FALSE)

    # split into list using parts
    production <- split(production, production$part)

    # loop through list
    result <- lapply(production, function(i){

      # get only needed columns
      values_i <- i[, c("meta", "timestep", "value")]

      # reshape to wide for internal cv fun
      values_i <- stats::reshape(values_i, idvar = "timestep", timevar = "meta",
                                 direction = "wide")[, -1, drop = FALSE]

      # calculate sum of each timestep
      values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

      # combine to final data.frame
      cbind(part = unique(i$part), calc_variability_internal(values_i = values_i,
                                                             values_m = values_m))

    })

  # calculate cv for turnover
  } else if (what == "turnover") {

    # calc turnover
    turnover <- get_meta_production(result = x, lag = lag, turnover = TRUE)

    # replace Inf values (no production) with NA
    turnover[is.infinite(turnover$value), "value"] <- NA

    # split into list using part
    turnover <- split(turnover, turnover$part)

    # loop through lists
    result <- lapply(turnover, function(i) {

      # get only needed columns
      values_i <- i[, c("meta", "timestep", "value")]

      # reshape to wide format used for internal cv fun
      values_i <- stats::reshape(values_i, idvar = "timestep", timevar = "meta",
                                 direction = "wide")[, -1, drop = FALSE]

      # calculate sum of each timestep
      values_m <- apply(X = values_i, MARGIN = 1, FUN = sum, na.rm = FALSE)

      # create final data.frame
      cbind(part = unique(i$part), calc_variability_internal(values_i = values_i,
                                                             values_m = values_m))

    })

  # return error message
  } else {

    stop("Please select either 'biomass', 'production', or 'turnover' as 'what' argument.",
         call. = FALSE)

  }

  # combine to one data.frame
  result <- do.call(what = "rbind", args = result)

  # make sure bg comes first
  result <- result[order(result$part), ]

  # remove rownames
  row.names(result) <- 1:nrow(result)

  # return result list
  return(result)

}

calc_variability_internal <- function(values_i, values_m) {

  # alpha scale #

  # calculate sd and mean of local ecosystems i
  alpha_sd_i <- apply(X = values_i, MARGIN = 2, stats::sd, na.rm = TRUE)

  gamma_mean <- mean(values_m, na.rm = TRUE)

  # alpha_mean_i <- apply(X = values_i, MARGIN = 2, mean)
  #
  # # calculate cv of local ecosystems i
  # alpha_cv_i <- unname(alpha_sd_i / alpha_mean_i)

  # calculate weighted mean CV on alpha scale
  alpha_cv <- sum(alpha_sd_i) / gamma_mean

  # gamma scale #

  # calculate global gamma CV
  gamma_cv <- stats::sd(values_m, na.rm = TRUE) / gamma_mean

  # beta scale #

  # calculate beta as ratio of alpha to gamma
  beta_cv <- alpha_cv / gamma_cv

  # check if NaN because division by zero
  beta_cv <- ifelse(test = is.finite(beta_cv),
                    yes = beta_cv, no = 0)

  # synchrony #
  synchrony <- stats::var(values_m, na.rm = TRUE) / sum(alpha_sd_i) ^ 2

  # check if NaN because division by zero
  synchrony <- ifelse(test = is.finite(synchrony),
                      yes = synchrony, no = 0)

  # final list #

  # combine to final result list
  result_df <- data.frame(measure = c("alpha", "beta", "gamma", "synchrony"),
                          value = c(alpha_cv, beta_cv, gamma_cv, synchrony))

  return(result_df)
}
