#' calc_cv
#'
#' @description
#' Simulate nutrient input.
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param timestep Numeric with maximum timestep Only used for \code{meta_rn} object.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Calculates relative median absolute deviation (MAD) and coefficient of variation (CV)
#' of provided \code{nutr_input} or \code{meta_rn} object.
#'
#' MAD = mad(x) / median(x) * 100
#'
#' CV = sd(x) / mean(x) * 100
#'
#' @return vector
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' calc_cv(nutr_input)
#'
#' @aliases calc_cv
#' @rdname calc_cv
#'
#' @export
calc_cv <- function(x, timestep, verbose) UseMethod("calc_cv")

#' @name calc_cv
#' @export
calc_cv.nutr_input <- function(x, timestep = NULL, verbose = TRUE) {

  # timestep not used
  if (!is.null(timestep) && verbose) {

    warning("'timestep' is only used for mdl_rn object.", call. = FALSE)

  }

  # preprocess values #

  # convert to matrix
  values_mat <- do.call("cbind", x$values)

  # alpha scale #

  # calculate relative Median Absolute Deviation
  alpha_mad <- apply(values_mat, MARGIN = 2, FUN = function(i)
    stats::mad(i) / stats::median(i) * 100)

  # calc local alpha CVs
  alpha_cv <- apply(values_mat, MARGIN = 2, FUN = function(i)
    stats::sd(i) / mean(i) * 100)

  # beta scale #

  # calculate correlation matrix for synchronicity
  beta_cor <- stats::cor(values_mat)

  # get only lower triangle
  beta_cor <- beta_cor[lower.tri(beta_cor, diag = FALSE)]

  # set names
  names(beta_cor) <- apply(X = utils::combn(1:length(x$values), 2),
                           MARGIN = 2, FUN = paste, collapse = "_")

  # gamma scale #

  # calculate sum of each timestep
  gamma_sum <- apply(X = values_mat, MARGIN = 1, FUN = sum)

  # calculate global gamma MAD
  gamma_mad <- stats::mad(gamma_sum) / stats::median(gamma_sum) * 100

  # calculate global gamma CV
  gamma_cv <- stats::sd(gamma_sum) / mean(gamma_sum) * 100

  # combine to final result list
  result_list <- list(alpha_mad = alpha_mad, alpha_cv = alpha_cv,
                      beta_cor = beta_cor,
                      gamma_mad = gamma_mad, gamma_cv = gamma_cv)

  # return result list
  return(result_list)
}

#' @name calc_cv
#' @export
calc_cv.meta_rn <- function(x, timestep = x$max_i, verbose = TRUE) {

  # get selected timestep
  timestep_slctd <- timestep

  # check if i can be divided by save_each without reminder
  if (timestep_slctd %% x$save_each != 0 || timestep_slctd > x$max_i) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)

  }

  # summarize values of each timestep
  seafloor_sum <- lapply(x$seafloor, function(i) {

    seafloor_temp <- subset(i, timestep <= timestep_slctd,
                            select = c("timestep", "bg_biomass", "ag_biomass"))

    seafloor_temp <- stats::aggregate(x = seafloor_temp[, c(2:3)],
                                      by = list(timestep = seafloor_temp$timestep),
                                      FUN = "sum")
  })

  # calculate mad and cv for each metaecosystem
  alpha <- do.call("rbind", lapply(seq_along(seafloor_sum), function(i) {

    # calculate relative Median Absolute Deviation
    alpha_mad <- unname(apply(X = seafloor_sum[[i]][, c(2:3)], MARGIN = 2, function(i)
      stats::mad(i) / stats::median(i) * 100))

    # calculate cv
    alpha_cv <- unname(apply(X = seafloor_sum[[i]][, c(2:3)], MARGIN = 2, function(i)
      stats::sd(i) / mean(i) * 100))

    # combine to one data.frame
    data.frame(metaecosystem = rep(x = i, times = 2), part = c("bg", "ag"),
               mad = alpha_mad, cv = alpha_cv)

  }))

  # create sequence of all present timesteps
  timesteps_unique <- seq(from = 0, to = x$max_i, by = x$save_each)

  # create empty data.frame
  seafloor_gamma <- data.frame(timestep = timesteps_unique,
                               bg_biomass = numeric(length(timesteps_unique)),
                               ag_biomass = numeric(length(timesteps_unique)))

  # loop through all metaecosystems
  for (i in 1:x$n) {

    seafloor_gamma$bg_biomass <- seafloor_gamma$bg_biomass + seafloor_sum[[i]]$bg_biomass

    seafloor_gamma$ag_biomass <- seafloor_gamma$ag_biomass + seafloor_sum[[i]]$ag_biomass

  }

  gamma_mad <- unname(apply(X = seafloor_gamma[, c(2:3)], MARGIN = 2, function(i)
    stats::mad(i) / stats::median(i) * 100))


  gamma_cv <- unname(apply(X = seafloor_gamma[, c(2:3)], MARGIN = 2, function(i)
    stats::sd(i) / mean(i) * 100))

  gamma <- data.frame(part = c("bg", "ag"), mad = gamma_mad, cv = gamma_cv)

  result_list <- list(alpha = alpha, gamma = gamma)

  # return result list
  return(result_list)
}
