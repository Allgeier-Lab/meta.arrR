#' calc_cv
#'
#' @description
#' Simulate nutrient input.
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param what String specifying which column us used for \code{meta_rn} object.
#' @param timestep Numeric with maximum timestep Only used for \code{meta_rn} object.
#' @param verbose Logical if TRUE progress reports are printed.
#'
#' @details
#' Calculates relative median absolute deviation (MAD), coefficient of variation (CV), and
#' forecastability (Omega) of provided \code{nutr_input} or \code{meta_rn} object.
#'
#' MAD = mad(x) / median(x) * 100
#'
#' CV = sd(x) / mean(x) * 100
#'
#' @references
#' Goerg, G., 2013. Forecastable component analysis, in: Dasgupta, S., McAllester, D. (Eds.),
#' Proceedings of the 30th International Conference on Machine Learning, Proceedings
#' of Machine Learning Research. Proceedings of Machine Learning Research, Atlanta, USA, pp. 64–72.
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
calc_cv <- function(x, what, timestep, verbose) UseMethod("calc_cv")

#' @name calc_cv
#' @export
calc_cv.nutr_input <- function(x, what = NULL, timestep = NULL, verbose = TRUE) {

  # timestep not used
  if (!is.null(timestep) || !is.null(what)  && verbose) {

    warning("'timestep' is only used for mdl_rn object.", call. = FALSE)

  }

  # preprocess values #

  # convert to matrix
  values_mat <- do.call("cbind", x$values)

  # alpha scale #

  # calculate relative Median Absolute Deviation
  alpha_mad <- unname(apply(X = values_mat, MARGIN = 2, FUN = function(i) {
    stats::mad(i) / stats::median(i) * 100}))

  # calc local alpha CVs
  alpha_cv <- unname(apply(X = values_mat, MARGIN = 2, FUN = function(i) {
    stats::sd(i) / mean(i) * 100}))

  # calculate local alpha forecastability
  alpha_omega <- unname(apply(X = values_mat, MARGIN = 2, FUN = function(i) {

    ifelse(test = length(unique(i)) == 1,
           yes = 100, no = as.numeric(ForeCA::Omega(i)))

   }))

  # combine to data.frame
  alpha_df <- data.frame(Meta = 1:x$n, MAD = alpha_mad, CV = alpha_cv,
                         Omega = alpha_omega)

  # beta scale #

  # calculate correlation matrix for synchronicity
  beta_cor <- suppressWarnings(stats::cor(values_mat))

  # get only lower triangle
  beta_cor <- beta_cor[lower.tri(x = beta_cor, diag = FALSE)]

  # get all combinations of correlation
  id_combinations <- utils::combn(1:x$n, 2)

  # combine to data.frame
  beta_df <- data.frame(Meta_a = id_combinations[1, ], Meta_b = id_combinations[2, ],
                        Correlation = beta_cor)

  # gamma scale #

  # calculate sum of each timestep
  gamma_sum <- apply(X = values_mat, MARGIN = 1, FUN = sum)

  # calculate global gamma MAD
  gamma_mad <- stats::mad(gamma_sum) / stats::median(gamma_sum) * 100

  # calculate global gamma CV
  gamma_cv <- stats::sd(gamma_sum) / mean(gamma_sum) * 100

  # calculate global gamma forecastability
  gamma_omega <- ifelse(test = length(unique(gamma_sum)) == 1,
                        yes = 100, no = as.numeric(ForeCA::Omega(gamma_sum)))

  # combine to data.frame
  gamma_df <- data.frame(Meta = NA, MAD = gamma_mad, CV = gamma_cv, Omega = gamma_omega)

  # combine to final result list
  result_list <- list(alpha = alpha_df, beta = beta_df, gamma = gamma_df)

  # return result list
  return(result_list)
}

#' @name calc_cv
#' @export
calc_cv.meta_rn <- function(x, what = "ag_biomass", timestep = x$max_i, verbose = TRUE) {

  # MH: What about fishpop?

  # preprocess data #

  # check if what makes sense
  if (!what %in% names(x$seafloor[[1]])) {

    stop("Please select column of seafloor data.frame.", call. = FALSE)

  }

  # get selected timestep
  timestep_slctd <- timestep

  # check if i can be divided by save_each without reminder
  if (timestep_slctd %% x$save_each != 0 || timestep_slctd > x$max_i) {

    stop("'timestep' was not saved during model run.",
         call. = FALSE)

  }

  # summarize values of each timestep
  seafloor_sum <- lapply(X = x$seafloor, FUN = function(i) {

    # get all values until timestep and selected column
    seafloor_temp <- subset(x = i, timestep <= timestep_slctd,
                            select = c("timestep", what))

    # sum for each timestep
    seafloor_temp <- stats::aggregate(x = seafloor_temp[, what],
                                      by = list(timestep = seafloor_temp$timestep),
                                      FUN = "sum")
  })

  # init matrix for beta correlation
  values_mat <- matrix(nrow = length(seq(from = 0, to = x$max_i, by = x$save_each)),
                       ncol = x$n)

  # fill matrix with meta values
  for (i in 1:x$n) {

    values_mat[, i] <- seafloor_sum[[i]][, 2]

  }


  # alpha scale #

  # calculate mad and cv for each metaecosystem
  alpha_df <- do.call("rbind", lapply(X = seq_along(seafloor_sum), FUN = function(i) {

    # calculate relative Median Absolute Deviation
    alpha_mad <- stats::mad(seafloor_sum[[i]][, 2]) / stats::median(seafloor_sum[[i]][, 2]) * 100

    # calculate cv
    alpha_cv <- stats::sd(seafloor_sum[[i]][, 2]) / mean(seafloor_sum[[i]][, 2]) * 100

    # calculate local alpha forecastability
    alpha_omega <- ifelse(test = length(unique(seafloor_sum[[i]][, 2])) == 1,
                          yes = 100, no = as.numeric(ForeCA::Omega(seafloor_sum[[i]][, 2])))

    # combine to one data.frame
    data.frame(Meta = i, mad = alpha_mad, cv = alpha_cv, omega = alpha_omega)

  }))

  # beta scale #

  # calculate correlation matrix for synchronicity
  beta_cor <- suppressWarnings(stats::cor(values_mat))

  # get only lower triangle
  beta_cor <- beta_cor[lower.tri(x = beta_cor, diag = FALSE)]

  # get all combinations of correlation
  id_combinations <- utils::combn(1:x$n, 2)

  # combine to data.frame
  beta_df <- data.frame(Meta_a = id_combinations[1, ], Meta_b = id_combinations[2, ],
                        Correlation = beta_cor)

  # gamma scale #

  # create sequence of all present timesteps
  timesteps_unique <- seq(from = 0, to = x$max_i, by = x$save_each)

  # create empty data.frame
  seafloor_total <- data.frame(timestep = timesteps_unique,
                               x = numeric(length(timesteps_unique)))

  # loop through all metaecosystems
  for (i in 1:x$n) {

    seafloor_total$x <- seafloor_total$x + seafloor_sum[[i]]$x

  }

  # calculad MAD on gamma scale
  gamma_mad <- stats::mad(seafloor_total$x) / stats::median(seafloor_total$x) * 100

  # calculate CV on gamma scale
  gamma_cv <- stats::sd(seafloor_total$x) / mean(seafloor_total$x) * 100

  # calculate global gamma forecastability
  gamma_omega <- ifelse(test = length(unique(seafloor_total$x)) == 1,
                        yes = 100, no = as.numeric(ForeCA::Omega(seafloor_total$x)))

  gamma_df <- data.frame(Meta = NA, mad = gamma_mad, cv = gamma_cv, omega = gamma_omega)

  result_list <- list(alpha = alpha_df, beta = beta_df, gamma = gamma_df)

  # return result list
  return(result_list)
}
