#' calc_cv
#'
#' @description
#' Simulate nutrient input.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param timestep Timestep only used for \code{meta_rn} object.
#' @param verbose If TRUE, progress reports are printed.
#'
#' @details
#' Calculate CV of provided code{nutr_input} object.
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

  # calculate global gamma CV
  gamma_cv <- stats::sd(gamma_sum) / mean(gamma_sum) * 100

  # combine to final result list
  result_list <- list(alpha_mad = alpha_mad, alpha_cv = alpha_cv,
                      beta_cor = beta_cor, gamma_cv = gamma_cv)

  # return result list
  return(result_list)
}

#' #' @name calc_cv
#' #' @export
#' calc_cv.meta_rn <- function(x, timestep = x$max_i) {
#'
#'   # get selected timestep
#'   timestep_slctd <- timestep
#'
#'   # check if i can be divided by save_each without reminder
#'   if (timestep_slctd %% x$save_each != 0) {
#'
#'     stop("'timestep' was not saved during model run.",
#'          call. = FALSE)
#'
#'   }
#'
#'   # # get data.frame with all seafloor values of selected timestep
#'   # seafloor <- do.call(rbind, lapply(seq_along(x$seafloor), function(i) {
#'   #
#'   #   id <- paste0("Metaecosystem ", i)
#'   #
#'   #   cbind(subset(x$seafloor[[i]], timestep == timestep_slctd,
#'   #                select = c("ag_biomass", "bg_biomass", "nutrients_pool", "detritus_pool")), id)
#'   #
#'   # }))
#'
#'   result_list <- list(local = numeric(1), global = numeric(1))
#'
#'   # return result list
#'   return(result_list)
#' }
