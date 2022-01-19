#' sample_variability
#'
#' @description
#' Sample variability
#'
#' @param x \code{nutr_input} or \code{meta_rn} object.
#' @param itr Integer with number of sample iterations.
#' @param biomass,production Logical to specifiy if biomass and/or production is summarize.d
#' @param lag Vector with logical. If TRUE, the difference to the previous timestep is returned.
#' The first element refers to biomass, the second element to production.
#' @param verbose Logical if TRUE, progress reports are printed.
#'
#' @details
#' Samples coefficient of variation (on gamma scale) for increasing number of metaecosystems. For
#' more information see \code{calc_variability}. The \code{itr} arguments allows to
#' resample the data n times to capture differences based on different orders in which
#' the metaecosystems are sampled.
#'
#' @return data.frame
#'
#' @examples
#' nutrients_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' sample_variability(nutrients_input)
#'
#' \dontrun{
#' sample_variability(result)
#' }
#'
#' @aliases sample_variability
#' @rdname sample_variability
#'
#' @export
sample_variability <- function(x, itr, biomass, production, lag, verbose) UseMethod("sample_variability")

#' @name sample_variability
#' @export
sample_variability.nutr_input <- function(x, itr = 1, biomass = NULL, production = NULL,
                                          lag = NULL, verbose = TRUE) {

  # warning for lag argument
  if ((!is.null(biomass) || !is.null(production) || !is.null(lag)) && verbose) {

    warning("'biomass', 'production' or 'lag' are used for 'meta_rn' objects only.",
            call. = FALSE)

  }
  # convert to matrix
  values_i <- get_input_df(x = x, gamma = FALSE)[, -1, drop = FALSE]

  # loop through all iterations
  result <- lapply(1:itr, function(i) {

    if (verbose) {

      message("\r> Progress (nutr_input): ", i, " / ", itr, " iterations\t\t",
              appendLF = FALSE)

    }

    # create empty result df
    temp_df <- data.frame(n = numeric(x$n), alpha = numeric(x$n), beta = numeric(x$n),
                          gamma = numeric(x$n), synchrony = numeric(x$n))

    # shuffle id of metaecosystems
    n_sample <- sample(x = 1:x$n, size = x$n)

    # loop through 1...n meteecosystems
    for (j in 1:length(n_sample)) {

      # get increasing number of metaecosystems
      values_temp <- values_i[, n_sample[1:j], drop = FALSE]

      # calculate sum of each timestep
      values_m <- apply(X = values_temp, MARGIN = 1, FUN = sum, na.rm = FALSE)

      # calculate CV
      cv_temp <- calc_variability_internal(values_i = values_temp, values_m = values_m)

      # save results in data.frame
      temp_df[j, ] <- cbind(j, cv_temp[1, "value"], cv_temp[2, "value"],
                            cv_temp[3, "value"], cv_temp[4, "value"])

    }

    # add column for part
    cbind(itr = i, part = "nutr_input", temp_df)

  })

  if (verbose) {

    message("")

  }

  # calculate reshape and calc mean/sd
  result <- reshape_sample_interal(result = result)

  # return result data.frame
  return(result)
}

#' @name sample_variability
#' @export
sample_variability.meta_rn <- function(x, itr = 1, biomass = TRUE, production = TRUE,
                                       lag = c(FALSE, FALSE), verbose = TRUE) {

  # get sum of total local ecosystems
  result_sum <- summarize_meta(result = x, biomass = biomass, production = production,
                               lag = lag)

  # calculate variability for what parts
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

        itr_sample_var_internal(values_i = values_i, part = j, n = x$n, itr = itr,
                                verbose = verbose)
      })

      # combine to one data.frame
      result_temp <- do.call(what = "rbind", args = result_temp)

      # make sure bg comes first
      result_temp[order(result_temp$part, result_temp$stat, result_temp$n), ]

    }
  })

  # return result list
  return(result)
}

itr_sample_var_internal <- function(values_i, part, n, itr, verbose) {

  # loop through all iterations
  result_temp <- lapply(1:itr, function(i) {

    if (verbose) {

      message("\r> Progress (", part, "): ", i, " / ", itr, " iterations\t\t",
              appendLF = FALSE)

    }

    # shuffle id of metaecosystems
    n_total <- sample(x = 1:n, size = n)

    # sample variability
    cbind(itr = i, part = part, sample_variability_internal(values_i = values_i,
                                                            n_total = n_total))

  })

  # print linebreak
  if (verbose) {

    message("")

  }

  # calculate reshape and calc mean/sd
  result_temp <- reshape_sample_interal(result = result_temp)

  return(result_temp)
}

sample_variability_internal <- function(values_i, n_total) {

  # get total number of metaecosystems
  n <- length(n_total)

  # create empty data.frame to store results
  result_df <- data.frame(n = numeric(n), alpha = numeric(n), beta = numeric(n),
                          gamma = numeric(n), synchrony = numeric(n))

  # loop through 1...n metaecosystems
  for (i in 1:n) {

    # get increasing number of metaecosystems
    values_temp <- values_i[, n_total[1:i], drop = FALSE]

    # calculate sum of each timestep
    values_m <- apply(X = values_temp, MARGIN = 1, FUN = sum, na.rm = FALSE)

    # calculate CV
    cv_temp <- calc_variability_internal(values_i = values_temp, values_m = values_m)

    # save results in data.frame
    result_df[i, ] <- cbind(i, cv_temp[1, "value"], cv_temp[2, "value"],
                            cv_temp[3, "value"], cv_temp[4, "value"])

  }

  return(result_df)
}

reshape_sample_interal <- function(result) {

  # combine to one data.frame
  result <- do.call(what = "rbind", args = result)

  # reshape to long format
  result <- stats::reshape(result, direction = "long",
                           v.names = "value", varying = c("alpha", "beta", "gamma", "synchrony"),
                           timevar = "stat", times = c("alpha", "beta", "gamma", "synchrony"),
                           idvar = "itr", ids = itr,
                           new.row.names = 1:(nrow(result) * 4))

  # aggregate and convert to data.frame because aggregate returns strage matrix structure
  result <- do.call(what = "data.frame",
                    args = stats::aggregate(x = result[, "value"],
                                            by = list(part = result$part, n = result$n,
                                                      stat = result$stat),
                                            FUN = function(x) c(mn = mean(x), sd = stats::sd(x))))

  # nice column names
  names(result) <- c("part", "n", "stat", "mean", "sd")

  return(result)
}
