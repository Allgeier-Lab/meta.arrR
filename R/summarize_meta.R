#' summarize_meta
#'
#' @description
#' Summarize biomass and production values of each timestep
#'
#' @param result \code{meta_rn} object simulated \code{run_meta}.
#' @param biomass,production Logical to specifiy if biomass and/or production is summarize.d
#' @param fun Function to aggregate results. Passed on to \code{aggregate}.
#' @param na.rm Logical passed on to \code{aggregate}.
#' @param lag Vector with logical. If TRUE, the difference to the previous timestep is returned.
#' The first element refers to biomass, the second element to production.
#' @param return_df Logical if TRUE, a data.frame is returned.
#'
#' @details
#' Summarize the biomass and production values for each local metaecosystem and timestep.
#'
#' @return list
#'
#' @examples
#' \dontrun{
#' summarize_meta(result = result_rand)
#' }
#'
#' @aliases summarize_meta
#' @rdname summarize_meta
#'
#' @export
summarize_meta <- function(result, biomass = TRUE, production = TRUE,
                           fun = "sum", na.rm = TRUE, lag = c(FALSE, FALSE), return_df = TRUE) {

  # create input with parts
  parts <- list(biomass = ifelse(test = c(biomass, biomass),
                                 yes = c("bg_biomass", "ag_biomass"), no = NA),
                production = ifelse(test = c(production, production),
                                    yes = c("bg_production", "ag_production"), no = NA))

  # loop through
  result_sum <- lapply(seq_along(parts), function(i) {

    # return only NULL
    if (all(is.na(parts[[i]]))) {

      return(NULL)

    # summarize results
    } else {

      # calculate variability for what parts
      result_part <- lapply(X = result$seafloor, FUN = function(j) {

        # get all values until timestep and selected column
        seafloor_temp <- subset(x = j, select = c("timestep", parts[[i]]))

        # sum for each timestep
        seafloor_temp <- stats::aggregate(x = seafloor_temp[, -1],
                                          by = list(timestep = seafloor_temp$timestep),
                                          FUN = fun, na.rm = na.rm)

        # use difference to previous timestep
        if (lag[[i]]) {

          seafloor_temp <- calc_lag_internal(seafloor_temp)

        }

        # calculate total
        seafloor_temp[, paste0("ttl_", names(parts)[[i]])] <- apply(X = seafloor_temp[, -1], MARGIN = 1, FUN = "sum")

        return(seafloor_temp)

      })

      # return data.frame
      if (return_df) {

        # rbind to data.frame
        result_part <- do.call("rbind", result_part)

        # add meta id by repeating 1:n for each timestep
        result_part$meta <- rep(x = 1:result$n, each = length(seq(from = 0, to = result$max_i,
                                                                  by = result$save_each)))

        # reorder cols
        result_part <- result_part[, c(5, 1, 2, 3, 4)]

        # remove weird rownames
        row.names(result_part) <- 1:nrow(result_part)

      }

      return(result_part)

    }
  })

  # rename result list
  names(result_sum) <- names(parts)

  return(result_sum)
}

calc_lag_internal <- function(x) {

  # bg_production
  x[, 2] <- c(NA, x[2:nrow(x), 2] - x[1:(nrow(x) - 1), 2])

  # ag_production
  x[, 3] <- c(NA, x[2:nrow(x), 3] - x[1:(nrow(x) - 1), 3])

  return(x)
}
