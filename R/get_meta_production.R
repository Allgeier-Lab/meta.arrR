#' get_meta_production
#'
#' @description
#' Get global range
#'
#' @param result \code{meta_rn} object simulated \code{run_meta}.
#' @param lag Logical if TRUE, the difference to the previous timestep is returned.
#'
#' @details
#' Calculate the production of each metaecosystem. The \code{lag} argument allows to either
#' calculate the cumulative production or the increase between timesteps.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_meta_production(result = result_rand)
#' }
#'
#' @aliases get_meta_production
#' @rdname get_meta_production
#'
#' @export
get_meta_production <- function(result, lag = TRUE) {

  prod_temp <- lapply(X = seq_along(result$seafloor), FUN = function(i) {

    # select only required columns
    seafloor_temp <- subset(x = result$seafloor[[i]],
                            select = c("timestep", "ag_production", "bg_production"))

    # sum for each timestep
    seafloor_temp <- stats::aggregate(x = seafloor_temp[, -1],
                                      by = list(timestep = seafloor_temp$timestep),
                                      FUN = "sum")

    # use difference to previous timestep
    if (lag) {

      # ag_production
      seafloor_temp[, 2] <- c(NA, seafloor_temp[2:nrow(seafloor_temp), 2] -
                                seafloor_temp[1:(nrow(seafloor_temp) - 1), 2])

      # bg_production
      seafloor_temp[, 3] <- c(NA, seafloor_temp[2:nrow(seafloor_temp), 3] -
                                seafloor_temp[1:(nrow(seafloor_temp) - 1), 3])

    }

    # reshape to long format
    seafloor_temp <- stats::reshape(data = seafloor_temp, direction = "long",
                                    v.names = "value", varying = list(names(seafloor_temp[, -1])),
                                    idvar = "timestep", ids = seafloor_temp[, 1],
                                    timevar = "part", times = names(seafloor_temp[, -1]),
                                    new.row.names = seq(from = 1, to = nrow(seafloor_temp) * 2))

    # timestep not needed here, will be added in cbind call
    cbind(meta = i, seafloor_temp)

  })

  # combine to one data.frame
  prod_temp <- do.call(what = "rbind", args = prod_temp)

  return(prod_temp)

}
