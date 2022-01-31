#' get_abundance
#'
#' @description
#' Get abundance of local metaecosystems.
#'
#' @param result \code{meta_rn} object simulated with \code{run_simulation_meta}.
#'
#' @details
#' Get the number of individuals within each local metaecosystem of model run result
#' created with \code{run_simulation_meta}.
#'
#' @return data.frame
#'
#' @examples
#' \dontrun{
#' get_abundance(result = result_rand)
#' }
#'
#' @aliases get_abundance
#' @rdname get_abundance
#'
#' @export
get_abundance <- function(result) {

  # check input
  if (!inherits(x = result, what = "meta_rn")) {

    stop("Please provide 'meta_rn' object.", call. = FALSE)

  }

  # loop through all metaecosystem and combine to data.frame
  abundance <- do.call(rbind, lapply(X = seq_along(result$fishpop), function(i) {

    # get only needed cols
    fishpop_temp <- result$fishpop[[i]][, c("id", "timestep")]

    # check which IDs are NA (no individual present)
    abundance_na <- fishpop_temp$timestep[is.na(fishpop_temp$id)]

    # count number of rows per timestep
    abundance_temp <- stats::aggregate(x = fishpop_temp$id,
                                       by = list(timestep = fishpop_temp$timestep),
                                       FUN = length)

    # replace names
    names(abundance_temp) <- c("timestep", "abundance")

    # replace all NA rows with 0 abundance
    abundance_temp[abundance_temp$timestep %in% abundance_na, "abundance"] <- 0

    # save meta id
    abundance_temp$meta <- i

    return(abundance_temp)
  }))

  # order by timestep and meta
  abundance <- abundance[order(abundance$timestep, abundance$meta), ]

  # order cols
  abundance <- abundance[, c("timestep", "meta", "abundance")]

  # remove row numbers because stupid
  rownames(abundance) <- NULL

  return(abundance)
}
