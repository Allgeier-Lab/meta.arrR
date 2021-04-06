#' get_abundance
#'
#' @description Get abundance of local metaecosystems
#'
#' @param result meta_rn object of simulation run.
#'
#' @details
#' Get the number of individuals within each local metaecosystem.
#'
#' @return data.frame
#'
#' @examples
#' # Add example code
#'
#' @aliases get_abundance
#' @rdname get_abundance
#'
#' @export
get_abundance <- function(resulti) {

  # check input
  if (!inherits(x = result, what = "meta_rn")) {

    stop("Please provide 'meta_rn' object.", call. = FALSE)

  }

  abundance <- do.call(rbind, lapply(X = seq_along(result$fishpop), function(i) {

    fishpop_temp <- subset(result$fishpop[[i]], select = c("id", "timestep"))

    abundance_temp <- stats::aggregate(id ~ timestep, data = fishpop_temp, length,
                                       na.action = na.pass)

    abundance_na  <- fishpop_temp$timestep[is.na(fishpop_temp$id)]

    abundance_temp$id[abundance_temp$timestep %in% abundance_na] <- 0

    names(abundance_temp) <- c("timestep", "abundance")

    abundance_temp$meta <- i

    return(abundance_temp)
  }))

  return(abundance)
}
