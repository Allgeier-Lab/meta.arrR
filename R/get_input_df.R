#' get_input_df
#'
#' @description
#' Get input data.frame
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input}.
#' @param gamma Logical if TRUE, the sum of gamma (regional) scale will be added.
#' @param long Logical if TRUE, \code{data.frame} will be reshaped to long format.
#'
#' @details
#' Returns a \code{data.frame} with all local nutrient inputs and total nutrients
#' input per timestep.
#'
#' @return data.frame
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' get_input_df(nutr_input)
#'
#' @aliases get_input_df
#' @rdname get_input_df
#'
#' @export
get_input_df <- function(x, gamma = TRUE, long = FALSE) {

  # cbind all local ecosystems
  input_df <- do.call("cbind", x$values)

  # convert to data.frame including timestep
  input_df <- data.frame(Timestep = 1:x$max_i, input_df)

  # calculate sum on regional/gamma scale
  if (gamma) {

    # calculate total input
    input_df$Gamma <- rowSums(input_df[, -1])

  }

  # reshape to long format
  if (long) {

    input_df <- stats::reshape(data = input_df, direction = "long",
                   v.names = "Value", varying = list(names(input_df[, -1])),
                   idvar = "Timestep", ids = input_df[, 1],
                   timevar = "Meta", times = names(input_df[, -1]),
                   new.row.names = seq(from = 1, to = nrow(input_df) *
                                         (ncol(input_df) - 1)))

    # set factor levels (gamma always last even if not present)
    input_df$Meta <- factor(input_df$Meta,
                            levels = c(paste0("Meta_", 1:x$n), "Gamma"))

  }

  return(input_df)
}
