#' get_input_df
#'
#' @description
#' Get input data.frame
#'
#' @param x \code{nutr_input} object simulated with \code{simulate_nutrient_*}.
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
#' \dontrun{
#' nutrients_input <- simulate_nutrient_sine(n = 3, max_i = 4380, input_mn = 1,
#' frequency = 3, noise = 0.5)
#' get_input_df(nutrients_input)
#' }
#'
#' @aliases get_input_df
#' @rdname get_input_df
#'
#' @export
get_input_df <- function(x, gamma = TRUE, long = FALSE) {

  # cbind all local ecosystems
  input_df <- do.call("cbind", lapply(X = x$values, function(i) i[, 2]))

  # convert to data.frame including timestep
  input_df <- data.frame(timestep = x$values[[1]]$timestep, input_df)

  # calculate sum on regional/gamma scale
  if (gamma) {

    # calculate total input
    input_df$gamma <- rowSums(input_df[, -1, drop = FALSE])

  }

  # reshape to long format
  if (long) {

    # only one meta-ecosystem present so reshape not working due to not-unique ids
    if (x$n == 1) {

      input_df <- data.frame(timestep = input_df$timestep,
                             meta = "meta_1", value = input_df$meta_1)

    # reshape from wide to long
    } else {

      input_df <- stats::reshape(data = input_df, direction = "long",
                                 v.names = "value", varying = list(names(input_df[, -1])),
                                 idvar = "timestep", ids = input_df[, 1],
                                 timevar = "meta", times = names(input_df[, -1]),
                                 new.row.names = seq(from = 1, to = nrow(input_df) *
                                                       (ncol(input_df) - 1)))

    }

    # set factor levels (gamma always last even if not present)
    input_df$meta <- factor(input_df$meta,
                            levels = c(paste0("meta_", 1:x$n), "gamma"))

  }

  return(input_df)
}
