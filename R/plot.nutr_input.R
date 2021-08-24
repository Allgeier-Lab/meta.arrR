#' plot.nutr_input
#'
#' @description
#' Plotting method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param add_total Logical if TRUE total input line is added.
#' @param viridis_option Character with option of viridis color option.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Ploting method for metaecosystem created with \code{setup_meta}.
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' plot(nutr_input)
#'
#' @aliases plot.nutr_input
#' @rdname plot.nutr_input
#'
#' @export
plot.nutr_input <- function(x, add_total = TRUE, base_size = 10, viridis_option = "C", ...) {

  # combine all list elements to one data.frame
  input_df <- do.call(rbind, lapply(seq_along(x$values), function(i) {

    # create timestep counter for each element of vector
    timestep <- seq(from = 1, to = length(x$values[[i]]), length.out = length(x$values[[i]]))

    # create data.frame
    data.frame(meta = i, timestep = timestep, input = x$values[[i]])

  }))

  # setupt color scale
  col_viridis <- viridis::viridis(n = length(x$values), option = viridis_option)

  # create plot
  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = input,
                                    col = factor(meta))) +
    ggplot2::geom_hline(yintercept = 0, color = "lightgrey", linetype = 2) +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  # add total input
  if (add_total) {

    # Formulas, one ~ one, one ~ many, many ~ one, and many ~ many:
    input_sum_df <- stats::aggregate(input ~ timestep, data = input_df, sum)

    gg_input <- gg_input +
      ggplot2::geom_line(data = input_sum_df, ggplot2::aes(x = timestep, y = input, col = "Total"),
                         linetype = 1) +
      ggplot2::scale_color_manual(name = "Metaecosystem", values = c(col_viridis, "grey"))

  # add only color scale
  } else {

    gg_input <- gg_input +
      ggplot2::scale_color_manual(name = "Metaecosystem", values = col_viridis)


  }

  return(gg_input)
}
