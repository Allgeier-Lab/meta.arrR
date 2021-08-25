#' plot.nutr_input
#'
#' @description
#' Plotting method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param total Logical if TRUE total input line is added.
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
plot.nutr_input <- function(x, total = TRUE, base_size = 10, viridis_option = "C", ...) {

  # combine all list elements to one data.frame
  input_df <-  get_global_input(x = x, long = TRUE)

  input_df$Facet <- ifelse(input_df$Meta == "Total", yes = "Total", no = "Metaecosystems")

  # setupt color scale
  col_viridis <- viridis::viridis(n = length(x$values), option = viridis_option)

  # create plot
  gg_input <- ggplot2::ggplot(data = subset(input_df, Meta != "Total")) +
    ggplot2::geom_line(ggplot2::aes(x = Timestep, y = Value,
                                    col = factor(Meta))) +
    ggplot2::geom_hline(yintercept = 0, color = "lightgrey", linetype = 2) +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  # add total input
  if (total) {

    gg_input <- gg_input +
      ggplot2::geom_line(data = subset(input_df, Meta == "Total"),
                         ggplot2::aes(x = Timestep, y = Value, col = "Total"),
                         linetype = 1) +
      ggplot2::scale_color_manual(name = "", values = c(col_viridis, "black")) +
      ggplot2::facet_wrap(. ~ Facet, ncol = 1)

  # add only color scale
  } else {

    gg_input <- gg_input +
      ggplot2::scale_color_manual(name = "", values = col_viridis)


  }

  return(gg_input)
}
