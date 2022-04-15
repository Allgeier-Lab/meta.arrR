#' plot.nutr_input
#'
#' @description
#' Plotting method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param gamma Logical if TRUE gamma values are plotted.
#' @param ... Not used.
#'
#' @details
#' Plotting method for metaecosystem created with \code{setup_meta}. If \code{gamma = TRUE}
#' the sum of all local metaecosystems is plotted additionally.
#'
#' @return ggplot
#'
#' @examples
#' nutrients_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, frequency = 3,
#' amplitude_sd = 0.5)
#' plot(nutrients_input)
#'
#' @aliases plot.nutr_input
#' @rdname plot.nutr_input
#'
#' @importFrom rlang .data
#'
#' @export
plot.nutr_input <- function(x, gamma = FALSE, ...) {

  # combine all list elements to one data.frame
  input_df <- get_input_df(x = x, long = TRUE)

  # create factor for facet plotting
  input_df$facet <- ifelse(input_df$meta == "gamma",
                           yes = "gamma scale", no = "alpha scale")

  input_df$facet <- factor(input_df$facet, levels = c("alpha scale", "gamma scale"))

  # subset data depending on alpha and gamma option
  if (gamma) {

    input_df <- input_df[input_df$facet == "gamma scale", ]

  } else {

    input_df <- input_df[input_df$facet == "alpha scale", ]

  }

  # create plot
  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = .data$timestep, y = .data$value,
                                    color = .data$meta)) +
    ggplot2::geom_hline(yintercept = 0, color = "darkgrey", linetype = 2) +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(. ~ .data$facet, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_viridis_d(name = "", option = "A") +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank())

  return(gg_input)
}
