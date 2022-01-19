#' plot.nutr_input
#'
#' @description
#' Plotting method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param alpha,gamma Logical if TRUE alpha and/or gamma are plotted.
#' @param viridis_option Character with option of viridis color option.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plotting method for metaecosystem created with \code{setup_meta}. If \code{gamma = TRUE}
#' the sum of all local metaecosystems is plotted additionally.
#'
#' @examples
#' nutrients_input <- sim_nutr_input(n = 3, max_i = 4380, input_mn = 1, freq_mn = 3,
#' variability = 0.5)
#' plot(nutrients_input)
#'
#' @aliases plot.nutr_input
#' @rdname plot.nutr_input
#'
#' @export
plot.nutr_input <- function(x, alpha = TRUE, gamma = TRUE,
                            base_size = 10, viridis_option = "C", ...) {

  # check if both are FALSE
  if (!alpha && !gamma) {

    stop("Either 'alpha' and/or 'gamma' must be TRUE.", call. = FALSE)

  }

  # combine all list elements to one data.frame
  input_df <- get_input_df(x = x, long = TRUE)

  # setup color scale
  col_viridis <- c(viridis::viridis(n = x$n, option = viridis_option), "black")

  # create factor for facet plotting
  input_df$Facet <- ifelse(input_df$Meta == "Gamma",
                           yes = "Gamma scale", no = "Alpha scale")

  input_df$Facet <- factor(input_df$Facet, levels = c("Alpha scale", "Gamma scale"))

  # subset data depending on alpha and gamma option
  if (!alpha) {

    input_df <- subset(input_df, Facet == "Gamma scale")

    # remove black color
    col_viridis <- "black"

  }

  if (!gamma) {

    input_df <- subset(input_df, Facet == "Alpha scale")

    # remove black color
    col_viridis <- col_viridis[-length(col_viridis)]

  }

  # get number of needed cols for legend
  ncol <- ifelse(test = gamma,
                 yes = ifelse(test = alpha,
                              yes = x$n + 1, no = 1),
                 no = x$n)

  # create plot
  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = Timestep, y = Value, color = Meta)) +
    ggplot2::geom_hline(yintercept = 0, color = "darkgrey", linetype = 2) +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom") +
    ggplot2::facet_wrap(. ~ Facet, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(name = "", values = col_viridis) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank()) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, ncol = ncol))

  return(gg_input)
}
