#' plot_nutrient_input
#'
#' @description Ploting method for ntr_inpt object
#'
#' @param x ntr_inpt object.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Ploting method for nutrient iinput created with \code{\link{simulate_input_sine}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases plot_nutrient_input
#' @rdname plot_nutrient_input
#'
#' @export
plot_nutrient_input <- function(x, base_size = 10, ...) {

  if (inherits(x = x, what = "data.frame")) {

    input_df <- x

  } else if (inherits(x = x, what = "list")) {

    input_df <- do.call(rbind, lapply(seq_along(x),
                                      function(i) data.frame(meta = i,
                                                             timestep = 1:length(x[[i]]),
                                                             input = x[[i]])))

  }

  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = input,
                                    col = factor(meta))) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::guides(col = FALSE) +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

  return(gg_input)

}
