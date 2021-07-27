#' plot.nutr_input
#'
#' @description
#' Ploting method for nutr_input object.
#'
#' @param x mdl_rn object of simulation run.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Ploting method for metaecosystem created with \code{\link{setup_meta}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases plot.nutr_input
#' @rdname plot.nutr_input
#'
#' @export
plot.nutr_input <- function(x, what = "seafloor", burn_in = TRUE, base_size = 10, ...) {

  # combine all list elements to one data.frame
  input_df <- do.call(rbind, lapply(seq_along(x), function(i){

    # create timestep counter for each element of vector
    timestep <- seq(from = 1, to = length(x[[i]]), length.out = length(x[[i]]))

    # create data.frame
    data.frame(meta = i, timestep = timestep, input = x[[i]])
    }))

  # create plot
  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = input,
                                    col = factor(meta))) +
    ggplot2::geom_hline(yintercept = 0, color = "lightgrey", linetype = 2) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    # ggplot2::guides(col = "none") +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_input)
}
