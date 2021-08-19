#' plot.nutr_input
#'
#' @description
#' Plotting method for nutr_input object.
#'
#' @param x \code{nutr_input} object simulated with \code{sim_nutr_input_*}.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Ploting method for metaecosystem created with \code{setup_meta}.
#'
#' @examples
#' nutr_input <- sim_nutr_input(n = 3, max_i = 4380, freq_mn = 3,
#' input_max = 1, variability = 0.5)
#' plot(nutr_input)
#'
#' @aliases plot.nutr_input
#' @rdname plot.nutr_input
#'
#' @export
plot.nutr_input <- function(x, base_size = 10, ...) {

  # combine all list elements to one data.frame
  input_df <- do.call(rbind, lapply(seq_along(x$values), function(i){

    # create timestep counter for each element of vector
    timestep <- seq(from = 1, to = length(x$values[[i]]), length.out = length(x$values[[i]]))

    # create data.frame
    data.frame(meta = i, timestep = timestep, input = x$values[[i]])
    }))

  # create plot
  gg_input <- ggplot2::ggplot(data = input_df) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = input,
                                    col = factor(meta))) +
    ggplot2::geom_hline(yintercept = 0, color = "lightgrey", linetype = 2) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "C") +
    ggplot2::labs(x = "Timestep", y = "Nutrient input [g/cell]") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_input)
}
