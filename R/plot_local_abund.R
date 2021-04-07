#' plot_local_abund
#'
#' @description Plotting method for meta_rn object
#'
#' @param x ntr_inpt object.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plotting method for nutrient input created with \code{\link{simulate_input_sine}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases plot_local_abund
#' @rdname plot_local_abund
#'
#' @export
plot_local_abund <- function(x, base_size = 10, ...) {

  abundance <- get_abundance(result = x)

  abundance_sum <- stats::aggregate(x = abundance$abundance,
                                    by = list(timestep = abundance$timestep),
                                    FUN = mean)

  gg_input <- ggplot2::ggplot(data = abundance) +
    ggplot2::geom_hline(yintercept = x$starting_values$pop_n, linetype = 2,
                        col = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = abundance, col = factor(meta),
                                    linetype = "Local")) +
    ggplot2::geom_line(data = abundance_sum,
                       ggplot2::aes(x = timestep, y = x, linetype = "Regional"),
                       col = "black") +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
    ggplot2::labs(x = "Timestep", y = "Local abundance ") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_input)

}
