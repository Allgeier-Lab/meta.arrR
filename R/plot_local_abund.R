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

  # get min and max values
  abundance_max <- max(abundance$abundance)

  abundance_min <- min(abundance$abundance)

  # combine to one df
  minmax_df <- rbind(data.frame(timestep = unique(abundance$timestep),
                                value = abundance_min, measure = "Min"),
                     data.frame(timestep = unique(abundance$timestep),
                                value = abundance_max, measure = "Max"),
                     data.frame(timestep = unique(abundance$timestep),
                                value = mean(x$starting_values$pop_n),
                                measure = "Mean"))

  # create plot
  gg_input <- ggplot2::ggplot(data = abundance) +
    ggplot2::geom_line(data = minmax_df, col = "lightgrey",
                       ggplot2::aes(x = timestep, y = value, linetype = measure)) +
    ggplot2::geom_point(ggplot2::aes(x = timestep, y = abundance,col = factor(meta))) +
    ggplot2::geom_path(ggplot2::aes(x = timestep, y = abundance, col = factor(meta)),
                       alpha = 1/3) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "", values = c("Min" = 2, "Max" = 2, "Mean" = 1)) +
    ggplot2::scale_y_continuous(limits = c(0, abundance_max + abundance_min),
                                breaks = 0:(abundance_max + abundance_min)) +
    ggplot2::labs(x = "Timestep", y = "Local abundance ") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_input)

}
