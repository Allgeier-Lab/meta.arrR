#' plot_local_abund
#'
#' @description
#' Plot local abundance.
#'
#' @param result \code{meta_rn} object simulated with \code{run_simulation_meta}.
#' @param base_size Numeric to specify base font size.
#'
#' @details
#' The resulting plot describes the number of individuals within each local metaecosystem
#' over time steps. Additional, the minimum, maximum and mean of all metaecosystems are
#' included.
#'
#' @return ggplot
#'
#' @examples
#' \dontrun{
#' plot_local_abund(result = result_rand)
#' }
#'
#' @aliases plot_local_abund
#' @rdname plot_local_abund
#'
#' @importFrom rlang .data
#'
#' @export
plot_local_abund <- function(result, base_size = 10) {

  # get abundance of all metaecosystems
  abundance <- get_abundance(result = result)

  # get min and max values
  abundance_max <- max(abundance$abundance)

  abundance_min <- min(abundance$abundance)

  # combine to one df
  minmax_df <- rbind(data.frame(timestep = unique(abundance$timestep),
                                value = abundance_min, measure = "Min"),
                     data.frame(timestep = unique(abundance$timestep),
                                value = abundance_max, measure = "Max"),
                     data.frame(timestep = unique(abundance$timestep),
                                value = mean(result$starting_values$pop_n),
                                measure = "Mean"))

  # create title
  title <- paste0("Fishpop (total) : ", sum(result$starting_values$pop_n),
                  " indiv [Movement : ", result$movement, "]")

  # create plot
  gg_input <- ggplot2::ggplot(data = abundance) +
    ggplot2::geom_line(data = minmax_df, col = "lightgrey",
                       ggplot2::aes(x = .data$timestep, y = .data$value,
                                    linetype = .data$measure)) +
    ggplot2::geom_point(ggplot2::aes(x = .data$timestep, y = .data$abundance,
                                     col = factor(.data$meta))) +
    ggplot2::geom_path(ggplot2::aes(x = .data$timestep, y = .data$abundance,
                                    col = factor(.data$meta)),
                       alpha = 1/3) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "", values = c("Min" = 2, "Max" = 2, "Mean" = 1)) +
    ggplot2::scale_y_continuous(limits = c(0, abundance_max + abundance_min),
                                breaks = 0:(abundance_max + abundance_min)) +
    ggplot2::labs(x = "Timestep", y = "Local abundance", title = title) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_input)

}
