#' plot_local_abund
#'
#' @description
#' Plot local abundance.
#'
#' @param result \code{meta_rn} object simulated with \code{run_simulation_meta}.
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
plot_local_abund <- function(result) {

  # get abundance of all metaecosystems
  abundance <- get_abundance(result = result)

  abundance_mean <- mean(abundance$abundance)

  # abundance_max <- max(abundance$abundance)

  # create title
  title <- paste0("Fishpop (total) : ", sum(result$starting_values$pop_n),
                  " indiv [Movement : ", result$movement, "]")

  # create plot
  gg_input <- ggplot2::ggplot(data = abundance) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey") +
    ggplot2::geom_hline(yintercept = abundance_mean, linetype = 2, color = "grey") +
    # ggplot2::geom_hline(yintercept = sum(result$starting_values$pop_n), linetype = 2, color = "grey") +
    ggplot2::geom_point(ggplot2::aes(x = .data$timestep, y = .data$abundance, col = factor(.data$meta))) +
    ggplot2::geom_path(ggplot2::aes(x = .data$timestep, y = .data$abundance, col = factor(.data$meta)),
                       alpha = 1/3) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "A") +
    ggplot2::scale_linetype_manual(name = "", values = c("Min" = 2, "Max" = 2, "Mean" = 1)) +
    ggplot2::labs(x = "Timestep", y = "Local abundance", title = title) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(gg_input)

}
