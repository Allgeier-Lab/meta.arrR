#' plot_production
#'
#' @description
#' Plot local abundance.
#'
#' @param result \code{meta_rn} object simulated with \code{run_meta}.
#' @param lag Logical if TRUE, the difference to the previous timestep is returned.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plot the production per meta ecosystem
#'
#' @examples
#' \dontrun{
#' plot_production(result = result_rand)
#' }
#'
#' @aliases plot_production
#' @rdname plot_production
#'
#' @export
plot_production <- function(result, lag = TRUE, base_size = 10, ...) {

  # calculate production
  production <- get_meta_production(result = result, lag = lag)

  # remove NA rows (first row)
  production <- production[stats::complete.cases(production), ]

  # better id col nameing
  production$meta <- paste0("Meta_", production$meta)

  # create title
  plot_title <- paste0("Total time        : ", result$max_i, " iterations [",
                       round(result$max_i * result$min_per_i / 60 / 24, 1), " days]",
                       "\nFishpop (total) : ", sum(result$starting_values$pop_n),
                       " indiv [Movement : ", result$movement, "]")

  # create plot
  gg_prod <- ggplot2::ggplot(data = production) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = value, col = factor(meta))) +
    ggplot2::facet_wrap(. ~ part, scales = "free_y", nrow = 2, ncol = 1) +
    ggplot2::scale_color_viridis_d(name = "", option = "C") +
    ggplot2::labs(x = "Timestep", y = "Biomass production", title = plot_title) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  return(gg_prod)

}
