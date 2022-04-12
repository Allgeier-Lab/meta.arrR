#' plot_meta_production
#'
#' @description
#' Plot local abundance.
#'
#' @param result \code{meta_rn} object simulated with \code{run_simulation_meta}.
#' @param lag Logical if TRUE, the difference to the previous timestep is returned.
#'
#' @details
#' Plot the production per meta ecosystem.
#'
#' @return ggplot
#'
#' @examples
#' \dontrun{
#' plot_meta_production(result = result_rand)
#' }
#'
#' @aliases plot_meta_production
#' @rdname plot_meta_production
#'
#' @importFrom rlang .data
#'
#' @export
plot_meta_production <- function(result, lag = c(FALSE, TRUE)) {

  # calculate production
  production <- summarize_meta(result = result, biomass = FALSE, production = TRUE,
                               lag = lag)[["production"]]

  # remove NA rows (first row)
  production <- production[stats::complete.cases(production), ]

  # better id col nameing
  production$meta <- paste0("Meta_", production$meta)

  production <- stats::reshape(production, direction = "long",
                               v.names = "value", varying = c("bg_production", "ag_production", "ttl_production"),
                               timevar = "part", times = c("bg_production", "ag_production", "ttl_production"),
                               new.row.names = 1:(nrow(production) * 3),
                               idvar = "timestep", ids = unique(production$timestep))

  # create title
  plot_title <- paste0("Total time        : ", result$max_i, " iterations [",
                       round(result$max_i * result$min_per_i / 60 / 24, 1), " days]",
                       "\nFishpop (total) : ", sum(result$starting_values$pop_n),
                       " indiv [Movement : ", result$movement, "]")

  # create plot
  gg_prod <- ggplot2::ggplot(data = production) +
    ggplot2::geom_line(ggplot2::aes(x = .data$timestep, y = .data$value,
                                    col = factor(.data$meta))) +
    ggplot2::facet_wrap(. ~ .data$part, scales = "free_y", nrow = 3, ncol = 1) +
    ggplot2::scale_color_viridis_d(name = "", option = "A") +
    ggplot2::labs(x = "Timestep", y = "Biomass production", title = plot_title) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(gg_prod)

}
