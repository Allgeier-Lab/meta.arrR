#' plot.meta_rn
#'
#' @description Ploting method for meta_rn object
#'
#' @param x meta_rn object.
#' @param digits Numeric of decimal places (round).
#' @param ... Arguments passed to cat.
#'
#' @details
#' Ploting method for metaecosystem created with \code{\link{setup_meta}}.
#'
#' @examples
#' # Add example code
#'
#' @aliases plot.meta_rn
#' @rdname plot.meta_rn
#'
#' @export
plot.meta_rn <- function(x, what = "seafloor", burn_in = TRUE,
                           limits = NULL, base_size = 10, ...) {
  # set color for burn in threshold
  col_burn <- ifelse(test = burn_in, yes = "grey", no = NA)

  burn_in_itr <- x$burn_in

  result_sum <- lapply(1:x$n, function(i)
    arrR::summarize_mdlrn(list(seafloor = x$seafloor[[i]], fishpop = x$fishpop[[i]],
                               burn_in = x$burn_in),
                          summary = "mean"))

  if (what == "seafloor") {

    seafloor_df <- do.call(rbind, lapply(1:x$n, function(i)
      cbind(meta = i, result_sum[[i]]$seafloor)))
    # create plot
    gg_top_left <- ggplot2::ggplot(data = seafloor_df) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = ag_biomass,
                                      col = factor(meta))) +
      ggplot2::scale_y_continuous(limits = limits$ag_biomass) +
      ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
      ggplot2::labs(x = "Timestep", y = "Dry weight ag biomass [g/cell]") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")
    # create plot
    gg_top_right <- ggplot2::ggplot(data = seafloor_df) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = bg_biomass,
                                      col = factor(meta))) +
      ggplot2::scale_y_continuous(limits = limits$bg_biomass) +
      ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
      ggplot2::guides(col = FALSE) +
      ggplot2::labs(x = "Timestep", y = "Dry weight bg biomass [g/cell]") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")
    # create plot
    gg_bottom_left <- ggplot2::ggplot(data = seafloor_df) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = nutrients_pool,
                                      col = factor(meta))) +
      ggplot2::scale_y_continuous(limits = limits$nutrients_pool) +
      ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
      ggplot2::guides(col = FALSE) +
      ggplot2::labs(x = "Timestep", y = "Nutrients pool [g/cell]") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")

    # create plot
    gg_bottom_right <- ggplot2::ggplot(data = seafloor_df) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = detritus_pool,
                                      col = factor(meta))) +
      ggplot2::scale_y_continuous(limits = limits$detritus_pool) +
      ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
      ggplot2::guides(col = FALSE) +
      ggplot2::labs(x = "Timestep", y = "Detritus pool [g/cell]") +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")

  } else if (what == "fishpop") {

    stop("what = fishpop not implemented yet.", call. = FALSE)

  } else {

    stop("Please select either what = 'seafloor' or what = 'fishpop'",
         call. = FALSE)

  }

  # create title
  plot_title <- paste0("Total time : ", x$max_i, " iterations (",
                       round(x$max_i * x$min_per_i / 60 / 24, 1), " days)",
                       "\nFishpop    : ", x$starting_values$pop_n,
                       " indiv (Reef attraction: ", x$reef_attraction, ")")

  # now add the title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(label = plot_title, x = 0, hjust = 0, size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 1, "cm"))

  legend_top_left <- cowplot::get_legend(
    gg_top_left
  )

  # combine to one grid
  gg_all <- cowplot::plot_grid(gg_top_left + ggplot2::theme(legend.position = "none"),
                               gg_top_right + ggplot2::theme(legend.position = "none"),
                               gg_bottom_left + ggplot2::theme(legend.position = "none"),
                               gg_bottom_right + ggplot2::theme(legend.position = "none"),
                               nrow = 2, ncol = 2)

  # add title
  gg_all <- cowplot::plot_grid(title, gg_all, legend_top_left, ncol = 1, rel_heights = c(0.1, 1, 0.1))

  return(gg_all)

}
