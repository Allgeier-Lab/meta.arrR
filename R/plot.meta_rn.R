#' plot.meta_rn
#'
#' @description
#' Plotting method for meta_rn object.
#'
#' @param x \code{meta_rn} object simulated with \code{simulate_meta}.
#' @param what Character specifying what to plot.
#' @param burn_in If TRUE, line to indicate burn-in time is plotted.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plotting method for result of metaecosystem model run created
#' with \code{simulate_meta}.
#'
#' @examples
#' \dontrun{
#' plot(result_attr)
#' }
#'
#' @aliases plot.meta_rn
#' @rdname plot.meta_rn
#'
#' @export
plot.meta_rn <- function(x, what = "seafloor", burn_in = TRUE, base_size = 10, ...) {

  if (!what %in% c("seafloor", "fishpop")) {

    stop("Please select either what = 'seafloor' or what = 'fishpop'",
         call. = FALSE)
  }

  # set color for burn in threshold
  col_burn <- ifelse(test = burn_in, yes = "grey", no = NA)

  # get burn_in value for filtering
  burn_in_itr <- x$burn_in

  # get separated values for each local metaecosystem
  result_sep <- lapply(1:x$n, function(i)
    arrR::summarize_mdlrn(list(seafloor = x$seafloor[[i]], fishpop = x$fishpop[[i]],
                               burn_in = x$burn_in),
                          summary = "mean"))

  # get total mean value
  result_sum <- arrR::summarize_mdlrn(result = list(seafloor = do.call(rbind, x$seafloor),
                                                    fishpop = do.call(rbind, x$fishpop),
                                                    burn_in = x$burn_in),
                                      summary = "mean")

  if (what == "seafloor") {

    # get only seafloor results
    result_sep <- do.call(rbind, lapply(1:x$n, function(i)
      cbind(meta = i, result_sep[[i]]$seafloor)))

    # order cols of local metaecosystems
    result_sep <- result_sep[, c("meta", "timestep",
                                 "ag_biomass", "bg_biomass",
                                 "nutrients_pool", "detritus_pool")]

    # order cols total values
    result_sum <- result_sum$seafloor[, c("timestep",
                                          "ag_biomass", "bg_biomass",
                                          "nutrients_pool", "detritus_pool")]

    # setup labels
    y_labels <- c("Dry weight ag biomass [g/cell]", "Dry weight bg biomass [g/cell]",
                  "Nutrients pool [g/cell]", "Detritus pool [g/cell]")

    # MH: check if limits are !is.null() and rename to top_left,...

  } else if (what == "fishpop") {

    # get results of fishpop only
    result_sep <- do.call(rbind, lapply(1:x$n, function(i)
      cbind(meta = i, result_sep[[i]]$fishpop)))

    # order cols of local metaecosystems
    result_sep <- result_sep[, c("meta", "timestep",
                                 "length", "weight",
                                 "died_consumption", "died_background")]

    # order total mean value
    result_sum <- result_sum$fishpop[, c("timestep",
                                         "length", "weight",
                                         "died_consumption", "died_background")]

    # setup labels
    y_labels <- c("Body length [cm]", "Body weigth [g]",
                  "Count mortality consumption [#]", "Count mortality background [#]")

    # MH: check if limits are !is.null() and rename to top_left,...

  }

  # setup names of list
  names(result_sep) <- c("meta", "timestep",
                         "top_left", "top_right",
                         "bottom_left", "bottom_right")

  names(result_sum) <- c("timestep",
                         "top_left", "top_right",
                         "bottom_left", "bottom_right")

  # create plot
  gg_top_left <- ggplot2::ggplot(data = result_sep) +
    ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = top_left, col = factor(meta),
                                    linetype = "Local")) +
    ggplot2::geom_line(data = result_sum,
                       ggplot2::aes(x = timestep, y = top_left, linetype = "Regional"),
                       col = "black") +
    # ggplot2::scale_y_continuous(limits = limits$ag_biomass) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
    ggplot2::labs(x = "Timestep", y = y_labels[1]) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                   legend.position = "bottom")

  # create plot
  gg_top_right <- ggplot2::ggplot(data = result_sep) +
    ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = top_right, col = factor(meta),
                                    linetype = "Local")) +
    ggplot2::geom_line(data = result_sum,
                       ggplot2::aes(x = timestep, y = top_right, linetype = "Regional"),
                       col = "black") +
    # ggplot2::scale_y_continuous(limits = limits$bg_biomass) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
    ggplot2::guides(col = "none", linetype = "none") +
    ggplot2::labs(x = "Timestep", y = y_labels[2]) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

# create plot
  gg_bottom_left <- ggplot2::ggplot(data = result_sep) +
    ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = bottom_left, col = factor(meta),
                                    linetype = "Local")) +
    ggplot2::geom_line(data = result_sum,
                       ggplot2::aes(x = timestep, y = bottom_left, linetype = "Regional"),
                       col = "black") +
    # ggplot2::scale_y_continuous(limits = limits$nutrients_pool) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
    ggplot2::guides(col = "none", linetype = "none") +
    ggplot2::labs(x = "Timestep", y = y_labels[3]) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

  # create plot
  gg_bottom_right <- ggplot2::ggplot(data = result_sep) +
    ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
    ggplot2::geom_line(ggplot2::aes(x = timestep, y = bottom_right, col = factor(meta),
                                    linetype = "Local")) +
    ggplot2::geom_line(data = result_sum,
                       ggplot2::aes(x = timestep, y = bottom_right, linetype = "Regional"),
                       col = "black") +
    # ggplot2::scale_y_continuous(limits = limits$detritus_pool) +
    ggplot2::scale_color_viridis_d(name = "Metaecosystem", option = "D") +
    ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
    ggplot2::guides(col = "none", linetype = "none") +
    ggplot2::labs(x = "Timestep", y = y_labels[4]) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

  legend_top_left <- cowplot::get_legend(gg_top_left)

  # create title
  plot_title <- paste0("Total time : ", x$max_i, " iterations (",
                       round(x$max_i * x$min_per_i / 60 / 24, 1), " days)",
                       "\nFishpop    : ", x$starting_values$pop_n,
                       " indiv (Movement: ", x$movement, ")")

  # now add the title
  title <- cowplot::ggdraw() +
    cowplot::draw_label(label = plot_title, x = 0, hjust = 0, size = base_size) +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 1, "cm"))

  # combine to one grid
  gg_all <- cowplot::plot_grid(gg_top_left + ggplot2::theme(legend.position = "none"),
                               gg_top_right,
                               gg_bottom_left,
                               gg_bottom_right,
                               nrow = 2, ncol = 2)

  # add title
  gg_all <- cowplot::plot_grid(title, gg_all, legend_top_left, ncol = 1, rel_heights = c(0.1, 1, 0.1))

  return(gg_all)

}
