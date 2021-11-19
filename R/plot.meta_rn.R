#' plot.meta_rn
#'
#' @description
#' Plotting method for meta_rn object.
#'
#' @param x \code{meta_rn} object simulated with \code{run_meta}.
#' @param what Character specifying what to plot.
#' @param summarize Logical if TRUE values over time steps are plotted.
#' @param fill Character specifying which column to use for plotting.
#' @param gamma Logical if TRUE gamma input line is added.
#' @param limits Vector with minium and maximum value of \code{fill} values.
#' @param burn_in If TRUE, line to indicate burn-in time is plotted.
#' @param viridis_option Character with \code{viridis} color palette.
#' @param base_size Numeric to specify base font size.
#' @param ... Not used.
#'
#' @details
#' Plotting method for result of metaecosystem model run created
#' with \code{run_meta}.
#'
#' @examples
#' \dontrun{
#' plot(result_rand)
#' }
#'
#' @aliases plot.meta_rn
#' @rdname plot.meta_rn
#'
#' @export
plot.meta_rn <- function(x, what = "seafloor", summarize = FALSE, fill = "ag_biomass", gamma = FALSE,
                         limits = NULL, burn_in = FALSE, base_size = 10, viridis_option = "C", ...) {

  if (!what %in% c("seafloor", "fishpop")) {

    stop("Please select either what = 'seafloor' or what = 'fishpop'",
         call. = FALSE)

  }

  # plot value over time steps
  if (summarize) {

    # set color for burn in threshold
    col_burn <- ifelse(test = burn_in, yes = "grey", no = NA)

    # get burn_in value for filtering
    burn_in_itr <- ifelse(test = burn_in, yes = x$burn_in,
                          no = min(x$seafloor[[1]]$timestep))

    # setup color scale
    if (gamma) {

      col_viridis <- "black"

    } else {

      col_viridis <- viridis::viridis(n = x$n, option = viridis_option)

    }

    # get data depending on what argument
    if (what == "seafloor") {

      # get list with values
      input_temp <- x$seafloor

      # name of columns
      cols_temp <- c("ag_biomass", "bg_biomass",
                     "nutrients_pool", "detritus_pool")

      # setup labels
      y_labels <- c("Dry weight bg biomass [g/cell]", "Dry weight ag biomass [g/cell]",
                    "Nutrients pool [g/cell]", "Detritus pool [g/cell]")

    } else if (what == "fishpop") {

      # get list with values
      input_temp <- x$fishpop

      # name of columns
      cols_temp <- c("length", "weight",
                     "died_consumption", "died_background")

      # setup labels
      y_labels <- c("Body length [cm]", "Body weigth [g]",
                    "Count mortality consumption [#]", "Count mortality background [#]")

    }

    # aggregate by time step
    result_aggr <- lapply(seq_along(input_temp), function(i) {

      cbind(meta = i, stats::aggregate(x = input_temp[[i]][, cols_temp],
                                       by = list(timestep = input_temp[[i]]$timestep),
                                       FUN = "mean", na.rm = TRUE))

    })

    # combine to one data.frame
    result_aggr <- do.call(rbind, result_aggr)

    # calculate total sum of values
    if (gamma) {

      # calculate sum for each timestep
      result_aggr <- stats::aggregate(x = result_aggr[, c(3:6)],
                                      by = list(timestep = result_aggr$timestep),
                                      FUN = "sum", na.rm = TRUE)

      # add id col for plotting
      result_aggr <- cbind(meta = "Gamma", result_aggr)

    } else {

      # better naming for plotting
      result_aggr$meta <- paste0("Meta_", result_aggr$meta)

    }

    # MH: check if limits are !is.null() and rename to top_left,...

    # setup names of list
    names(result_aggr) <- c("meta", "timestep", "top_right", "top_left",
                           "bottom_left", "bottom_right")

    # create plot
    gg_top_left <- ggplot2::ggplot(data = result_aggr) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = top_left, col = factor(meta))) +
      ggplot2::scale_color_manual(name = "", values = col_viridis) +
      ggplot2::scale_linetype_manual(name = "Scale", values = c("Local" = 2, "Regional" = 1)) +
      ggplot2::labs(x = "Timestep", y = y_labels[1]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                     legend.position = "bottom")

    # create plot
    gg_top_right <- ggplot2::ggplot(data = result_aggr) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = top_right, col = factor(meta))) +
      ggplot2::scale_color_manual(name = "", values = col_viridis) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = y_labels[2]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # create plot
    gg_bottom_left <- ggplot2::ggplot(data = result_aggr) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = bottom_left, col = factor(meta))) +
      ggplot2::scale_color_manual(name = "", values = col_viridis) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = y_labels[3]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    # create plot
    gg_bottom_right <- ggplot2::ggplot(data = result_aggr) +
      ggplot2::geom_vline(xintercept = burn_in_itr, col = col_burn, linetype = 3) +
      ggplot2::geom_line(ggplot2::aes(x = timestep, y = bottom_right, col = factor(meta))) +
      ggplot2::scale_color_manual(name = "", values = col_viridis) +
      ggplot2::guides(col = "none", linetype = "none") +
      ggplot2::labs(x = "Timestep", y = y_labels[4]) +
      ggplot2::theme_classic(base_size = base_size) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))

    legend_top_left <- cowplot::get_legend(gg_top_left)

    # create title
    plot_title <- paste0("Total time        : ", x$max_i, " iterations [",
                         round(x$max_i * x$min_per_i / 60 / 24, 1), " days]",
                         "\nFishpop (total) : ", sum(x$starting_values$pop_n),
                         " indiv [Movement : ", x$movement, "]")

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
    gg_all <- cowplot::plot_grid(title, gg_all, legend_top_left, ncol = 1,
                                 rel_heights = c(0.1, 0.8, 0.1))

  } else {

    if (what == "seafloor") {

      max_i <- x$max_i

      # get data.frame with all seafloor values of selected timestep
      seafloor <- do.call(rbind, lapply(seq_along(x$seafloor), function(j) {

        id <- paste0("Metaecosystem ", j)

        cbind(subset(x$seafloor[[j]], timestep == max_i,
                     select = c("x", "y", fill)), id)

      }))

      # rename columns for plotting
      names(seafloor) <- c("x", "y", "fill", "id")

      # create title
      title <- paste0("Timestep         : ", x$max_i, " iterations [",
                      round(x$max_i * x$min_per_i / 60 / 24, 1), " days]",
                      "\nFishpop (total) : ", sum(x$starting_values$pop_n),
                      " indiv [Movement : ", x$movement, "]")

      # create ggplot
      gg_all <- ggplot2::ggplot(data = seafloor) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = fill)) +
        ggplot2::facet_wrap(. ~ id, nrow = 1) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      na.value = "#9B964A", limits = limits,
                                      name = fill) +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "", title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size),
                       legend.position = "bottom", legend.key.width = ggplot2::unit(10, 'mm'))

    } else if (what == "fishpop") {

      # get density within each cell
      densities <- get_meta_densities(result = x)

      # get number of reefs
      no_reefs <- vapply(x$coords_reef, nrow, FUN.VALUE = numeric(1))

      # get coords of reefs
      coords_reef <- data.frame(do.call(rbind, x$coords_reef))

      # add metaecosystem id
      coords_reef$id <- rep(x = 1:x$n, times = no_reefs)

      # rename
      names(coords_reef) <- c("cell", "x", "y", "id")

      # create title
      title <- paste0("Timestep         : ", x$max_i, " iterations [",
                      round(x$max_i * x$min_per_i / 60 / 24, 1), " days]",
                      "\nFishpop (total) : ", sum(x$starting_values$pop_n),
                      " indiv [Movement : ", x$movement, "]")

      gg_all <- ggplot2::ggplot(data = densities) +
        ggplot2::geom_raster(ggplot2::aes(x = x, y = y, fill = density)) +
        ggplot2::geom_raster(data = coords_reef, ggplot2::aes(x = x, y = y),
                             fill = "#9B964A") +
        ggplot2::facet_wrap(. ~ id) +
        ggplot2::scale_fill_gradientn(colours = c("#368AC0", "#F4B5BD", "#EC747F"),
                                      name = "Density") +
        ggplot2::coord_equal() +
        ggplot2::labs(x = "", y = "", title = title) +
        ggplot2::theme_classic(base_size = base_size) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = base_size))
    }
  }

  return(gg_all)

}
