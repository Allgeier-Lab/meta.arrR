#' plot.meta_syst
#'
#' @description
#' Plotting method for meta_syst object.
#'
#' @param x \code{meta_syst} object simulated with \code{setup_meta}.
#' @param lambda Distance decay parameter.
#' @param base_size Numeric to specify base font size.
#' @param viridis_option Character with \code{viridis} color palette.
#' @param ... Not used.
#'
#' @details
#' Plotting method for metaecosystem created with \code{setup_meta}.
#'
#' @return ggplot
#'
#' @examples
#' \dontrun{
#' plot(metasyst)
#' }
#'
#' @aliases plot.meta_syst
#' @rdname plot.meta_syst
#'
#' @importFrom rlang .data
#'
#' @export
plot.meta_syst <- function(x, lambda = 1, base_size = 10, viridis_option = "C", ...) {

  # create data.frame with polygon coordinates
  poly_xy <- data.frame(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

  # convert coordinates of ecosystems to data.frame
  local_xy <- as.data.frame(x$seafloor_xy)

  # calculate probabilities
  local_prob <- calc_probability(metasyst = x, lambda = lambda, full = FALSE)

  # add id col for reshaping
  local_prob <- data.frame(id_origin = 1:x$n, id_reach = local_prob)

  # reshape long
  local_prob <- stats::reshape(data = local_prob, direction = "long",
                               v.names = "probability", varying = list(names(local_prob[, -1])),
                               idvar = "id_origin", ids = local_prob[, 1], timevar = "id_reach",
                               times = 1:x$n, new.row.names = seq(from = 1, to = x$n ^ 2))

  # back-calculate distance
  local_prob$distance <- -log(local_prob$probability) / lambda

  # create color scale
  col_viridis <- viridis::viridis(n = x$n, option = viridis_option)

  gg_map <- ggplot2::ggplot(data = local_xy) +
    ggplot2::geom_polygon(data = poly_xy, ggplot2::aes(x = .data$x, y = .data$y),
                          fill = NA, col = "black") +
    ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, color = factor(.data$id)),
                        shape = 15, size = 5, alpha = 1/4) +
    ggplot2::geom_point(ggplot2::aes(x = .data$x, y = .data$y, color = factor(.data$id)),
                        shape = 0, size = 5) +
    ggplot2::geom_point(ggplot2::aes(x = 0.0, y = 0.0), shape = 3, col = "grey") +
    ggplot2::geom_text(ggplot2::aes(x = .data$x, y = .data$y, label = factor(.data$id)),
                       col = "black") +
    ggplot2::coord_equal() +
    ggplot2::scale_color_manual(name = "Ecosystem", values = col_viridis) +
    ggplot2::labs(x = "x coordinate", y = "y coordinate") +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom", axis.title = ggplot2::element_text(),
                   axis.title.y = ggplot2::element_text(angle = 90))

  gg_raster <- ggplot2::ggplot(data = local_prob) +
    ggplot2::geom_tile(ggplot2::aes(x = factor(.data$id_origin), y = factor(.data$id_reach),
                                    fill = .data$probability)) +
    # ggplot2::geom_tile(ggplot2::aes(x = factor(.data$id_local), y = factor(.data$id_source)),
    #                    fill = NA, colour = "black", size = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradientn(name = "Probability", limits = c(0, 1), breaks = c(0, 0.5, 1),
                         colors = viridis::viridis(n = 255, option = viridis_option),
                         na.value = "white") +
    ggplot2::labs(x = "Ecosystem origin", y = "Ecosystem reach") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")

  gg_function <- ggplot2::ggplot(data = local_prob) +
    ggplot2::geom_line(ggplot2::aes(x = .data$distance, y = .data$probability,
                                    col = factor(.data$id_origin)) ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$distance, y = .data$probability,
                                     col = factor(.data$id_origin)), shape = 1, size = 2) +
    ggplot2::scale_color_manual(name = "Ecoystem", values = col_viridis) +
    ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 3, by = 0.5), limits = c(0, 3)) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Distance", y = "Probability") +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(legend.position = "bottom")

  # combine to one grid

  suppressWarnings(gg_all <- cowplot::plot_grid(gg_map, gg_raster, gg_function, ncol = 3))

  return(gg_all)

}
