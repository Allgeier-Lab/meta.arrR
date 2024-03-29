#' plot.meta_syst
#'
#' @description
#' Plotting method for meta_syst object.
#'
#' @param x \code{meta_syst} object simulated with \code{setup_meta}.
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
plot.meta_syst <- function(x, ...) {

  # create data.frame with polygon coordinates
  poly_xy <- data.frame(x = c(-1, 1, 1, -1), y = c(-1, -1, 1, 1))

  # convert coordinates of ecosystems to data.frame
  local_xy <- as.data.frame(x$seafloor_xy)

  # calculate probabilities
  prob_dist <- calc_probability(metasyst = x, lambda = x$parameters$move_lambda,
                                full = FALSE, dist = TRUE)

  # add id col for reshaping
  local_prob <- data.frame(id_origin = 1:x$n, id_reach = prob_dist$probs)

  # reshape long
  local_prob <- stats::reshape(data = local_prob, direction = "long",
                               v.names = "probability", varying = list(names(local_prob[, -1])),
                               idvar = "id_origin", ids = local_prob[, 1], timevar = "id_reach",
                               times = 1:x$n, new.row.names = seq(from = 1, to = x$n ^ 2))

  # add distance vector
  local_prob$distance <- as.vector(prob_dist$dist)

  # use only complete cases
  local_prob <- local_prob[stats::complete.cases(local_prob), ]

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
    ggplot2::scale_colour_viridis_d(name = "Ecosystem", option = "A") +
    ggplot2::labs(x = "x coordinate", y = "y coordinate") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none", axis.title = ggplot2::element_text(),
                   axis.title.y = ggplot2::element_text(angle = 90))

  gg_raster <- ggplot2::ggplot(data = local_prob) +
    ggplot2::geom_raster(ggplot2::aes(x = factor(.data$id_origin), y = factor(.data$id_reach),
                                      fill = .data$probability)) +
    ggplot2::geom_text(ggplot2::aes(x = factor(.data$id_origin), y = factor(.data$id_reach),
                                    label = round(.data$probability, digits = 1))) +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_viridis_c(name = "Probability", option = "D") +
    ggplot2::labs(x = "Ecosystem origin", y = "Ecosystem reach") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  gg_function <- ggplot2::ggplot(data = local_prob) +
    ggplot2::geom_line(ggplot2::aes(x = .data$distance, y = .data$probability)) +
    ggplot2::geom_point(ggplot2::aes(x = .data$distance, y = .data$probability),
                        shape = 1, size = 2) +
    ggplot2::scale_colour_viridis_d(name = "Ecosystem", option = "A") +
    # ggplot2::scale_x_continuous(limits = c(0, max(prob_dist$dist))) + # 2.828427m is diagonal dist
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(x = "Distance", y = "Probability") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "none")

  # combine to one grid

  gg_all <- cowplot::plot_grid(gg_map, gg_raster, ncol = 2)

  gg_all <- cowplot::plot_grid(gg_all, gg_function, nrow = 2)

  # suppressWarnings(gg_all <- cowplot::plot_grid(gg_map, gg_raster, gg_function, ncol = 3))

  return(gg_all)

}
