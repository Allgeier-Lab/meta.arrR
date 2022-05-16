#' plot_fish_attr
#'
#' @description
#' Plot fish attributes
#'
#' @param x \code{meta_syst} object simulated with \code{setup_meta}.
#'
#' @details
#' Plots the distribution of \code{reserves_thres} and \code{move_prob}.
#'
#' @return ggplot
#'
#' @examples
#' \dontrun{
#' plot_fish_attr(x = metasyst)
#' }
#'
#' @aliases plot_fish_attr
#' @rdname plot_fish_attr
#'
#' @importFrom rlang .data
#'
#' @export
plot_fish_attr <- function(x) {

  fish_attr <- as.data.frame(x$fishpop_attr)

  fish_attr <- stats::reshape(data = fish_attr, direction = "long", v.names = "value", ids = "id",
                              varying = c("reserves_thres", "move_prob"),
                              timevar = "name", times = c("reserves_thres", "move_prob"),
                              new.row.names = 1:(nrow(fish_attr) * 2))

  # create plot
  gg_attr <- ggplot2::ggplot(data = fish_attr, ggplot2::aes(x = .data$value, y = ..density..)) +
    # ggplot2::geom_density() +
    ggplot2::geom_histogram(binwidth = 0.05, alpha = 0.25, color = "black") +
    ggplot2::facet_wrap(. ~ factor(name, labels = c("Meta move probability", "Reserves threshold"))) +
    ggplot2::labs(x = "Value", y = "Density individuals") +
    ggplot2::scale_x_continuous(limits = c(-0.05, 1.05)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")

  return(gg_attr)

}
