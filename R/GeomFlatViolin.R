#' Helper function to plot half-violins in ggplots
#'
#' Code from David Robinson
#' https://gist.github.com/dgrtwo/eb7750e74997891d7c20
#' @param
#' @keywords flat; violin
#' @export
#' @examples
#'




GeomFlatViolin <- ggplot2::ggproto("GeomFlatViolin", ggplot2::Geom,
                                   setup_data = function(data, params) {
                                     data$width <- data$width %||%
                                       params$width %||% (resolution(data$x, FALSE) * 0.9)
                                     # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
                                     data %>%
                                       dplyr::group_by(group) %>%
                                       dplyr::mutate(ymin = min(y), ymax = max(y), xmin = x, xmax = x + width / 2)
                                   },

                                   draw_group = function(data, panel_scales, coord) {
                                     # Find the points for the line to go all the way around
                                     data <- transform(data, xminv = x,
                                                       xmaxv = x + violinwidth * (xmax - x))
                                     # Make sure it's sorted properly to draw the outline
                                     newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                                                      plyr::arrange(transform(data, x = xmaxv), -y))
                                     # Close the polygon: set first and last point the same
                                     # Needed for coord_polar and such
                                     newdata <- rbind(newdata, newdata[1,])
                                     ggplot2:::ggname("geom_flat_violin", ggplot2::GeomPolygon$draw_panel(newdata, panel_scales, coord))
                                   },
                                   draw_key = ggplot2::draw_key_polygon,
                                   default_aes = ggplot2::aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                                     alpha = NA, linetype = "solid"),
                                   required_aes = c("x", "y")
)
