#' Helper function to for flat violin geoms
#'
#' Code from David Robinson
#' https://gist.github.com/dgrtwo/eb7750e74997891d7c20
#' @param
#' @keywords flat; violin
#' @export
#' @examples
#'


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomFlatViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, ...)
  )
}
