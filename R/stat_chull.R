#' Draw convex hull for a set of points on a ggplot
#'
#' @description
#' Draws a (convex) polygon around the outermost points of a set of points.
#' Useful as a visual aid for identifying groups of points on a scatterplot,
#' such as an ordination plot.
#'
#' @details
#' This is a ggplot2 extension - slightly modified from the original code found here:
#'
#' \url{https://CRAN.r-project.org/package=ggplot2/vignettes/extending-ggplot2.html}
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_ellipse
#' @seealso \code{ggplot2::\link[ggplot2]{stat_ellipse}}
#' @seealso \code{\link{ord_plot}}
#' @export
#' @examples
#' library(ggplot2)
#' microViz::ibd %>%
#'   tax_fix() %>%
#'   tax_transform(rank = "Genus", trans = "clr") %>%
#'   ord_calc(method = "PCA") %>%
#'   ord_plot(colour = "DiseaseState", shape = "DiseaseState", alpha = 0.5) +
#'   stat_chull(aes(colour = DiseaseState))
#'
#' microViz::ibd %>%
#'   tax_fix() %>%
#'   tax_transform(rank = "Genus", trans = "clr") %>%
#'   ord_calc(method = "PCA") %>%
#'   ord_plot(colour = "DiseaseState", shape = "DiseaseState", alpha = 0.5) +
#'   stat_chull(aes(colour = DiseaseState, fill = DiseaseState), alpha = 0.1)
stat_chull <- function(mapping = NULL, data = NULL, geom = "polygonHollow",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

polygonHollowDefaultAes <- function() {
  def <- list(colour = "black", fill = NA, linetype = 1, alpha = NA)
  oldGG <- isTRUE(utils::packageVersion("ggplot2") < "3.4.0")
  if (oldGG) def[["size"]] <- 0.5 else def[["linewidth"]] <- 0.5
  do.call(ggplot2::aes, args = def)
}

GeomPolygonHollow <- ggplot2::ggproto(
  `_class` = "GeomPolygonHollow", `_inherit` = ggplot2::GeomPolygon,
  default_aes = polygonHollowDefaultAes()
)

StatChull <- ggplot2::ggproto(
  `_class` = "StatChull", `_inherit` = ggplot2::Stat,
  compute_group = function(data, scales) {
    data[chull(data$x, data$y), , drop = FALSE]
  },
  required_aes = c("x", "y")
)
