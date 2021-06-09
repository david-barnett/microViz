# helper functions for adding arrows/segments/vectors to ordination plots

#' @title create ordination plot vector styling lists
#'
#' @param size width of vector
#' @param alpha opacity of vector
#' @param colour colour of vector
#' @param arrow arrow style specified with grid::arrow() or NULL for no arrow
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param ... further arguments passed to geom_segment
#'
#' @return list

## vector style helpers -------------------------------------------------------

#' @export
#' @rdname ord_plotArrows
vec_constraint <- function(
  size = 1, alpha = 0.8, colour = "brown",
  arrow = grid::arrow(
    length = grid::unit(0.005, units = "npc"), type = "closed", angle = 30
  ),
  lineend = "round", linejoin = "mitre",
  ...
){
  list(
    size = size, alpha = alpha, arrow = arrow, colour = colour,
    lineend = lineend, linejoin = linejoin, ...
  )
}

#' @export
#' @rdname ord_plotArrows
vec_tax_sel <- function(
  size = 0.5, alpha = 1, colour = "black",
  arrow = grid::arrow(
    length = grid::unit(0.005, units = "npc"), type = "closed", angle = 30
  ),
  lineend = "round", linejoin = "mitre",
  ...
){
  list(
    size = size, alpha = alpha, arrow = arrow, colour = colour,
    lineend = lineend, linejoin = linejoin, ...
  )
}

#' @export
#' @rdname ord_plotArrows
vec_tax_all <- function(size = 0.5, alpha = 0.25, arrow = NULL, ...
){
  list(size = size, alpha = alpha, arrow = arrow, ...)
}




