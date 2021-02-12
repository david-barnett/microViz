#' @name ps_extra-accessors
#' @title Extract elements from ps_extra class
#'
#' @param ps_extra ps_extra class object
#'
#' @return element of ps_extra class object
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("esophagus")
#'
#' @export
#' @rdname ps_extra-accessors
ps_get <- function(ps_extra){
  if (inherits(ps_extra, "ps_extra")){
    return(ps_extra[["ps"]])
  } else if (methods::is(ps_extra, "phyloseq")){
    return(ps_extra)
  } else {
    stop('class of argument should be "ps_extra" or "phyloseq", not: ', class(ps_extra))
  }
}
#' @rdname ps_extra-accessors
#' @export
dist_get <- function(ps_extra){
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["dist"]]
}
#' @rdname ps_extra-accessors
#' @export
ord_get <- function(ps_extra){
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["ord"]]
}
#' @rdname ps_extra-accessors
#' @export
info_get <- function(ps_extra){
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["info"]]
}
#' @rdname ps_extra-accessors
#' @export
perm_get <- function(ps_extra){
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["permanova"]]
}
#' @rdname ps_extra-accessors
#' @export
bdisp_get <- function(ps_extra){
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["bdisp"]]
}
