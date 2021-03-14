# ps_extra S3 class
# S3 class to store a list of "extras" alongside a phyloseq object
#' @examples
#' library(phyloseq)
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#' ps <- dietswap
#'
#' new_ps_extra_info()
#' print(new_ps_extra_info(), all = TRUE)
#' ps_extra <- new_ps_extra(ps)
#' ps_extra
#' @export
#' @noRd
print.ps_extra <- function(x, ...) {
  cat("ps_extra object - a list with phyloseq and extras:\n\n")
  print(x[["ps"]])
  i <- x[["info"]]
  cat("\n")
  print(i)
  d <- x[["dist"]]
  if (!identical(d, NULL)) {
    d_size <- attr(d, "Size")
    cat("\n\n")
    cat(i[["distMethod"]], "distance matrix of size", d_size, "\n")
    cat(d[1:min(5, d_size)], "...")
  }
  o <- x[["ord"]]
  if (!identical(o, NULL)) {
    cat("\n\nordination of class:", class(o), "\n")
    if (!identical(o[["call"]], NULL)) print(o[["call"]])
    if (!identical(i[["constraints"]], NA_character_)) cat("constraints:", i[["constraints"]])
    if (!identical(i[["conditions"]], NA_character_)) cat("\nconditions:", i[["conditions"]])
  }
  p <- x[["permanova"]]
  if (!identical(p, NULL)) {
    cat("\n\npermanova:\n")
    print(p)
  }
  b <- x[["bdisp"]]
  if (!identical(b, NULL)) {
    cat("\n\nbetadisper:\n")
    cat(names(b))
  }
  cat("\n")
}

#' @export
#' @noRd
print.ps_extra_info <- function(x, ..., all = FALSE) {
  cat("phyloseq info:\n")
  if (isFALSE(all)){
    out <- paste("tax_agg =", x[["tax_agg"]], "tax_transform =", x[["tax_transform"]])
    if (!identical(x[["tax_scale"]], NA_character_)) out <- paste(out, paste("tax_scale =", x[["tax_scale"]]))
    cat(out)
  } else if (isTRUE(all)){
    for (i in names(x)) cat(i, "=", x[[i]], "\n")
  } else {
    stop("all must be TRUE or FALSE, it is: ", all)
  }
}

#' @param ps phyloseq object
#' @param dist dist class distance matrix
#' @param ord ordination object
#' @param info info about the other 3 args
#' @noRd
new_ps_extra <- function(
                         ps,
                         dist = NULL,
                         ord = NULL,
                         info = new_ps_extra_info()) {
  stopifnot(methods::is(ps, "phyloseq"))
  stopifnot(identical(dist, NULL) && !inherits(dist, "dist"))
  stopifnot(inherits(info, "ps_extra_info"))
  structure(list(ps = ps, dist = dist, ord = ord, info = info), class = c("ps_extra", "list"))
}

#' @param tax_agg aggregation level from tax_agg() (if any)
#' @param tax_transform transformation name from tax_transform() (if any)
#' @param tax_scale scaling specified in tax_scale() if any
#' @param distMethod distance method in dist_calc()
#' @param ordMethod ordination method in ord_calc()
#' @param constraints constraints (if any) for ord_calc()
#' @param conditions conditions (if any) for ord_calc()
#' @noRd
new_ps_extra_info <- function(
                              tax_agg = NA_character_,
                              tax_transform = NA_character_,
                              tax_scale = NA_character_,
                              distMethod = NA_character_,
                              ordMethod = NA_character_,
                              constraints = NA_character_,
                              conditions = NA_character_) {
  info <- c(
    tax_agg = tax_agg, tax_transform = tax_transform,
    tax_scale = tax_scale,
    distMethod = distMethod,
    ordMethod = ordMethod, constraints = constraints, conditions = conditions
  )
  stopifnot(all(is.character(info)))

  structure(.Data = info, class = "ps_extra_info")
}
