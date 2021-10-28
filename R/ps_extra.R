#' ps_extra S3 class
#'
#' S3 class to store a list of "extras" alongside a phyloseq object
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
  print(i, all = FALSE)
  # print distance matrix info and sample if present
  d <- x[["dist"]]
  if (!identical(d, NULL)) {
    d_size <- attr(d, "Size")
    cat("\n\n")
    cat(i[["distMethod"]], "distance matrix of size", d_size, "\n")
    cat(d[1:min(5, d_size)], "...")
  }
  # print ordination class and call and constraints + conditions if present
  o <- x[["ord"]]
  if (!identical(o, NULL)) {
    cat("\n\nordination of class:", class(o), "\n")
    if (!identical(o[["call"]], NULL)) print(o[["call"]])
    if (!identical(i[["constraints"]], NA_character_)) {
      cat("constraints:", i[["constraints"]])
    }
    if (!identical(i[["conditions"]], NA_character_)) {
      cat("\nconditions:", i[["conditions"]])
    }
  }
  # check for and shortly print other possible elements' info
  counts <- x[["counts"]]
  if (!identical(counts, NULL)) {
    cat("\n\n$counts OTU Table:")
    cat(
      " [", phyloseq::ntaxa(counts), "taxa and",
      phyloseq::nsamples(counts), "samples ]"
    )
  }
  # print permanova if present
  p <- x[["permanova"]]
  if (!identical(p, NULL)) {
    cat("\n\npermanova:\n")
    print(p)
  }
  # print dist_bdisp names if present
  b <- x[["bdisp"]]
  if (!identical(b, NULL)) {
    cat("\n\nbetadisper:\n")
    cat(names(b))
  }
  # print info about taxatree_models list if present
  if (!identical(x[["taxatree_models"]], NULL)) {
    cat("\n\n$taxatree_models list:\n")
    cat("Ranks:", paste(names(x[["taxatree_models"]]), collapse = "/"))
  }
  # print info about taxatree_models list if present
  if (!identical(x[["taxatree_stats"]], NULL)) {
    cat("\n\n$taxatree_stats dataframe:\n")
    taxatree_stats_summary(x[["taxatree_stats"]])
  }
  cat("\n")
}

# helper for summarising taxatree_stats objects
taxatree_stats_summary <- function(df) {
  n <- length(unique(df[["taxon"]]))
  r <- unique(df[["rank"]])
  t <- levels(df[["term"]])
  cat(n, "taxa at", length(r), "ranks:", paste(r, collapse = ", "), "\n")
  cat(length(t), "terms:", paste(t, collapse = ", "))
}

#' @export
#' @noRd
print.ps_extra_info <- function(x, ..., all = TRUE) {
  cat("ps_extra info:\n")
  if (isFALSE(all)) {
    out <- paste(
      "tax_agg =", x[["tax_agg"]], "tax_transform =", x[["tax_transform"]]
    )
    if (!identical(x[["tax_scale"]], NA_character_)) {
      out <- paste(out, paste("tax_scale =", x[["tax_scale"]]))
    }
    cat(out)
  } else if (isTRUE(all)) {
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
new_ps_extra <- function(ps,
                         dist = NULL,
                         ord = NULL,
                         info = new_ps_extra_info()) {
  stopifnot(methods::is(ps, "phyloseq"))
  stopifnot(identical(dist, NULL) && !inherits(dist, "dist"))
  stopifnot(inherits(info, "ps_extra_info"))
  psx <- structure(
    .Data = list(ps = ps, dist = dist, ord = ord, info = info),
    class = c("ps_extra", "list")
  )
  return(psx)
}

#' @param tax_agg aggregation level from tax_agg() (if any)
#' @param tax_transform transformation name from tax_transform() (if any)
#' @param tax_scale scaling specified in tax_scale() if any
#' @param distMethod distance method in dist_calc()
#' @param ordMethod ordination method in ord_calc()
#' @param constraints constraints (if any) for ord_calc()
#' @param conditions conditions (if any) for ord_calc()
#' @noRd
new_ps_extra_info <- function(tax_agg = NA_character_,
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

  psxi <- structure(.Data = info, class = "ps_extra_info")
  return(psxi)
}

# internal helper to convert plain phyloseq to ps_extra or leave ps_extra as is
as_ps_extra <- function(ps) {
  if (methods::is(ps, "phyloseq")) {
    return(new_ps_extra(ps = ps))
  } else if (inherits(ps, "ps_extra")) {
    return(ps)
  } else {
    stop(
      "Cannot coerce object of class '", paste(class(ps), collapse = "' '"),
      "' into a ps_extra. \nObject must be a phyloseq or already a ps_extra!"
    )
  }
}
