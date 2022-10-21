# psExtraOrdInfo -------------------------------------------------------------------

#' Constructor for info list class used in ord_info element of psExtraInfo
#'
#' @param method character string naming ordination method
#' @param constraints character vector naming any constraint variables
#' @param conditions character vector naming any condition variables
#'
#' @return psExtraOrdInfo S3 class list
#' @keywords internal
new_psExtraOrdInfo <- function(method = character(), constraints = character(), conditions = character()) {
  structure(class = "psExtraOrdInfo", .Data = list(
    method = method, constraints = constraints, conditions = conditions
  ))
}


validate_psExtraOrdInfo <- function(psExtraOrdInfo) {
  stopifnot(is.list(psExtraOrdInfo))
  # check valid classes of list entries
  for (i in c("method", "constraints", "conditions")) {
    if (!is.character(psExtraOrdInfo[[i]])) {
      stop(i, " psExtraOrdInfo entry is not character")
    }
  }
}

#' Print method for psExtraOrdInfo list
#'
#' @export
print.psExtraOrdInfo <- function(psExtraOrdInfo) {
  lens <- purrr::map(psExtraOrdInfo, length)
  if (any(lens > 0)) {
    cat("Ordination info:\n")
    for (N in names(psExtraOrdInfo)) {
      v <- psExtraOrdInfo[[N]]
      if (length(v) > 0) {
        cat(N, " = '", paste(v, collapse = "', '"), "'\t", sep = "")
      }
    }
    cat("\n")
  }
}

setOldClass("psExtraOrdInfo")
setMethod("show", "psExtraOrdInfo", function(object) print.psExtraOrdInfo(object))

# psExtraInfo ----------------------------------------------------------------

#' Constructor for psExtraInfo list objects (internal use)
#'
#' @param tax_agg
#' character string naming rank at which psExtra was aggregated
#' @param tax_trans
#' character vector naming transformation(s) applied to taxa in psExtra
#' @param tax_scale character vector
#' @param dist_method character vector
#' @param ord_info psExtraOrdInfo list
#'
#' @return psExtraInfo S3 class list
#' @keywords internal
new_psExtraInfo <- function(tax_agg = character(),
                            tax_trans = character(),
                            tax_scale = character(),
                            dist_method = character(),
                            ord_info = new_psExtraOrdInfo()) {
  out <- structure(class = "psExtraInfo", .Data = list(
    tax_agg = tax_agg,
    tax_trans = tax_trans,
    tax_scale = tax_scale, # TODO should probably remove this and improve logging of chained transformations as length N tax_trans vector (with a printing/collapsing method for e.g. ord plot captions) # deprecate tax_scale in favour of tax_trans trans = "scale" and trans = "center"
    dist_method = dist_method,
    ord_info = ord_info
  ))
  validate_psExtraInfo(out)
  return(out)
}

validate_psExtraInfo <- function(psExtraInfo) {
  stopifnot(is.list(psExtraInfo))
  # check valid classes of list entries
  for (i in c("tax_agg", "tax_trans", "tax_scale", "dist_method")) {
    if (!is.character(psExtraInfo[[i]])) {
      stop(i, " psExtraInfo entry is not character")
    }
  }
  validate_psExtraOrdInfo(psExtraInfo[["ord_info"]])
}

update_psExtraInfo <- function(psExtraInfo, ..., append = FALSE) {
  new <- list(...)
  stopifnot(rlang::is_named(new))
  stopifnot(
    all(names(new) %in% c("tax_agg", "tax_trans", "tax_scale", "dist_method", "ord_info"))
  )
  for (n in names(new)) {
    psExtraInfo[n] <- if (append) c(psExtraInfo[n], new[n]) else new[n]
  }
  validate_psExtraInfo(psExtraInfo) # TODO remove once S4 transition completed?
  return(psExtraInfo)
}

#' Print method for psExtraInfo object
#'
#' @param psExtraInfo psExtraInfo object
#' @param which which elements of psExtraInfo list to print
#'
#' @export
print.psExtraInfo <- function(psExtraInfo,
                              which = c("tax_agg", "tax_trans", "tax_scale", "dist_method", "ord_info")) {
  which <- rlang::arg_match(which, multiple = TRUE)
  vectorElementNames <- setdiff(which, "ord_info") # vector slots
  lens <- purrr::map(psExtraInfo[vectorElementNames], length)
  if (any(lens > 0)) {
    cat("psExtra info:\n")
    for (N in vectorElementNames) {
      v <- psExtraInfo[[N]]
      if (length(v) > 0) cat(N, " = '", paste(v, collapse = "', '"), "'\t", sep = "")
    }
    cat("\n")
  }
  if ("ord_info" %in% which) print(psExtraInfo[["ord_info"]])
}

setOldClass("psExtraInfo")
setMethod("show", "psExtraInfo", function(object) print.psExtraInfo(object))


# psExtra -------------------------------------------------------------------
#' Define psExtra class S4 object
#'
#' @slot info list.
#' @slot counts otu_table.
#' @slot dist dist.
#' @slot ord ANY.
#' @slot permanova ANY.
#' @slot bdisp ANY.
#' @slot taxatree_models list.
#' @slot taxatree_stats data.frame.
#' @slot tax_models list.
#' @slot tax_stats data.frame.
#'
#' @importClassesFrom phyloseq phyloseq
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("shao19")
#'
#' ps <- shao19 %>% ps_filter(infant_age == 12)
#' ps %>% tax_agg("genus")
setClass(
  Class = "psExtra",
  contains = "phyloseq",
  slots = c(
    info = "psExtraInfo",
    counts = "otu_tableOrNULL",
    dist = "ANY",
    ord = "ANY",
    permanova = "ANY",
    bdisp = "ANY",
    taxatree_models = "ANY",
    taxatree_stats = "ANY",
    tax_models = "ANY",
    tax_stats = "ANY"
  ),
  prototype = list(
    info = new_psExtraInfo(),
    counts = NULL,
    dist = NULL,
    ord = NULL,
    permanova = NULL,
    bdisp = NULL,
    taxatree_models = NULL,
    taxatree_stats = NULL,
    tax_models = NULL,
    tax_stats = NULL
  )
)

psExtra <- function(ps,
                    info,
                    counts = NULL,
                    dist = NULL,
                    ord = NULL,
                    permanova = NULL,
                    bdisp = NULL,
                    taxatree_models = NULL,
                    taxatree_stats = NULL,
                    tax_models = NULL,
                    tax_stats = NULL) {
  if (!methods::is(ps, "phyloseq")) stop("ps must be a phyloseq object")
  new(
    Class = "psExtra",
    otu_table = ps@otu_table, tax_table = ps@tax_table, sam_data = ps@sam_data,
    phy_tree = ps@phy_tree, refseq = ps@refseq,
    info = info, counts = counts, dist = dist, ord = ord,
    permanova = permanova, bdisp = bdisp,
    taxatree_models = taxatree_models, taxatree_stats = taxatree_stats,
    tax_models = tax_models, tax_stats = tax_stats
  )
}

update_psExtra <- function(psExtra, ...) {
  check_is_psExtra(psExtra, argName = "psExtra")
  new <- list(...)
  stopifnot(rlang::is_named(new))
  # split up phyloseq slots
  if ('ps' %in% names(new)) {
    for (s in slotNames(new[['ps']])) new[[s]] <- slot(new[["ps"]], s)
    new[["ps"]] <- NULL
  }
  stopifnot(all(names(new) %in% slotNames(psExtra)))
  for (n in names(new)) slot(psExtra, n) <- new[[n]]
  return(psExtra)
}

## psExtra 'show' method --------
setMethod("show", "psExtra", function(object) {
  x <- object
  cat("psExtra object - a phyloseq object with extra slots:\n\n")
  show(as(x, "phyloseq"))
  cat("\n")

  counts <- x@counts
  if (!identical(counts, NULL)) {
    cat("otu_get(counts = TRUE)\t\t")
    cat(
      " [", phyloseq::ntaxa(counts), "taxa and",
      phyloseq::nsamples(counts), "samples ]\n"
    )
    cat("\n")
  }
  # cat("\n- extra slots:\n")
  i <- x@info
  print.psExtraInfo(i, which = c("tax_agg", "tax_trans", "tax_scale"))

  # check for and shortly print other possible elements' info
  d <- x@dist
  if (!identical(d, NULL)) {
    cat("\n")
    d_size <- attr(d, "Size")
    cat(i[["dist_method"]], "distance matrix of size", d_size, "\n")
    cat(d[1:min(5, d_size)], "...\n")
  }
  # print ordination class and call and constraints + conditions if present
  o <- x@ord
  if (!identical(o, NULL)) {
    cat("\nordination of class:", class(o), "\n")
    if (!identical(o[["call"]], NULL)) print(o[["call"]])
    print(i[["ord_info"]])
  }
  # print permanova if present
  p <- x@permanova
  if (!identical(p, NULL)) {
    cat("\npermanova:\n")
    print(p)
  }
  # print dist_bdisp names if present
  b <- x@bdisp
  if (!identical(b, NULL)) {
    cat("\nbetadisper:\n")
    cat(names(b))
  }
  # print info about taxatree_models list if present
  if (!identical(x@taxatree_models, NULL)) {
    cat("\n\n$taxatree_models list:\n")
    cat("Ranks:", paste(names(x@taxatree_models), collapse = "/"))
  }
  # print info about taxatree_stats list if present
  if (!identical(x@taxatree_stats, NULL)) {
    cat("\n\n$taxatree_stats dataframe:\n")
    taxatree_stats_summary(x@taxatree_stats)
  }
  # print info about tax_models list if present
  if (!identical(x@tax_models, NULL)) {
    cat("\n\n$tax_models list at rank: ", names(x@tax_models), "\n")
  }
  # print info about tax_stats list if present
  if (!identical(x@tax_stats, NULL)) {
    cat("\n\n$tax_stats dataframe:\n")
    tax_stats_summary(x@tax_stats)
  }
})

