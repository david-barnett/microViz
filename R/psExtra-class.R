# psExtraOrdInfo -------------------------------------------------------------------

#' Constructor for info list class used in ord_info element of psExtraInfo
#'
#' @param method character string naming ordination method
#' @param constraints character vector naming any constraint variables
#' @param conditions character vector naming any condition variables
#'
#' @return psExtraOrdInfo S3 class list
#' @keywords internal
new_psExtraOrdInfo <- function(
    method = character(), constraints = character(), conditions = character()
) {
  structure(class = "psExtraOrdInfo", .Data = list(
    method = method, constraints = constraints, conditions = conditions
  ))
}

setOldClass("psExtraOrdInfo")

validate_psExtraOrdInfo <- function(psExtraOrdInfo) {
  stopifnot(is.list(psExtraOrdInfo))
  # check valid classes of list entries
  for (i in c("method", "constraints", "conditions")) {
    if (!is.character(psExtraOrdInfo[[i]])) {
      stop(i, " psExtraOrdInfo entry is not character")
    }
  }
}


# setClass(
#   Class = "ordInfo",
#   slots = c(
#     method = "character", constraints = "character", conditions = "character"
#   ),
#   prototype = list(
#     method = character(), constraints = character(), conditions = character()
#   )
# )


print.psExtraOrdInfo <- function(psExtraOrdInfo) {
  cat("Ordination info:\n")
  for (N in names(psExtraOrdInfo)) {
    v <- psExtraOrdInfo[[N]]
    if (length(v) > 0) cat(N, " = '", paste(v, collapse = "', '"), "'\t", sep = "")
  }
  cat("\n")
}
#
# setMethod("show", "ordInfo", function(object) {
#   cat("ordination info:\n")
#   for (s in slotNames(object)) {
#     x <- slot(object, s)
#     if (length(x) > 0) cat(s, "=", paste(x, collapse = ", "), "\t")
#   }
#   cat("\n")
# })

# psExtraInfo ----------------------------------------------------------------

# #' Info list S4 class used in info slot of psExtra
# #'
# #' @slot tax_agg character.
# #' @slot tax_trans character.
# #' @slot tax_scale character.
# #' @slot dist_method character.
# #' @slot ord_info ordInfo
# #'
# #' @export
# setClass(
#   Class = "psExtraInfo",
#   slots = c(
#     tax_agg = "character",
#     tax_trans = "character",
#     tax_scale = "character", # TODO should probably remove this and improve logging of chained transformations as length N tax_trans vector (with a printing/collapsing method for e.g. ord plot captions) # deprecate tax_scale in favour of tax_trans trans = "scale" and trans = "center"
#     dist_method = "character",
#     ord_info = "ordInfo"
#   ),
#   prototype = list(
#     tax_agg = character(),
#     tax_trans = character(),
#     tax_scale = character(),
#     dist_method = character(),
#     ord_info = new("ordInfo")
#   )
# )

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
                            ord_info = new_psExtraOrdInfo()
) {
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

setOldClass("psExtraInfo")

validate_psExtraInfo <- function(psExtraInfo) {
  stopifnot(is.list(psExtraInfo))
  # check valid classes of list entries
  for (i in c("tax_agg", "tax_trans", "tax_scale", "dist_method")) {
    if (!is.character(psExtraInfo[[i]])) {
      stop(i, " psExtraInfo entry is not character")
    }
  }
  # validate_psExtraOrdInfo(psExtraInfo[["ord_info"]])
}

update_psExtraInfo <- function(psExtraInfo, ..., append = FALSE) {
  new <- list(...)
  stopifnot(rlang::is_named(new))
  stopifnot(
    !all(names(new) %in% c("tax_agg", "tax_trans", "tax_scale", "dist_method", "ord_info"))
  )
  for (n in names(new)) {
    psExtraInfo[n] <- if (append) c(psExtraInfo[n], new[n]) else new[n]
  }
  validate_psExtraInfo(psExtraInfo) # TODO remove once S4 transition completed?
  return(psExtraInfo)
}

print.psExtraInfo <- function(psExtraInfo,
                              which = c("tax_agg", "tax_trans", "tax_scale", "dist_method", "ord_info")
                              ) {
  which <- rlang::arg_match(which, multiple = TRUE)
  cat("psExtra info:\n")
  vectorElementNames <- setdiff(which, "ord_info") # vector slots
  for (N in vectorElementNames) {
    v <- psExtraInfo[[N]]
    if (length(v) > 0) cat(N, " = '", paste(v, collapse = "', '"), "'\t", sep = "")
  }
  cat("\n")
  print(psExtraInfo[['ord_info']])
}

#
# setMethod("show", "psExtraInfo", function(object) {
#   cat("psExtra info:\n")
#   v <- setdiff(slotNames(object), "ord_info") # vector slots
#   for (s in v) {
#     x <- slot(object, s)
#     if (length(x) > 0) cat(s, "=", paste(x, collapse = ", "), "\t")
#   }
#   cat("\n")
#   if (!identical(object@ord_info, new("ordInfo"))) print(object@ord_info)
# })


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

setMethod("show", "psExtra", function(object) {
  x <- object
  cat("psExtra object - a phyloseq object with extra slots:\n\n")
  show(as(x, "phyloseq"))
  cat("\n")
  # cat("\n- extra slots:\n")
  i <- x@info
  print.psExtraInfo(i, which = c("tax_agg", "tax_trans", "tax_scale"))

  d <- x@dist
  if (!identical(d, NULL)) {
    d_size <- attr(d, "Size")
    cat(i[["dist_method"]], "distance matrix of size", d_size, "\n")
    cat(d[1:min(5, d_size)], "...")
  }
  # print ordination class and call and constraints + conditions if present
  o <- x@ord
  if (!identical(o, NULL)) {
    cat("\n\nordination of class:", class(o), "\n")
    if (!identical(o[["call"]], NULL)) print(o[["call"]])
    print(i[['ord_info']])
  }
  # check for and shortly print other possible elements' info
  counts <- x@counts
  if (!identical(counts, NULL)) {
    cat("\n\n$counts OTU Table:")
    cat(
      " [", phyloseq::ntaxa(counts), "taxa and",
      phyloseq::nsamples(counts), "samples ]"
    )
  }
  # print permanova if present
  p <- x@permanova
  if (!identical(p, NULL)) {
    cat("\n\npermanova:\n")
    print(p)
  }
  # print dist_bdisp names if present
  b <- x@bdisp
  if (!identical(b, NULL)) {
    cat("\n\nbetadisper:\n")
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
