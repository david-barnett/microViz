#' Sort taxa in phyloseq otu_table and tax_table
#'
#' @description
#' Multiple ways of sorting taxa are possible and determined by the `by` argument.
#' The `by` argument must be one of:
#'  - 'rev' to reverse the current order
#'  - 'name' (sort alphabetically by `at`)
#'  - 'asis' to keep current order as is
#'  - a sample name (descending abundance sorting within that sample)
#'  - summary stat. function e.g. `sum` or `mean`
#'
#'  The `at` argument must be "names" for sorting unique taxa,
#'  or a rank name, for sorting at that rank.
#'  `at` is ignored when `by` is "rev".
#'
#' @details
#' Don't forget to pass `na.rm = TRUE` to `...`
#' if using a summary stat function in `by`
#'
#' @param data ps_extra or phyloseq
#' @param by how to sort, see description
#' @param at "names" or a taxonomic rank to apply sorting method to, as specified in `by`.
#' @param ...
#' used if summary function given, or pass `undetected` arg
#' for tax_transform("binary") if by = "prev" or "prevalence"
#' @param tree_warn
#' If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot be reordered.
#' Default behaviour of tax_sort is to remove the phylogenetic tree and warn about this.
#' tree_warn = FALSE will suppress the warning message, but still remove the tree!
#' @param verbose
#' passed to phyloseq_validate verbose
#' (if TRUE: message about suspicious values in tax_table, and how to fix)
#' @param trans
#' name of transformation to apply to taxa before sorting (taxa are returned un-transformed)
#' @param use_counts use count data if available, instead of transformed data
#'
#' @return sorted phyloseq or ps_extra
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("dietswap", package = "microbiome")
#' dietswap
#'
#' # reverse current order
#' dietswap %>%
#'   tax_sort("rev") %>%
#'   tax_table() %>%
#'   head(30)
#'
#' # sort alphabetically by a taxonomic rank (or "names" for taxa_names)
#' dietswap %>%
#'   tax_sort(by = "name", at = "Phylum") %>%
#'   tax_table() %>%
#'   head(30)
#'
#' # sequentially sorting by higher ranks
#' # sets tax_table in nested alphabetical order
#' dietswap %>%
#'   tax_sort(at = "names") %>%
#'   tax_sort(at = "Genus") %>%
#'   tax_sort(at = "Family") %>%
#'   tax_sort(at = "Phylum") %>%
#'   tax_table() %>%
#'   head(30)
#'
#' # sort by function e.g. total sum or median abundance
#' dietswap %>%
#'   tax_sort(by = sum) %>%
#'   taxa_names() %>%
#'   head(20)
#'
#' # transform to compositional data (proportions) before sorting
#' # note that abundances are returned untransformed
#' dietswap %>%
#'   tax_sort(by = sum, trans = "compositional") %>%
#'   taxa_names() %>%
#'   head(20)
#'
#' # order by descending abundance in a single named sample
#' dietswap %>%
#'   tax_sort(by = "Sample-1") %>%
#'   otu_table() %>%
#'   .[1:8, 1:4]
#'
#'
#' # sum order should always equal mean order if non-negative abundances
#' # don't forget to add na.rm = TRUE if you expect NAs in otu_table somehow
#' dietswap %>%
#'   tax_sort(by = sum, na.rm = TRUE) %>%
#'   taxa_names() %>%
#'   head(20)
#'
#' # if your phyloseq object has a phylogenetic tree,
#' # tax_sort will remove the tree, and warn you about this
#' # unless you disable that warning with tree_warn = FALSE
#'
#' # You can sort by abundance at higher taxonomic ranks,
#' # without losing lower rank info
#' # e.g. sort (descending) by phyla abundances
#' dietswap %>%
#'   tax_sort(by = sum, at = "Phylum") %>%
#'   tax_table() %>%
#'   head()
#'
#' # You can sort by ascending abundance (or prevalence etc) by reversing after
#' dietswap %>%
#'   tax_sort(by = "prev", at = "Phylum") %>%
#'   tax_sort(by = "rev") %>%
#'   tax_table() %>%
#'   head()
tax_sort <- function(data,
                     by = "name",
                     at = "names",
                     ...,
                     tree_warn = TRUE,
                     verbose = TRUE,
                     trans = "identity",
                     use_counts = TRUE) {

  # checks, and get phyloseq--------------------------------------------------
  stopifnot(rlang::is_bool(use_counts))
  stopifnot(rlang::is_bool(tree_warn))
  if (!rlang::is_string(trans)) stop("`trans` must be a transformation name")
  if (!rlang::is_string(at)) stop("`at` must be 'names' or a tax rank")
  byIsInvalidError <- paste0(
    "`by` argument must be one of:\n",
    "- 'rev' to reverse the current order\n",
    "- 'asis' to keep the current order as is\n",
    "- 'name' (alphabetical sorting)\n",
    "- summary stat. function e.g. `sum` or `mean` (don't forget na.rm)\n",
    "- 'prev' or 'prevalence' using value of optional `undetected` arg\n",
    "- a sample name (abundance sorting within that sample)\n"
  )
  if (!rlang::is_string(by) && !rlang::is_function(by)) stop(byIsInvalidError)
  # TODO ?allow numeric or character vector sorting by subsetting?

  # get and check phyloseq (of counts?)
  ps <- if (use_counts) ps_counts(data, warn = TRUE) else ps_get(data)
  ps <- phyloseq_validate(ps, remove_undetected = FALSE, verbose = verbose)

  # check `by` string options including sample names
  if (rlang::is_string(by)) {
    sampleNames <- phyloseq::sample_names(ps)
    byValidStrings <- c("asis", "rev", "name", "prev", "prevalence")
    if (!by %in% byValidStrings && !by %in% sampleNames) stop(byIsInvalidError)
    # check `by` is not both a sample name and other string option!
    if (by %in% byValidStrings && by %in% sampleNames) {
      stop("Unclear how to sort, because there is a sample named: ", by)
    }
  }

  if (!identical(by, "asis") && !identical(by, "rev")) {
    # check `at` options including rank names
    psCheckRanks(ps, rank = at, varname = "at", or = c("unique", "names"))
  }

  # generate taxSorted (vector of taxa names) --------------------------------

  # apply sorting rules that don't require otu table or tax table
  if (identical(by, "asis")) taxSorted <- phyloseq::taxa_names(ps)
  if (identical(by, "rev")) taxSorted <- rev(phyloseq::taxa_names(ps))

  # sort by "name" --> aggregation of taxa not required regardless of `at`
  if (identical(by, "name")) {
    if (at %in% phyloseq::rank_names(ps)) {
      if (at == "names") warning("Using the taxa names, not the rank 'names'!")
      tt <- unclass(phyloseq::tax_table(ps))
      new_order <- order(unname(tt[, at])) # ordering named character -> wrong!
      taxSorted <- phyloseq::taxa_names(ps)[new_order]
    }
    if (at == "names") taxSorted <- sort(phyloseq::taxa_names(ps))
  }

  # sorting methods that use otu_table (a function, or abundance in 1 sample)
  if (rlang::is_function(by) || by %in% c("prev", "prevalence", sampleNames)) {
    if (at == "names") {
      # aggregation not required
      if ("names" %in% phyloseq::rank_names(ps)) {
        warning("Using unaggregated taxa, not the rank 'names'!")
      }
      if (trans != "identity") {
        ps <- tax_transform(ps, trans = trans, rank = NA)[["ps"]]
      }
      taxSorted <- tax_sort_by_otu(ps, by = by, err = byIsInvalidError, ...)
    } else {
      # it was checked already that it must otherwise be a rank name
      # --> aggregation required
      psAg <- tax_agg(ps = ps, rank = at)[["ps"]]
      psAg <- tax_sort(data = psAg, by = by, at = "names", trans = trans, ...)

      # the aggregated tax_table has different dimensions to the un-aggregated
      # make tax table of un-aggregated phyloseq as dataframe
      tt <- as.data.frame.matrix(
        x = unclass(phyloseq::tax_table(ps)),
        optional = TRUE, make.names = FALSE
      )
      # sort un-aggregated data based on order of taxa in aggregated phyloseq
      tt[, at] <- factor(tt[, at], levels = phyloseq::taxa_names(psAg))
      tt <- dplyr::arrange(tt, dplyr::across(dplyr::all_of(at)))
      taxSorted <- rownames(tt)
    }
  }

  # use taxSorted vector to reorder taxa in ORIGINAL phyloseq -----------------
  ps <- tax_reorder(
    ps = ps_get(data), tax_order = taxSorted, tree_warn = tree_warn
  )

  # return ps_extra if given one
  if (inherits(data, "ps_extra")) {
    data$ps <- ps
    if (!identical(data$counts, NULL)) {
      data$counts <- tax_reorder_otu(data$counts, tax_order = taxSorted)
    }
    return(data)
  } else {
    return(ps)
  }
}

# internal helper function does main sorting of phyloseq object
# returns sorted character vector of taxa names
# names can then be used to order original unnaggregated ps if desired
# or if ps was not aggregated, just use this on original otu_table
tax_sort_by_otu <- function(ps, by, err, ...) {
  # check (again) argument types
  if (!rlang::is_string(by) && !rlang::is_function(by)) stop(err)

  if (rlang::is_string(by, string = c("prev", "prevalence"))) {
    ps <- tax_transform(ps, trans = "binary", ...)
    by <- base::sum # sum of binary transformed otu_table is prevalence!
  }
  # otu_get --> taxa as columns! (unclass leaves as a matrix)
  otu <- unclass(otu_get(ps))
  tax_names_in <- colnames(otu)

  if (rlang::is_string(by)) {
    if (!by %in% phyloseq::sample_names(ps)) stop(err, "`by` is: ", by)
    new_order <- order(otu[by, ], decreasing = TRUE)
  }
  if (rlang::is_function(by)) {
    result <- apply(X = otu, MARGIN = 2, FUN = by, ...)
    new_order <- order(result, decreasing = TRUE)
  }
  # double check all taxa will be returned
  stopifnot(length(new_order) == length(tax_names_in))
  tax_names_out <- tax_names_in[new_order]
  return(tax_names_out)
}

#' Reorder taxa in phyloseq object using vector of names
#'
#' @param ps phyloseq object
#' @param tax_order
#' Names of taxa in desired order; at least some must match.
#' (Numerical indices are also possible)
#' @param tree_warn
#' If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot be reordered.
#' Default behaviour of tax_sort is to remove the phylogenetic tree and warn about this.
#' tree_warn = FALSE will suppress the warning message, but still remove the tree!
#' @param unmatched_warn
#' Warn if any names (or indices) given in tax_order are not found within
#' (range of) taxa_names(ps) - these will be ignored
#' @param ignore
#' Values that you do not want to be used for reordering taxa
#' (useful for comp_barplot when custom palette names are used to set tax_order)
#'
#' @return phyloseq object (always without phy_tree)
#' @examples
#' data("dietswap", package = "microbiome")
#' new_order <- c(
#'   "Fusobacteria", "Cyanobacteria", "Verrucomicrobia", "Spirochaetes",
#'   "Actinobacteria", "Firmicutes", "Proteobacteria", "Bacteroidetes"
#' )
#' tax_agg(dietswap, rank = "Phylum") %>%
#'   ps_get() %>%
#'   phyloseq::taxa_names()
#'
#' tax_agg(dietswap, rank = "Phylum") %>%
#'   ps_get() %>%
#'   tax_reorder(tax_order = new_order) %>%
#'   phyloseq::taxa_names()
#'
#' # partial reordering (of the frontmost positions only) is possible
#' tax_agg(dietswap, rank = "Phylum") %>%
#'   ps_get() %>%
#'   tax_reorder(tax_order = c("Cyanobacteria", "Bacteroidetes")) %>%
#'   phyloseq::taxa_names()
#'
#' @export
tax_reorder <- function(ps,
                        tax_order,
                        tree_warn = TRUE,
                        unmatched_warn = TRUE,
                        ignore = c("other", "Other")
) {
  # can't sort taxa if phylogenetic tree present, as tree fixes order
  if (!identical(phyloseq::phy_tree(ps, errorIfNULL = FALSE), NULL)) {
    if (isTRUE(tree_warn)) {
      warning(
        "Removing phylogenetic tree!\n",
        "Avoid this warning by either by\n",
        "\t- setting tree_warn = FALSE\n",
        "\t- removing the tree yourself, e.g. `ps@phy_tree <- NULL`"
      )
    }
    ps@phy_tree <- NULL
  }

  # check valid tax_order class
  if (!rlang::is_character(tax_order) && !rlang::is_integerish(tax_order)) {
    stop("tax_order arg must be character or numeric (integerish)")
  }
  if (!rlang::is_character(ignore)) stop("ignore must be character vector")

  # establish new order in full (as partial rearrangement is possible)
  if (rlang::is_integerish(tax_order)) {
    currentOrder <- seq_along(phyloseq::taxa_names(ps))
  } else {
    currentOrder <- phyloseq::taxa_names(ps)
    tax_order <- setdiff(tax_order, ignore)
  }
  if (isTRUE(unmatched_warn) && any(!tax_order %in% currentOrder)) {
    warning(
      length(setdiff(tax_order, currentOrder)),
      " taxa specified in tax_order are not in phyloseq ps: they are ignored"
    )
  }
  bring2front <- intersect(tax_order, currentOrder)
  if (length(bring2front) == 0) stop("tax_order did not match any taxa in ps")
  keepAsIs <- setdiff(currentOrder, bring2front)
  newOrder <- c(bring2front, keepAsIs)

  # reorder otu_table
  phyloseq::otu_table(ps) <- tax_reorder_otu(
    otu = otu_get(ps), tax_order = newOrder
  )

  return(ps)
}

# internal helper for tax_reorder, reorders otu_table without
# changing orientation
tax_reorder_otu <- function(otu, tax_order) {
  taxaWereRows <- phyloseq::taxa_are_rows(otu)
  otu <- unclass(otu)
  if (taxaWereRows) otu <- t(otu)
  otu <- otu[, tax_order, drop = FALSE]
  if (taxaWereRows) otu <- t(otu)
  otu <- phyloseq::otu_table(
    object = otu, taxa_are_rows = taxaWereRows
  )
  return(otu)
}
