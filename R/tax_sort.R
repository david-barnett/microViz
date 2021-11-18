#' Sort taxa in phyloseq otu_table and tax_table
#'
#' @description
#' Multiple ways of sorting taxa are possible and determined by the `by` argument.
#' The `by` argument must be one of:
#'  - 'rev' to reverse the current order
#'  - 'name' (sort alphabetically by `at`)
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
#' @param ... used if summary function given, or pass `undetected` arg for tax_transform("binary") if by = "prev" or "prevalence"
#' @param tree_warn
#' If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot be reordered.
#' Default behaviour of tax_sort is to remove the phylogenetic tree and warn about this.
#' tree_warn = FALSE will suppress the warning message, but still remove the tree!
#' @param verbose
#' passed to phyloseq_validate verbose
#' (if TRUE: message about suspicious values in tax_table, and how to fix)
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
#' # sort by function e.g. median abundance
#' dietswap %>%
#'   tax_sort(by = median) %>%
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
#' # tax_sort will remove the tree, and warn you about this.
#'
#' # You can sort by abundance at higher taxonomic ranks,
#' # without losing lower rank info
#' # e.g. sort (descending) by phyla abundances
#' dietswap %>%
#'   tax_sort(by = sum, at = "Phylum") %>%
#'   tax_table() %>%
#'   head()
#'
#' # You can sort by ascending abundance by reversing afterwards
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
                     use_counts = TRUE) {
  if (!inherits(at, "character")) stop("`at` must be 'names' or a tax rank")
  by_is_invalid_error <- paste0(
    "`by` argument must be one of:\n",
    "- 'rev' to reverse the current order\n",
    "- 'name' (alphabetical sorting)\n",
    "- summary stat. function e.g. `sum` or `mean` (don't forget na.rm)\n",
    "- 'prev' or 'prevalence' using value of optional `undetected` arg\n",
    "- a sample name (abundance sorting within that sample)\n"
  )
  if (!inherits(by, "character") &&
    !inherits(by, "function") ||
    length(by) != 1
  ) {
    stop(by_is_invalid_error)
    # TODO allow numeric or character vector sorting by subsetting?
  }
  # get components that are always required
  if (isTRUE(use_counts)) {
    ps <- ps_counts(data = data, warn = TRUE)
  } else {
    ps <- ps_get(data)
  }
  ps <- phyloseq_validate(
    ps = ps, remove_undetected = FALSE, verbose = verbose
  )

  # apply sorting rules that don't require otu table, for calculation
  # generate taxSorted (vector of taxa names)
  if (identical(by, "rev")) {
    # rev is special case: ignores `at`
    taxSorted <- rev(phyloseq::taxa_names(ps))
  } else if (identical(by, "name")) {
    if (identical(at, "names")) {
      taxSorted <- sort(phyloseq::taxa_names(ps))
    } else if (length(at) == 1 && at %in% phyloseq::rank_names(ps)) {
      tt <- unclass(phyloseq::tax_table(ps))
      new_order <- order(unname(tt[, at])) # ordering named character -> wrong
      taxSorted <- phyloseq::taxa_names(ps)[new_order]
    } else {
      stop("`at` must be 'names' or one of phyloseq::rank_names(ps)\n")
    }
  } else {
    if (identical(at, "names")) {
      # sort phyloseq, return reordered taxa (character vector)
      taxSorted <- tax_sort_by_otu(ps, by = by, err = by_is_invalid_error, ...)
    } else if (at %in% phyloseq::rank_names(ps)) {
      psAgg <- tax_agg(ps = ps, rank = at)[["ps"]]
      psAgg <- tax_sort(data = psAgg, by = by, at = "names", ...)
      # make taxTable of original ps as df
      tt <- unclass(phyloseq::tax_table(ps)) %>%
        as.data.frame.matrix(optional = TRUE, make.names = FALSE)
      tt[, at] <- factor(tt[, at], levels = phyloseq::taxa_names(psAgg))
      tt <- dplyr::arrange(tt, dplyr::across(dplyr::all_of(at)))
      taxSorted <- rownames(tt)
    } else {
      stop("`at` must be 'names' or one of phyloseq::rank_names(ps)\n")
    }
  }

  # reorder taxa in phyloseq with taxSorted vector
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
  if (identical(by, "prev") || identical(by, "prevalence")) {
    ps <- tax_transform(ps, trans = "binary", ...)
    by <- base::sum # sum of binary transformed otu_table is prevalence!
  }
  # otu_get --> taxa as columns! (unclass leaves as a matrix)
  otu <- unclass(otu_get(ps))
  tax_names_in <- colnames(otu)

  if (inherits(by, "character") && length(by) == 1) {
    if (by %in% phyloseq::sample_names(ps)) {
      new_order <- order(otu[by, ], decreasing = TRUE)
    } else {
      stop(err, "`by` is: ", by)
    }
  } else if (inherits(by, "function")) {
    result <- apply(X = otu, MARGIN = 2, FUN = by, ...)
    new_order <- order(result, decreasing = TRUE)
  } else {
    stop(err, "`by` is: ", by)
  }
  tax_names_out <- tax_names_in[new_order]
  return(tax_names_out)
}

#' Reorder taxa in phyloseq object using vector of names
#'
#' @param ps phyloseq object
#' @param tax_order
#' names or current numerical indices of taxa
#' in desired order and same length as taxa_names(ps)
#' @param tree_warn
#' If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot be reordered.
#' Default behaviour of tax_sort is to remove the phylogenetic tree and warn about this.
#' tree_warn = FALSE will suppress the warning message, but still remove the tree!
#'
#' @return phyloseq object (always without phy_tree)
#' @examples
#' data("dietswap", package = "microbiome")
#' new_order <- c(
#'   "Fusobacteria", "Cyanobacteria", "Verrucomicrobia", "Spirochaetes",
#'   "Actinobacteria", "Firmicutes", "Proteobacteria", "Bacteroidetes"
#' )
#' tax_agg(dietswap, rank = "Phylum")[["ps"]] %>%
#'   phyloseq::taxa_names()
#' tax_agg(dietswap, rank = "Phylum")[["ps"]] %>%
#'   microViz:::tax_reorder(tax_order = new_order) %>%
#'   phyloseq::taxa_names()
tax_reorder <- function(ps, tax_order, tree_warn = TRUE) {
  stopifnot(identical(length(phyloseq::taxa_names(ps)), length(tax_order)))

  tax_as_rows <- phyloseq::taxa_are_rows(ps)

  # can't sort taxa if phylogenetic tree present, as tree fixes order
  if (!identical(phyloseq::phy_tree(ps, errorIfNULL = FALSE), NULL)) {
    if (isTRUE(tree_warn)) {
      warning(
        "tax_sort is removing phylogenetic tree!\n",
        "Avoid this warning by either by\n",
        "\t- running tax_sort with tree_warn = FALSE\n",
        "\t- or removing tree yourself, e.g. `ps@phy_tree <-- NULL`"
      )
    }
    ps@phy_tree <- NULL
  }

  # reorder otu_table
  phyloseq::otu_table(ps) <- tax_reorder_otu(
    otu = otu_get(ps), tax_order = tax_order
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
