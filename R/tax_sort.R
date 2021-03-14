#' Sort taxa in phyloseq otu_table and tax_table
#'
#' Multiple ways of sorting taxa are possible and determined by the by argument.
#' `by` argument must be one of:
#'     - 'rev' to reverse the current order
#'     - 'name' or 'names' (sort alphabetically by taxa_names)
#'     - a taxonomic rank name (sort alphabetically by this rank)
#'     - a sample name (descending abundance sorting within that sample)
#'     - summary stat. function e.g. `sum` or `mean`
#'
#' Don't forget to pass `na.rm` to `...` if using a summary stat function in by
#'
#' @param data ps_extra or phyloseq
#' @param by how to sort, see description
#' @param ... used if summary function given, or pass `undetected` arg for tax_transform("binary") if by = "prev" or "prevalence"
#'
#' @return sorted phyloseq or ps_extra
#' @export
#'
#' @examples
#' library(microbiome)
#' data("dietswap")
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
#'   tax_sort("Phylum") %>%
#'   tax_table() %>%
#'   head(30)
#'
#' # sequentially sorting by higher ranks
#' # sets tax_table in nested alphabetical order
#' dietswap %>%
#'   tax_sort("names") %>%
#'   tax_sort("Genus") %>%
#'   tax_sort("Family") %>%
#'   tax_sort("Phylum") %>%
#'   tax_table() %>%
#'   head(30)
#'
#' # order by descending abundance in a single named sample
#' dietswap %>%
#'   tax_sort("Sample-1") %>%
#'   otu_table() %>%
#'   .[1:5, 1:4]
#'
#' # sort by function e.g. median abundance
#' dietswap %>%
#'   tax_sort(by = median) %>%
#'   taxa_names() %>%
#'   head(20)
#'
#' # sum order should always equal mean order if non-negative abundances
#' # don't forget to add na.rm = TRUE if you expect NAs in otu_table somehow
#' dietswap %>%
#'   tax_sort(by = sum, na.rm = TRUE) %>%
#'   taxa_names() %>%
#'   head(20)
tax_sort <- function(data, by = "name", ...) {
  by_is_invalid_error <- paste0(
    "`by` argument must be one of:\n",
    "- 'rev' to reverse the current order\n",
    "- 'name' or 'names' (alphabetical by taxa_names)\n",
    "- a taxonomic rank name\n",
    "- summary stat. function e.g. `sum` or `mean` (don't forget na.rm)\n",
    "- 'prev' or 'prevalence' using value of optional `undetected` arg\n",
    "- a sample name (abundance sorting within that sample)\n"
  )
  if (
    !inherits(by, "character") &&
      !inherits(by, "function") ||
      length(by) != 1
  ) {
    stop(by_is_invalid_error)
       # TODO allow numeric or character vector sorting by subsetting?
  }
  # get components that are always required
  ps <- ps_get(data)
  tax_as_rows <- phyloseq::taxa_are_rows(ps)
  # taxa as columns from otu_get
  otu <- unclass(otu_get(ps))

  if (inherits(by, "character")) {
    # reverse order
    if (identical(by, "rev")) {
      new_order <- rev(phyloseq::taxa_names(ps))
    } else if (identical(by, "name") || identical(by, "names")) {
      new_order <- sort(phyloseq::taxa_names(ps))
    } else if (by %in% phyloseq::rank_names(ps)) {
      tt <- unclass(tt_get(ps))
      new_order <- order(tt[, by])
    } else if (by %in% phyloseq::sample_names(ps)) {
      new_order <- order(otu[by, ], decreasing = TRUE)
    } else if (by %in% c("prev", "prevalence")){
      otu_binary <- otu_get(tax_transform(ps, transformation = "binary", ...))
      result <- apply(X = unclass(otu_binary), MARGIN = 2, FUN = sum)
      new_order <- order(result, decreasing = TRUE)
    } else {
      stop(by_is_invalid_error)
    }
  } else if (inherits(by, "function")) {
    result <- apply(X = otu, MARGIN = 2, FUN = by, ...)
    new_order <- order(result, decreasing = TRUE)
  } else {
    stop(by_is_invalid_error)
  }
  # actually sort
  otu <- otu[, new_order]

  # return otu_table oriented as found
  if (tax_as_rows) otu <- t(otu)
  phyloseq::otu_table(ps) <- phyloseq::otu_table(
    object = otu, taxa_are_rows = tax_as_rows
  )

  # return ps_extra if given one
  if (inherits(data, "ps_extra")){
    data$ps <- ps
    return(data)
  } else {
    return(ps)
  }
}
