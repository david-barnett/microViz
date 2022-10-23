#' Add logical label column to taxatree_stats dataframe
#'
#' @description
#' `taxatree_label` is used internally by `taxatree_plotkey`, but
#' can also be used prior to `taxatree_plots` to label those plots directly
#'
#' `...` arguments are passed to `dplyr::filter()`,
#' so all `filter` syntax can be used.
#'
#' @details
#' If taxatree_stats missing (or if data is a phyloseq)
#' it will create a plain taxatree_stats dataframe using only taxatree_nodes
#'
#' `node_fun` can also be a precalculated dataframe (output of taxatree_nodes)
#' but you should probably not use this option.
#' This is used internally for efficiency inside `taxatree_plotkey()`
#'
#' @param data psExtra (or phyloseq)
#' @param ...
#' REQUIRED logical conditions for labelling
#' e.g. rank == "Phylum", p.value < 0.1 | taxon %in% listOfTaxa
#' @param .node_fun
#' named list of length 1 providing `taxatree_nodes` `fun` arg.
#' (name of list iterm is available for use in ...)
#' @param .label_var name of label indicator variable to be created.
#' If you change this, beware that taxatree_plotkey will not work, you will
#' need to called taxatree_plot_label with
#'
#' @return psExtra with (modified) taxatree_stats dataframe
#' @export
#'
#' @examples
#' # simple example with plain phyloseq input
#' data("dietswap", package = "microbiome")
#' labelled <- dietswap %>%
#'   tax_prepend_ranks() %>%
#'   taxatree_label(rank == "Phylum", prevalence > 0.1)
#'
#' # Note that "prevalence" column was available in data
#' # because it is created by `taxatree_nodes()` using the named function
#' # provided to the `node_fun` argument
#'
#' # psExtra is returned
#' labelled
#'
#' # notice how both conditions must be met for label column to be TRUE
#' labelled$taxatree_stats
taxatree_label <- function(data,
                           ...,
                           .label_var = "label",
                           .node_fun = list(prevalence = prev)) {

  # get node data or use node data if already provided
  if (inherits(.node_fun, "data.frame")) {
    treeNodes <- .node_fun
  } else {
    treeNodes <- taxatree_nodes(data, fun = .node_fun, .use_counts = TRUE)
  }

  # get taxatree_stats if present
  if (is(data, "psExtra") && !identical(data@taxatree_stats, NULL)) {
    stats <- data@taxatree_stats
    stats <- dplyr::left_join(stats, treeNodes, by = c("taxon", "rank"))
  } else {
    stats <- treeNodes
  }

  # filter stats dataframe to label taxa
  stats[["..ROW.ID.."]] <- rownames(stats)
  labelled <- dplyr::filter(.data = stats, ...)
  stats[[.label_var]] <- stats[["..ROW.ID.."]] %in% labelled[["..ROW.ID.."]]
  stats[["..ROW.ID.."]] <- NULL
  stats <- dplyr::as_tibble(stats)

  # modify or create psExtra
  if (!is(data, "psExtra")) data <- psExtra(data, info = new_psExtraInfo())
  data@taxatree_stats <- stats

  return(data)
}
