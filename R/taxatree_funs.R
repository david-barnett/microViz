#' @name taxatree_funs
#'
#' @title Create node and edge dataframes for taxonomic tree graphs
#'
#' @description
#' - `taxatree_nodes` creates taxon nodes and calculates basic info about each taxon. From a phyloseq object.
#'
#' - `taxatree_edges` uses the output of `taxatree_nodes` to create a dataframe of edges.
#'
#' `taxatree_nodes` makes nodes for taxa at all ranks or for a list of consecutive (rooted) ranks.
#' You can also join at dataframe of additional taxon level information to the output of `taxatree_nodes` before calling taxatree_edges, e.g. the output of `models2stats_corncob`.
#'
#'
#' @param ps phyloseq object
#' @param ranks selection of taxonomic ranks to make nodes for (all, names or numbers)
#' @param nodes_df the dataframe output of `taxatree_nodes` (possibly with extra info/stats joined to it)
#'
#' @rdname taxatree_funs
#' @export
taxatree_nodes <- function(ps, ranks = "all") {

  # identify numerical selection of ranks (all, names or numbers)
  available_rank_names <- phyloseq::rank_names(ps)
  if (identical(ranks, "all")) {
    rank_nums <- seq_along(available_rank_names)
  } else if (class(ranks) %in% c("numeric", "integer")) {
    rank_nums <- ranks
  } else if (!all(ranks %in% available_rank_names)) {
    stop(paste(ranks, collapse = ", "), " are not all available in phyloseq object")
  } else {
    rank_nums <- match(ranks, table = available_rank_names)
  }

  # get number of samples for calculations at all ranks
  n_samples <- phyloseq::nsamples(ps)

  # iterate over ranks, calculating stats for all taxa
  node_info <- lapply(X = rank_nums, FUN = function(r) {
    ps <- microbiome::aggregate_taxa(ps, level = available_rank_names[[r]])
    out <-
      data.frame(
        taxon_name = as.vector(phyloseq::tax_table(ps)[, available_rank_names[[r]]]),
        taxon_parent = as.vector(phyloseq::tax_table(ps)[, available_rank_names[[max(r - 1, 1)]]]),
        taxon_level = available_rank_names[[r]],
        taxon_count = phyloseq::taxa_sums(ps)
      )
    out$taxon_mean <- out$taxon_count / n_samples
    return(out)
  })
  node_df <- purrr::reduce(node_info, rbind.data.frame)

  return(node_df)
}

#' @rdname taxatree_funs
#' @export
taxatree_edges <- function(nodes_df) {
  edge_list <- lapply(
    X = nodes_df$taxon_name,
    FUN = function(unique_name) {
      taxon_row_index <- nodes_df$taxon_name == unique_name
      data.frame(
        from = nodes_df[taxon_row_index, "taxon_parent"],
        to = unique_name
      )
    }
  )
  edge_df <- purrr::reduce(edge_list, rbind.data.frame)
  # remove any duplicate edges
  edge_df <- dplyr::distinct(edge_df, dplyr::across(tidyselect::everything()))
  # remove nodes that point to themselves (root level always will)
  edge_df <- edge_df[edge_df$from != edge_df$to, ]
  # edge_df gets all attributes from the "to" node
  edge_df <- dplyr::left_join(x = edge_df, y = nodes_df, by = c("to" = "taxon_name"))

  return(edge_df)
}
