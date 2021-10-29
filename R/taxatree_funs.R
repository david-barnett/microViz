#' @name taxatree_funs
#'
#' @title
#' Create node and edge dataframes for taxatree_plots
#'
#' @description
#'  Mostly you will not have to use these functions directly:
#'  instead call `taxatree_plots` with the output of `taxatree_stats`
#'
#' - `taxatree_nodes` creates taxon nodes and calculates a summary statistic
#'  about each taxon (given by `fun`). Takes a ps_extra or phyloseq object.
#'
#' - `taxatree_edges` uses the output of `taxatree_nodes` to create a
#' dataframe of edges.
#'
#' @details
#' `taxatree_nodes` makes nodes for taxa at all ranks or for a list of
#' consecutive ranks (plus a root rank if tree is not rooted).
#'
#' @param ps phyloseq object or ps_extra
#' @param ranks
#' selection of taxonomic ranks to make nodes for ("all", or names)
#' @param fun function to calculate for each taxon/node
#' @param .sort
#' sort nodes by "ascending" or "descending" values of fun function result
#' @param .use_counts use count data if available (instead of transformed data)
#'
#' @rdname taxatree_funs
#' @export
taxatree_nodes <- function(ps,
                           fun = list(sum = sum),
                           ranks = "all",
                           .sort = NULL,
                           .use_counts = TRUE) {
  if (isTRUE(.use_counts)) {
    ps <- ps_counts(ps)
  } else {
    ps <- ps_get(ps)
  }

  # check fun format
  if (!is.list(fun) || !inherits(fun[[1]], "function") || any(is.null(names(fun)))) {
    stop("fun must be a length 1 named list holding a function for a vector")
  }


  # check if there is more than one value in top level: if so, add a root level
  if (length(unique(unclass(phyloseq::tax_table(ps))[, 1])) > 1) {
    phyloseq::tax_table(ps) <- cbind(root = "root", phyloseq::tax_table(ps))
  }

  # identify numerical selection of ranks (all, names or numbers)
  phyloseq_ranks <- phyloseq::rank_names(ps)
  rank_nums <- taxatree_nodes_ranksAsNumeric(ps = ps, ranks = ranks)

  # iterate over ranks, calculating stats for all taxa
  nodes_info <- lapply(X = rank_nums, FUN = function(r) {
    ps <- ps_get(tax_agg(ps, rank = phyloseq_ranks[[r]]))

    # get basic taxon info
    tt <- phyloseq::tax_table(ps)
    taxon <- as.vector(tt[, phyloseq_ranks[[r]]])
    parent <- as.vector(tt[, phyloseq_ranks[[max(r - 1, 1)]]])
    rank <- phyloseq_ranks[[r]]

    # calculate stat with fun function
    otu <- otu_get(ps)
    stat <- apply(otu, MARGIN = 2, FUN = fun[[1]])

    # create dataframe
    df <- data.frame(taxon = taxon, parent = parent, rank = rank)
    df[[names(fun)[[1]]]] <- stat

    return(df)
  })
  nodes_df <- purrr::reduce(nodes_info, rbind.data.frame)
  taxatree_nodes_checkLoops(nodes_df)

  # sort if requested
  if (identical(.sort, "ascending") | identical(.sort, "increasing")) {
    nodes_df[order(nodes_df[[names(fun)[[1]]]], decreasing = FALSE), ]
  }
  if (identical(.sort, "descending") | identical(.sort, "decreasing")) {
    nodes_df[order(nodes_df[[names(fun)[[1]]]], decreasing = TRUE), ]
  }
  return(nodes_df)
}

# internal helper for taxatree_nodes
taxatree_nodes_ranksAsNumeric <- function(ps, ranks) {
  phyloseq_ranks <- phyloseq::rank_names(ps)
  if (identical(ranks, "all")) {
    rank_nums <- seq_along(phyloseq_ranks)
  } else if (class(ranks) %in% c("numeric", "integer")) {
    rank_nums <- ranks
  } else if (!all(ranks %in% phyloseq_ranks)) {
    stop(
      paste(ranks, collapse = ", "),
      " are not all available in phyloseq object"
    )
  } else {
    rank_nums <- match(x = ranks, table = phyloseq_ranks)
  }
  # ensure rank number are in increasing order
  rank_nums <- sort(rank_nums)

  # ensure first (root) rank always included
  rank_nums <- union(1, rank_nums)

  return(rank_nums)
}

taxatree_nodes_checkLoops <- function(df) {
  # check if any nodes connect to themselves
  # (would happen if tax_table entries duplicated across ranks)
  df <- df[-1, ]
  if (any(df[["taxon"]] == df[["parent"]])) {
    df <- dplyr::filter(df, taxon == parent)
    rank <- df[["rank"]]
    taxon <- df[["taxon"]]
    parent <- df[["parent"]]
    stop(
      call. = FALSE,
      "tax_table values must not be duplicated across ranks, but some are:",
      "\n - e.g. the parent of the ", rank[1], " named '", taxon[1],
      "' is also named '", parent[1], "'\n",
      " - Fix duplicated names with `tax_prepend_ranks()` or fix them manually"
    )
  }
  # TODO a more comprehensive error message?
}

#' @param nodes_df dataframe output from taxatree_nodes
#'
#' @rdname taxatree_funs
#' @export
taxatree_edges <- function(nodes_df) {
  edge_list <- lapply(
    X = nodes_df[["taxon"]],
    FUN = function(unique_name) {
      taxon_row_index <- nodes_df[["taxon"]] == unique_name
      data.frame(
        from = nodes_df[taxon_row_index, "parent"],
        to = unique_name
      )
    }
  )
  edge_df <- purrr::reduce(edge_list, rbind.data.frame)
  # remove any duplicate edges
  edge_df <- dplyr::distinct(edge_df, dplyr::across(dplyr::everything()))
  # remove nodes that point to themselves (root level always will)
  edge_df <- dplyr::filter(edge_df, .data$from != .data$to)
  # edge_df gets all attributes from the "to" node
  edge_df <- dplyr::left_join(x = edge_df, y = nodes_df, by = c("to" = "taxon"))

  return(edge_df)
}
