#' Draw labelled key to accompany taxatree_plots
#'
#' @inheritParams taxatree_plots
#'
#' @param data ps_extra (or phyloseq)
#' @param ...
#' logical conditions for labelling
#' e.g. rank == "Phylum", p.value < 0.1 | taxon %in% listOfTaxa
#' @param colour fixed colour and fill of nodes and edges
#' @param title title of plot (NULL for none)
#' @param label_fun function used to draw labels
#' @param label_fun_args arguments passed to label fun (for styling)
#' @param .combine_label
#' all or any: function to combine multiple logical "label" values for a taxon
#' (relevant if taxatree_stats already present in data)
#' @param .calc_label
#' if you already set labels with taxatree_label, set this to FALSE to use those
#'
#' @return ggplot
#' @export
#'
#' @examples
#' # see taxatree_plots() examples
taxatree_plotkey <- function(data,
                             ...,
                             size_stat = list(prevalence = prev),
                             node_size_range = c(1.5, 5),
                             edge_width_range = node_size_range * 0.8,
                             size_guide = "legend",
                             size_trans = "identity",
                             colour = "lightgrey",
                             edge_alpha = 0.7,
                             title = "Key",
                             taxon_renamer = identity,
                             label_fun = ggrepel::geom_label_repel,
                             label_fun_args = list(
                               min.segment.length = 0, box.padding = 0,
                               label.padding = 0.15
                             ),
                             .combine_label = any,
                             .calc_label = TRUE,
                             layout = "tree",
                             layout_seed = NA,
                             circular = identical(layout, "tree"),
                             node_sort = NULL,
                             add_circles = isTRUE(circular),
                             drop_ranks = TRUE) {

  # make basic nodes
  if (isTRUE(drop_ranks) &&
    inherits(data, "ps_extra") &&
    !identical(data$taxatree_stats, NULL)
  ) {
    ranks <- unique(data[["taxatree_stats"]][["rank"]])
  } else {
    ranks <- "all"
  }
  treeNodes <- taxatree_nodes(
    ps = data, fun = size_stat, .sort = node_sort,
    ranks = ranks, .use_counts = TRUE
  )

  # make labels
  data <- taxatree_label(data = data, ..., node_fun = treeNodes)
  label_data <- data$taxatree_stats[, c("taxon", "label")]
  label_data <- dplyr::summarise(
    .data = dplyr::group_by(.data = label_data, .data$taxon),
    label = .combine_label(.data$label)
  )
  # join label_data to treeNodes
  treeNodes <- dplyr::left_join(treeNodes, label_data, by = "taxon")

  # make graph
  treeEdges <- taxatree_edges(treeNodes)
  treeGraph <- tidygraph::tbl_graph(
    nodes = treeNodes, edges = treeEdges, node_key = "taxon", directed = TRUE
  )
  # set seed if requested, for stochastic layouts
  if (!identical(layout_seed, NA)) set.seed(layout_seed)
  layout <- ggraph::create_layout(
    graph = treeGraph, layout = layout, circular = circular
  )

  # create plot from graph
  p <- ggraph::ggraph(graph = layout)
  if (isTRUE(add_circles)) {
    p <- taxatree_plotCircles(p = p, layout = layout)
  }
  p <- p +
    ggraph::geom_edge_link(
      mapping = ggplot2::aes(
        edge_width = .data[[names(size_stat)[[1]]]]
      ),
      edge_colour = colour, alpha = edge_alpha
    ) +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        size = .data[[names(size_stat)[[1]]]],
      ),
      color = colour, fill = colour, shape = "circle filled"
    ) +
    ggplot2::ggtitle(label = title)

  p <- taxatree_plotRootNode(p = p, size_stat = names(size_stat)[[1]])

  # set size ranges
  p <- taxatree_plotSizeScaling(
    p = p, node_size_range = node_size_range, node_size_guide = size_guide,
    edge_width_range = edge_width_range, size_trans = size_trans
  )
  # set basic theme and coords
  p <- taxatree_plot_styling(p = p, circular = circular)

  # add labels
  p <- taxatree_plotLabels(
    p = p, fun = label_fun, fun_args = label_fun_args,
    taxon_renamer = taxon_renamer
  )

  return(p)
}

# helper to add labels to tree using arbitrary labelling function
taxatree_plotLabels <- function(p,
                                fun = ggrepel::geom_label_repel,
                                fun_args = list(
                                  min.segment.length = 0, box.padding = 0,
                                  label.padding = 0.15
                                ),
                                taxon_renamer = identity) {

  # set universal default arguments
  args <- list(
    data = ~ dplyr::filter(., label),
    mapping = ggplot2::aes(
      x = .data$x, y = .data$y, label = taxon_renamer(.data$taxon)
    ),
    size = 2, family = "sans"
  )
  # override or append with new argument entries
  args[names(fun_args)] <- fun_args
  p <- p + do.call(fun, args = args)

  return(p)
}
