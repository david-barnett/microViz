#' Draw labelled key to accompany taxatree_plots
#'
#' @inheritParams taxatree_plots
#'
#' @param data psExtra (or phyloseq)
#' @param ...
#' logical conditions for labelling
#' e.g. rank == "Phylum", p.value < 0.1 | taxon %in% listOfTaxa
#' @param taxon_renamer
#' function that takes taxon names and returns modified names for labels
#' @param colour fixed colour and fill of nodes and edges
#' @param title title of plot (NULL for none)
#' @param .combine_label
#' all or any: function to combine multiple logical "label" values for a taxon
#' (relevant if taxatree_stats already present in data)
#' @param .draw_label
#' should labels be drawn, or just the bare tree, set this to FALSE if you want
#' to draw customised labels with taxatree_plot_labels afterwards
#' @param .calc_label
#' if you already set labels with taxatree_label:
#' set this to FALSE to use / avoid overwriting that data
#' (ignores `...` if FALSE)
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
                             size_guide = "none",
                             size_trans = "identity",
                             colour = "lightgrey",
                             edge_alpha = 0.7,
                             title = "Key",
                             title_size = 14,
                             taxon_renamer = identity,
                             .combine_label = any,
                             .draw_label = TRUE,
                             .calc_label = TRUE,
                             layout = "tree",
                             layout_seed = NA,
                             circular = identical(layout, "tree"),
                             node_sort = NULL,
                             add_circles = isTRUE(circular),
                             drop_ranks = TRUE) {

  # make basic nodes
  if (isTRUE(drop_ranks) && is(data, "psExtra") && !is.null(data@taxatree_stats, NULL)) {
    ranks <- unique(data@taxatree_stats[["rank"]])
  } else {
    ranks <- "all"
  }
  treeNodes <- taxatree_nodes(
    ps = data, fun = size_stat, .sort = node_sort,
    ranks = ranks, .use_counts = TRUE
  )

  if (isTRUE(.calc_label)) {
    # make labels
    data <- taxatree_label(data = data, ..., .node_fun = treeNodes)
    label_data <- data@taxatree_stats[, c("taxon", "label")]
    label_data <- dplyr::summarise(
      .data = dplyr::group_by(.data = label_data, .data$taxon),
      label = .combine_label(.data$label)
    )
    # join label_data to treeNodes
    treeNodes <- dplyr::left_join(treeNodes, label_data, by = "taxon")
  }

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
  p <- taxatree_plot_styling(
    p = p, circular = circular, expand = TRUE, title_size = title_size
  )

  if (isTRUE(.draw_label)) {
    # add labels
    p <- taxatree_plot_labels(
      p = p, taxon_renamer = taxon_renamer, circular = circular
    )
  }

  return(p)
}

#' Add labels to taxatree plots/key
#'
#' Finer control over label drawing for `taxatree_plotkey`
#' (with .draw_label = FALSE),
#' and label drawing for `taxatree_plots` output too.
#'
#' @param p taxatree_plotkey or taxatree_plots output plot
#' @param circular
#' is the plot layout circular? labels are drawn differently for circular trees
#' @param taxon_renamer
#' function that takes taxon names and returns modified names for labels
#' @param fun
#' ggrepel labelling function: geom_text_repel or geom_label_repel
#' @param label_var
#' name of variable in taxatree_stats that indicates which taxa to label
#' @param x_nudge
#' absolute amount by which the initial position of taxon labels is nudged
#' (relevant only for circular layouts, use nudge_x for other layouts)
#' @param y_nudge
#' absolute amount by which the initial position of taxon labels is nudged
#' (relevant only for circular layouts, use nudge_y for other layouts)
#' @param fontface fontface of label text
#' @param size size of labels
#' @param colour colour of label outlines and text
#' @param max.overlaps max number of overlapping labels tolerated
#' @param min.segment.length min length of label line segment to bother drawing
#' @param segment.size thickness of line segment
#' @param segment.color colour of line segment
#' @param point.padding padding around node points (for label positioning)
#' @param box.padding padding around labels/text (for label positioning)
#' @param seed set this for reproducible label positions
#' @inheritDotParams ggrepel::geom_text_repel
#' arrow force force_pull max.time max.iter xlim ylim direction verbose
#'
#' @export
taxatree_plot_labels <- function(p,
                                 circular = TRUE,
                                 taxon_renamer = identity,
                                 fun = ggrepel::geom_text_repel,
                                 label_var = "label",
                                 x_nudge = 0.1,
                                 y_nudge = 0.025,
                                 fontface = "bold",
                                 size = 2.5,
                                 colour = "grey15",
                                 max.overlaps = Inf,
                                 min.segment.length = 0,
                                 segment.size = 0.15,
                                 segment.color = "grey15",
                                 point.padding = 0.05,
                                 box.padding = 0.1,
                                 seed = NA,
                                 ...) {
  args <- list(
    p = p,
    fun = fun,
    label_var = label_var,
    mapping = ggplot2::aes(
      x = .data$x, y = .data$y, label = taxon_renamer(.data$taxon)
    ),
    fontface = fontface,
    size = size, colour = colour,
    max.overlaps = max.overlaps,
    min.segment.length = min.segment.length,
    segment.size = segment.size,
    segment.color = segment.color,
    point.padding = point.padding,
    box.padding = box.padding,
    seed = seed
  )

  # append/overwrite args with extra args
  dots <- list(...)

  if (isTRUE(circular)) {
    # add circular plot specific nudges and limits
    args[c("x_nudge", "y_nudge", "xlim", "ylim")] <- list(
      x_nudge, y_nudge, c(-1.5, 1.5), c(-1.5, 1.5)
    )
    args[names(dots)] <- dots

    for (pos in list(c("x", "y"), "y", NULL, "x")) {
      args[["pos"]] <- pos
      p <- do.call(taxatree_plot_labelsQuadrant, args = args)
      args[["p"]] <- p
    }
  } else {
    args[names(dots)] <- dots
    p <- do.call(taxatree_plot_labelsNotCircular, args = args)
  }
  return(p)
}

# helper for plotting labels on each quadrant of circular tree plot
taxatree_plot_labelsQuadrant <- function(p,
                                         taxon_renamer,
                                         fun = ggrepel::geom_text_repel,
                                         pos = NULL,
                                         label_var,
                                         x_nudge,
                                         y_nudge,
                                         ...) {
  neg <- c("x", "y")[!c("x", "y") %in% pos]

  if ("y" %in% neg) {
    ysign <- -1
  } else {
    ysign <- 1
  }
  if ("x" %in% neg) {
    xsign <- -1
    hjust <- 1
  } else {
    xsign <- 1
    hjust <- 0
  }

  args <- list(
    data = function(d) {
      dplyr::filter(
        .data = d,
        dplyr::across(dplyr::all_of(pos), ~ . >= 0),
        dplyr::across(dplyr::all_of(neg), ~ . < 0),
        .data[[label_var]]
      )
    },
    nudge_x = xsign * x_nudge,
    nudge_y = ysign * y_nudge,
    hjust = hjust
  )

  # append/overwrite args with extra args
  dots <- list(...)
  args[names(dots)] <- dots

  p <- p + do.call(what = fun, args = args)
  return(p)
}


# helper for plotting labels on non-circular taxatree plots/keys
taxatree_plot_labelsNotCircular <- function(p,
                                            taxon_renamer,
                                            fun = ggrepel::geom_text_repel,
                                            label_var,
                                            ...) {
  args <- list(
    data = function(d) {
      dplyr::filter(.data = d, .data[[label_var]])
    }
  )

  # append/overwrite args with extra args
  dots <- list(...)
  args[names(dots)] <- dots

  p <- p + do.call(what = fun, args = args)
  return(p)
}
