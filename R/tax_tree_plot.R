#' Plot taxonomic stats on a ggraph tree
#'
#' Uses ggraph. See tree-development vignette.
#'
#' @param graph_object tidygraph made with tax_tree_nodes tax_tree_edges
#' @param layout_coords dataframe of x and y coordinates
#' @param size_stat name of variable to scale size of nodes and edges (natural log scale)
#' @param colour_stat name of variable to scale colour/fill of nodes and edges (natural log scale)
#' @param max_node_size set this to avoid overlapping nodes at different scales
#' @param max_edge_width max edge width (optionally relative to max node size)
#' @param sig_var name of variable used to define statistical significance (outlines significant nodes) - NULL for none
#' @param sig_threshold value of sig_var below which statistical significance outlines are drawn
#' @param sig_stroke_width fixed width of statistical significance node outlines
#' @param colour_palette any diverging hcl colour palette name see: `colorspace::hcl_palettes("diverging", n = 7, plot = TRUE)`
#' @param reverse_colours reverse direction of colour scale?
#' @param luminance_l2 luminance of colourscale midpoint (higher is lighter)
#' @param colour_lims limits of colour and fill scale, used to harmonise across several plots (values squished to fit this range!)
#'
#' @return ggraph ggplot
#' @export
#'
#' @examples
#' #
tax_tree_plot <- function(
                          graph_object,
                          layout_coords,
                          size_stat = "taxon_mean",
                          colour_stat = "b_mu",
                          max_node_size = 8,
                          max_edge_width = max_node_size - 2,
                          sig_var = "p_mu",
                          sig_threshold = 0.05,
                          sig_stroke_width = 1.5,
                          colour_palette = "Green-Brown", # currently not working inside function
                          reverse_colours = FALSE, # currently not working inside function
                          luminance_l2 = 80, # currently not working inside function
                          colour_lims = c(-2, 2) # currently partly not working inside function
) {
  p <- ggraph::ggraph(graph = graph_object, layout = layout_coords) +
    ggraph::geom_edge_link(
      mapping = ggplot2::aes(
        edge_width = log(.data[[size_stat]]),
        edge_colour = .data[[colour_stat]]
      ),
      alpha = 0.7
    ) +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        size = log(.data[[size_stat]]),
        color = .data[[colour_stat]]
      ),
      shape = "circle"
    )

  if (isFALSE(is.null(sig_var))) {
    # outlined circles for significance
    p <- p + ggraph::geom_node_point(
      data = ~ dplyr::filter(.x, .data[[sig_var]] < sig_threshold),
      mapping = ggplot2::aes(
        size = log(.data[[size_stat]]),
        color = dplyr::if_else(
          condition = .data[[colour_stat]] < 0,
          true = colour_lims[[1]],
          false = colour_lims[[2]]
        )
      ),
      stroke = sig_stroke_width,
      shape = "circle open"
    )
  }

  p <- p +
    # central black node for root
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = taxon_level == "root",
        size = log(.data[[size_stat]])
      )
    ) +
    ggplot2::scale_size_continuous(range = c(1, max_node_size), guide = "none") +
    ggraph::scale_edge_width_continuous(range = c(1, max_edge_width), guide = "none") +

    # colorspace block THROWING ERRORS about can't find objects... when used INSIDE function

    # colorspace::scale_fill_continuous_diverging(
    #   palette = colour_palette,
    #   l2 = luminance_l2,
    #   aesthetics = c("edge_colour", "fill"),
    #   limits = colour_lims,
    #   oob = scales::oob_squish,
    #   rev = reverse_colours,
    #   # trans = abs_sqrt,
    #   guide = guide_colorbar(
    #     frame.colour = "black",
    #     ticks.colour = "black",
    #     barwidth = grid::unit(0.01, "npc")
    #   )
    # ) +
    # # colour scale for sig. points stroke (which has no legend)
    # colorspace::scale_color_continuous_diverging(
    #   palette = colour_palette,
    #   l2 = luminance_l2,
    #   aesthetics = "color",
    #   rev = reverse_colours,
    #   # trans = abs_sqrt,
    #   guide = "none"
    # ) +
    ggplot2::coord_fixed(expand = FALSE, clip = "off") +
    ggraph::theme_graph(
      base_family = "sans",
      plot_margin = grid::unit(x = rep(0.03, 4), "npc")
    ) +
    NULL

  return(p)
}
