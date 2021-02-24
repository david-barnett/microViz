#' Plot taxonomic stats on a ggraph tree (and draw key)
#'
#' - Uses a phyloseq object to make a tree graph structure from the taxonomic table.
#' - Then adds statistical results, i.e. the output out taxatree_models (from the same phyloseq).
#' - Arguments `colour_stat` and (optionally) `sig_var` must be variables that can be extracted from the model objects in `models_list`.
#' - `taxatree_plotkey` plots same layout as `taxatree_plots`, but in a fixed colour
#'
#' Uses ggraph (see help for main underlying graphing function with ?ggraph::ggraph)
#'
#' @param ps phyloseq object
#' @param models_list list of lists of models, as output by taxatree_models
#' @param var_selection optionally only plot trees for this selection of predictor variables
#' @param var_renamer function to rename variables (called AFTER any var selection)
#' @param size_stat name of variable to scale size of nodes and edges (natural log scale)
#' @param colour_stat name of variable to scale colour/fill of nodes and edges (natural log scale)
#' @param max_node_size set this to avoid overlapping nodes at different scales
#' @param max_edge_width max edge width (optionally relative to max node size)
#' @param sig_var optional name of variable used to define statistical significance (outlines significant nodes) - NULL for no outlines
#' @param sig_threshold value of sig_var below which statistical significance outlines are drawn
#' @param sig_stroke_width fixed width of statistical significance node outlines
#' @param colour_palette any diverging hcl colour palette name see: `colorspace::hcl_palettes("diverging", n = 7, plot = TRUE)`
#' @param reverse_colours reverse direction of colour scale?
#' @param luminance_l2 luminance of colourscale midpoint (higher is lighter)
#' @param colour_lims limits of colour and fill scale, used to harmonise across several plots (values squished to fit this range!)
#' @param layout name of ggraph layout or manually specified layout with data.frame of x and y coordinates
#' @param drop_tax_levels remove taxonomic levels from tree plots if stats are not available for them from models list
#' @param add_circles draw grey background circles, 1 circle per taxonomic rank plotted
#'
#' @return ggraph ggplot (or list of)
#'
#' @examples
#' # corncob stats testing
#' library(dplyr)
#' library(microbiome)
#' library(corncob)
#' library(patchwork) # for arranging grid of plots
#'
#' data(dietswap)
#' ps <- dietswap
#'
#' # create some binary variables for easy visualisation
#' ps <- ps %>% ps_mutate(
#'   female = if_else(sex == "female", 1, 0, NaN),
#'   african = if_else(nationality == "AFR", 1, 0, NaN),
#'   overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
#'   obese = if_else(bmi_group == "obese", 1, 0, NaN)
#' )
#'
#' # This example dataset has some taxa with the same name for phylum and family...
#' # We can fix problems like this with the tax_prepend_ranks function
#' ps <- tax_prepend_ranks(ps)
#'
#' # filter out rare taxa
#' ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
#'
#' # specify variables used for modelling
#' models <- taxatree_models(ps, tax_levels = 1:3, formula = ~ female + african, verbose = FALSE)
#' plots <- taxatree_plots(ps, models_list = models, max_node_size = 4)
#' patchwork::wrap_plots(plots, guides = "collect")
#' key <- taxatree_plotkey(ps, taxon_renamer = function(x) stringr::str_remove(x, "^F: "))
#' key
#'
#' # cowplot is easiest for arranging trees and key and colourbar legend
#' colourbar <- cowplot::get_legend(plots[[1]])
#' keyrow <- cowplot::plot_grid(key, colourbar, rel_widths = c(9, 1))
#' plot_col <- cowplot::plot_grid(
#'   plots[[1]] + theme(legend.position = "none"),
#'   plots[[2]] + theme(legend.position = "none"),
#'   ncol = 1
#' )
#' cowplot::plot_grid(keyrow, plot_col, nrow = 1, rel_widths = c(2, 1))
#'
#' models_lm <- ps %>%
#'   microbiome::transform("compositional") %>%
#'   taxatree_models(type = "lm", tax_levels = 1:3, formula = ~ female + african, verbose = FALSE)
#' plots_lm <- taxatree_plots(ps, models_lm, max_node_size = 5)
#' patchwork::wrap_plots(plots_lm, nrow = 1, guides = "collect")
#' key
#' @export
#' @rdname taxatree_plots
taxatree_plots <- function(
                           ps,
                           models_list,
                           var_selection = NULL,
                           var_renamer = function(x) identity(x),
                           size_stat = "taxon_mean",
                           colour_stat = NULL,
                           max_node_size = 5,
                           max_edge_width = round(max_node_size * 3 / 4),
                           sig_var = TRUE,
                           sig_threshold = 0.05,
                           sig_stroke_width = 1.5,
                           colour_palette = "Green-Brown",
                           reverse_colours = FALSE,
                           luminance_l2 = 80,
                           colour_lims = NULL,
                           layout = "tree",
                           drop_tax_levels = TRUE,
                           add_circles = TRUE) {

  # check name are in rank names
  if (any(!names(models_list) %in% phyloseq::rank_names(ps))) {
    stop(
      "Did you use taxatree_models to generate models list?\n",
      "The following names in the models_list are not in rank_names of ps:\n",
      paste(names(models_list)[!names(models_list) %in% phyloseq::rank_names(ps)], collapse = "\n")
    )
  }
  # handle defaults for styling particular classes of model results
  if (inherits(models_list[[1]][[1]], "bbdml")) {
    if (identical(NULL, colour_stat)) colour_stat <- "b_mu"
    if (isTRUE(sig_var)) sig_var <- "p_mu"
  } else {
    if (identical(NULL, colour_stat)) colour_stat <- "estimate"
    if (isTRUE(sig_var)) sig_var <- "p.value"
  }

  # convert models to stats and split
  modelstats_list <- lapply(models_list, models2stats)
  # TODO allow computing an adjusted p.value variable (adjustment per rank)
  # modelstats_list <- lapply(..... ?)

  stats_df <- purrr::reduce(modelstats_list, rbind.data.frame)
  var_stats <- split.data.frame(stats_df, stats_df[["model_var"]])

  # get full range of colour stat variable
  if (identical(colour_lims, NULL)) {
    colour_var <- purrr::map_dbl(var_stats, .f = ~ max(abs(.x[[colour_stat]]), na.rm = TRUE))
    colour_lims <- c(-max(colour_var) * 1.25, max(colour_var) * 1.25)
  }

  # make a selection of variables for plotting, if requested
  if (!identical(var_selection, NULL)) {
    var_stats <- var_stats[var_selection]
  }

  # rename vars optionally
  names(var_stats) <- var_renamer(names(var_stats))

  # check if there is more than one value in top level
  # if so, add a root level
  if (length(unique(unclass(phyloseq::tax_table(ps))[, 1])) > 1) {
    phyloseq::tax_table(ps) <- cbind(root = "root", phyloseq::tax_table(ps))
  }

  # keep only tax levels present in the models lists
  # (and always first level, as must be rooted tree)
  if (isTRUE(drop_tax_levels)) {
    kept_ranks <- union(phyloseq::rank_names(ps)[[1]], names(models_list))
    phyloseq::tax_table(ps) <- phyloseq::tax_table(ps)[, kept_ranks]
  }
  # create nodes dataframe from phyloseq
  nodes_df <- taxatree_nodes(ps)

  plots_list <- lapply(
    X = names(var_stats),
    FUN = function(var) {
      df <- var_stats[[var]]
      # create a tibble graph for each variable
      nodes_df <- dplyr::left_join(x = nodes_df, y = df, by = "taxon_name")
      edge_df <- taxatree_edges(nodes_df = nodes_df)
      graph <- tidygraph::tbl_graph(nodes = nodes_df, edges = edge_df, node_key = "taxon_name", directed = TRUE)

      if (inherits(layout, "character")) {
        layout <- ggraph::create_layout(graph = graph, layout = layout, circular = TRUE)
      }

      # create plots from graphs
      p <- ggraph::ggraph(graph = layout)

      if (isTRUE(add_circles)) {
        p <- add_circles(p = p, layout = layout)
      }

      p <- p +
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
            color = .data[[colour_stat]],
            fill = .data[[colour_stat]]
          ),
          shape = "circle filled"
        ) +
        ggplot2::ggtitle(label = var)

      if (isFALSE(is.null(sig_var))) {
        # outlined circles for significance
        p <- p + ggraph::geom_node_point(
          data = ~ dplyr::filter(.x, .data[[sig_var]] < sig_threshold),
          mapping = ggplot2::aes(
            size = log(.data[[size_stat]]),
            fill = .data[[colour_stat]],
            color = dplyr::if_else(
              condition = .data[[colour_stat]] < 0,
              true = colour_lims[[1]],
              false = colour_lims[[2]]
            )
          ),
          stroke = sig_stroke_width,
          shape = "circle filled"
        )
      }
      # central black node for root
      p <- p +
        ggraph::geom_node_point(
          mapping = ggplot2::aes(
            filter = .data[["taxon_level"]] %in% c("Kingdom", "kingdom", "Root", "root"),
            size = log(.data[[size_stat]])
          ),
          colour = "black",
          shape = "circle"
        )

      # styling
      p <- p +
        ggplot2::scale_size_continuous(range = c(max(max_node_size/3, 0.2), max_node_size), guide = "none") +
        ggraph::scale_edge_width_continuous(range = c(max(max_edge_width/3, 0.2), max_edge_width), guide = "none") +

        # previously colorspace block was THROWING ERRORS about can't find objects specified in its args... when used INSIDE function:
        # solution, bizarrely, was just to use scale_colour_continuous_diverging (with aesthetics = "fill")
        # instead of scale_fill_continuous_diverging, which fails to find the args, seemingly given the resulting location of the do.call(parent.env()) call....???

        colorspace::scale_color_continuous_diverging(
          palette = colour_palette,
          l2 = luminance_l2,
          aesthetics = c("edge_colour", "fill", "colour"),
          limits = colour_lims,
          oob = scales::oob_squish,
          rev = reverse_colours,
          trans = abs_sqrt(),
          guide = ggplot2::guide_colorbar(
            frame.colour = "black",
            ticks.colour = "black" # ,
            # barwidth = grid::unit(0.01, "npc")
          )
        ) +
        ggraph::theme_graph(
          base_family = "sans",
          title_size = 10,
          plot_margin = grid::unit(x = rep(0.03, 4), "npc")
        )

      # trick to set new coordinates as "default", see: https://github.com/tidyverse/ggplot2/issues/2799
      cf <- ggplot2::coord_fixed(expand = FALSE, clip = "off")
      cf$default <- TRUE
      p <- p + cf
      return(p)
    }
  )
  names(plots_list) <- names(var_stats)
  return(plots_list)
}

#' @param colour fixed colour of points and edges
#'
#' @param label names of taxonomic ranks at which to label taxa
#' @param tax_levels names of tax ranks to include in plot
#' @param label_style list to style labels: passed as arguments to geom_label_repel()
#' @param taxon_renamer function to rename taxa when labelling (e.g. removing g__ etc.)
#'
#' @rdname taxatree_plots
#' @export
taxatree_plotkey <- function(
                             ps,
                             size_stat = "taxon_mean",
                             colour = "grey",
                             max_node_size = 8,
                             max_edge_width = round(max_node_size * 3 / 4),
                             layout = "tree",
                             label = utils::tail(phyloseq::rank_names(ps), 2)[1],
                             label_style = list(size = 2.5, alpha = 0.8),
                             taxon_renamer = function(x) identity(x),
                             tax_levels = phyloseq::rank_names(ps),
                             add_circles = TRUE) {

  # get tax_table values at desired ranks, to filter taxa for labelling
  taxa_to_label <- unique(as.character(unclass(phyloseq::tax_table(ps)[, label])))

  # check if there is more than one value in top level
  # if so, add a root level
  if (length(unique(unclass(phyloseq::tax_table(ps))[, 1])) > 1) {
    phyloseq::tax_table(ps) <- cbind(root = "root", phyloseq::tax_table(ps))
    tax_levels <- union("root", tax_levels)
  }

  # keep only tax levels specified (and root)
  phyloseq::tax_table(ps) <- phyloseq::tax_table(ps)[, tax_levels]

  # create directed graph from phyloseq
  nodes_df <- taxatree_nodes(ps)
  edge_df <- taxatree_edges(nodes_df = nodes_df)
  graph <- tidygraph::tbl_graph(nodes = nodes_df, edges = edge_df, node_key = "taxon_name", directed = TRUE)

  if (inherits(layout, "character")) {
    layout <- ggraph::create_layout(graph = graph, layout = layout, circular = TRUE)
  }

  # create plot from graph
  p <- ggraph::ggraph(graph = layout)

  if (isTRUE(add_circles)) {
    p <- add_circles(p = p, layout = layout)
  }

  p <- p +
    ggraph::geom_edge_link(
      mapping = ggplot2::aes(edge_width = log(.data[[size_stat]])),
      edge_colour = colour, alpha = 0.7,
      show.legend = FALSE
    ) +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(size = log(.data[[size_stat]])),
      fill = colour, colour = colour,
      shape = "circle filled",
      show.legend = FALSE
    ) +
    ggplot2::coord_fixed(expand = FALSE, clip = "off") +
    ggraph::theme_graph(
      base_family = "sans",
      plot_margin = grid::unit(x = rep(0.03, 4), "npc")
    ) +
    ggplot2::ggtitle(label = "KEY") +
    ggplot2::scale_size_continuous(range = c(max(max_node_size/3, 0.2), max_node_size)) +
    ggraph::scale_edge_width_continuous(range = c(max(max_edge_width/3, 0.1), max_edge_width))

  # add a black central node to mark the rook
  p <- p + ggraph::geom_node_point(
    mapping = ggplot2::aes(
      filter = .data[["taxon_level"]] == "root",
      size = log(.data[[size_stat]])
    ),
    show.legend = FALSE
  )

  # setting labels
  label_args <- list(
    data = ~ dplyr::filter(., .data[["taxon_name"]] %in% taxa_to_label),
    mapping = ggplot2::aes(
      x = .data[["x"]], y = .data[["y"]],
      label = taxon_renamer(.data[["taxon_name"]]),
    ),
    xlim = c(-Inf, Inf),
    min.segment.length = 0,
    show.legend = FALSE
  )
  # set user values
  label_args[names(label_style)] <- label_style
  # add labels
  p <- p + do.call(ggrepel::geom_label_repel, args = label_args)

  return(p)
}

# helper function for colour scale
abs_sqrt <- function() {
  scales::trans_new(
    name = "abs_sqrt",
    transform = function(x) {
      sign(x) * sqrt(abs(x))
    },
    inverse = function(x) {
      sign(x) * x^2
    }
  )
}

# helper function for drawing circles
add_circles <- function(p, layout) {
  # find radii of circles
  # dividing by 2 seems to be necessary for grob drawing
  radii <- sqrt(layout[["x"]]^2 + layout[["y"]]^2) / 2
  radii <- unique(round(radii, digits = 5))
  radii <- radii[radii != 0]
  # message(paste(radii, collapse = " "))

  # add background circles
  for (r in radii) {
    p <- p + ggplot2::annotation_custom(
      grid::circleGrob(r = r, gp = grid::gpar(fill = NA, col = "grey80", lwd = 0.3))
    )
  }
  return(p)
}