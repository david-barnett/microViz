#' Plot statistical model results for all taxa on a taxonomic tree
#'
#' - Uses a psExtra object to make a tree graph structure from the taxonomic table.
#' - Then adds statistical results stored in "taxatree_stats" of psExtra data
#' - You must use `taxatree_models()` first to generate statistical model results.
#' - You can adjust p-values with `taxatree_stats_p_adjust()`
#'
#' `taxatree_plotkey` plots same layout as `taxatree_plots`, but in a fixed colour
#'
#' See website article for more examples of use:
#' https://david-barnett.github.io/microViz/articles/web-only/modelling-taxa.html
#'
#' @details
#' Uses ggraph, see help for main underlying graphing function with `?ggraph::ggraph`
#'
#' It is possible to provide multiple significance markers for multiple thresholds,
#' by passing vectors to the sig_shape, sig_threshold, etc. arguments.
#' It is critically important that the thresholds are provided in decreasing
#' order of severity, e.g. sig_threshold = c(0.001, 0.01, 0.1) and you must provide
#' a shape value for each of them.
#'
#' @param data psExtra with taxatree_stats, e.g. output of `taxatree_models2stats()`
#' @param colour_stat name of variable to scale colour/fill of nodes and edges
#' @param palette
#' diverging hcl colour palette name from `colorspace::hcl_palettes("diverging")`
#' @param reverse_palette reverse direction of colour palette?
#' @param colour_lims
#' limits of colour and fill scale, NULL will infer lims from range of all data
#' @param colour_oob
#' scales function to handle colour_stat values outside of colour_lims
#' (default simply squishes "out of bounds" values into the given range)
#' @param colour_trans
#' name of transformation for colour scale:
#' default is "abs_sqrt", the square-root of absolute values,
#' but you can use the name of any transformer from the `scales` package,
#' such as "identity" or "exp"
#' @param size_stat
#' named list of length 1, giving function calculated for each taxon,
#' to determine the size of nodes (and edges). Name used as size legend title.
#' @param node_size_range min and max node sizes, decrease to avoid node overlap
#' @param edge_width_range min and max edge widths
#' @param size_guide
#' guide for node sizes, try "none", "legend" or ggplot2::guide_legend()
#' @param size_trans
#' transformation for size scale
#' you can use (the name of) any transformer from the scales package,
#' such as "identity", "log1p", or "sqrt"
#' @param sig_stat name of variable indicating statistical significance
#' @param sig_threshold
#' value of sig_stat variable indicating statistical significance (below this)
#' @param sig_shape fixed shape for significance marker
#' @param sig_size fixed size for significance marker
#' @param sig_stroke fixed stroke width for significance marker
#' @param sig_colour
#' fixed colour for significance marker (used as fill for filled shapes)
#' @param edge_alpha fixed alpha value for edges
#' @param vars
#' name of column indicating terms in models (one plot made per term)
#' @param var_renamer function to rename variables for plot titles
#' @param title_size font size of title
#' @param layout any ggraph layout, default is "tree"
#' @param layout_seed
#' any numeric, required if a stochastic igraph layout is named
#' @param circular should the layout be circular?
#' @param add_circles
#' add faint concentric circles to plot, behind each rank?
#' @param node_sort
#' sort nodes by "increasing" or "decreasing" size? NULL for no sorting.
#' Use `tax_sort()` before `taxatree_plots()` for finer control.
#' @param drop_ranks drop ranks from tree if not included in stats dataframe
#' @param colour_na
#' colour for NA values in tree.
#' (if unused ranks are not dropped, they will have NA values for colour_stat)
#' @param l1 Luminance value at the scale endpoints, NULL for palette's default
#' @param l2 Luminance value at the scale midpoint, NULL for palette's default
#'
#' @seealso [taxatree_models()] to calculate statistical models for each taxon
#' @seealso [taxatree_plotkey()] to plot the corresponding labelled key
#' @seealso [taxatree_plot_labels()] and [taxatree_label()] to add labels
#' @seealso [taxatree_stats_p_adjust()] to adjust p-values
#'
#' @return list of ggraph ggplots
#' @export
#'
#' @examples
#' # Limited examples, see website article for more
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' data(dietswap, package = "microbiome")
#' ps <- dietswap
#'
#' # create some binary variables for easy visualisation
#' ps <- ps %>% ps_mutate(
#'   female = if_else(sex == "female", 1, 0, NaN),
#'   african = if_else(nationality == "AFR", 1, 0, NaN)
#' )
#'
#' # This example dataset has some taxa with the same name for phylum and family...
#' # We can fix problems like this with the tax_prepend_ranks function
#' # (This will always happen with Actinobacteria!)
#' ps <- tax_prepend_ranks(ps)
#'
#' # filter out rare taxa
#' ps <- ps %>% tax_filter(
#'   min_prevalence = 0.5, prev_detection_threshold = 100
#' )
#'
#' # delete the Family rank as we will not use it for this small example
#' # this is necessary as taxatree_plots can only plot consecutive ranks
#' ps <- ps %>% tax_mutate(Family = NULL)
#'
#' # specify variables used for modelling
#' models <- taxatree_models(
#'   ps = ps, type = corncob::bbdml, ranks = c("Phylum", "Genus"),
#'   formula = ~ female + african, verbose = TRUE
#' )
#' # models list stored as attachment in psExtra
#' models
#'
#' # get stats from models
#' stats <- taxatree_models2stats(models, param = "mu")
#' stats
#'
#' plots <- taxatree_plots(
#'   data = stats, colour_trans = "identity",
#'   size_stat = list(mean = mean),
#'   size_guide = "legend", node_size_range = c(1, 6)
#' )
#'
#' # if you change the size_stat for the plots, do the same for the key!!
#' key <- taxatree_plotkey(
#'   data = stats,
#'   rank == "Phylum" | p.value < 0.05, # labelling criteria
#'   .combine_label = all, # label only taxa where criteria met for both plots
#'   size_stat = list(mean = mean),
#'   node_size_range = c(2, 7), size_guide = "none",
#'   taxon_renamer = function(x) {
#'     stringr::str_remove_all(x, "[PG]: | [ae]t rel.")
#'   }
#' )
#'
#' # cowplot is powerful for arranging trees and key and colourbar legend
#' legend <- cowplot::get_legend(plots[[1]])
#' plot_col <- cowplot::plot_grid(
#'   plots[[1]] + theme(legend.position = "none"),
#'   plots[[2]] + theme(legend.position = "none"),
#'   ncol = 1
#' )
#' cowplot::plot_grid(key, plot_col, legend, nrow = 1, rel_widths = c(4, 2, 1))
taxatree_plots <- function(data,
                           colour_stat = "estimate",
                           palette = "Green-Brown",
                           reverse_palette = FALSE,
                           colour_lims = NULL,
                           colour_oob = scales::oob_squish,
                           colour_trans = "abs_sqrt",
                           size_stat = list(prevalence = prev),
                           node_size_range = c(1, 4),
                           edge_width_range = node_size_range * 0.8,
                           size_guide = "legend",
                           size_trans = "identity",
                           sig_stat = "p.value",
                           sig_threshold = 0.05,
                           sig_shape = "circle filled",
                           sig_size = 0.75,
                           sig_stroke = 0.75,
                           sig_colour = "white",
                           edge_alpha = 0.7,
                           vars = "term",
                           var_renamer = identity,
                           title_size = 10,
                           layout = "tree",
                           layout_seed = NA,
                           circular = identical(layout, "tree"),
                           node_sort = NULL,
                           add_circles = isTRUE(circular),
                           drop_ranks = TRUE,
                           l1 = if (palette == "Green-Brown") 10 else NULL,
                           l2 = if (palette == "Green-Brown") 85 else NULL,
                           colour_na = "grey35") {
  rlang::check_installed(c("tidygraph", "ggraph"), "to use taxatree_plots")

  check_is_psExtra(data, argName = "data")
  # get variable-specific stats for joining to node data
  stats <- data@taxatree_stats
  taxatree_plots_statsCheck(
    stats = stats, vars = vars, colour_stat = colour_stat, sig_stat = sig_stat
  )

  terms <- unique(stats[[vars]])

  # make basic nodes
  if (isTRUE(drop_ranks)) {
    ranks <- unique(data@taxatree_stats[["rank"]])
  } else {
    ranks <- "all"
  }

  treeNodes <- taxatree_nodes(
    ps = data, fun = size_stat, .sort = node_sort,
    ranks = ranks, .use_counts = TRUE
  )

  # return list of ggraph layout dataframes for trees (one per variable)
  var_layouts <- lapply(
    X = terms,
    FUN = function(v) {
      # add variable-specific statistics/variables
      var_stats <- dplyr::filter(stats, .data$term == v)
      sharedColumns <- intersect(colnames(treeNodes), colnames(var_stats))
      treeNodes <- dplyr::left_join(
        x = treeNodes, y = var_stats, by = sharedColumns
      )
      # create edge data, graph, and graph layout
      # (graph stored as attribute on layout object, which allows edge styling)
      treeEdges <- taxatree_edges(treeNodes)
      treeGraph <- tidygraph::tbl_graph(
        nodes = treeNodes, edges = treeEdges, node_key = "taxon", directed = TRUE
      )
      # set seed if requested, for stochastic layouts
      if (!identical(layout_seed, NA)) set.seed(layout_seed)
      layout <- ggraph::create_layout(
        graph = treeGraph, layout = layout, circular = circular
      )
      return(layout)
    }
  )

  # calculate colour limits if null, making them symmetrical around zero
  if (identical(colour_lims, NULL)) {
    colour_lims <- range(stats[[colour_stat]])
    colour_lims <- c(-max(abs(colour_lims)), max(abs(colour_lims)))
  }

  var_plots <- lapply(
    X = terms,
    FUN = function(name) {
      l <- var_layouts[[name]]
      p <- taxatree_plot_layout(
        layout_df = l, size_stat = names(size_stat)[[1]],
        colour_stat = colour_stat, edge_alpha = edge_alpha,
        var_name = var_renamer(name), add_circles = add_circles
      )
      # add significance markings
      p <- taxatree_plot_sig(
        p = p, colour_stat = colour_stat, palette = palette, l1 = l1, l2 = l2,
        sig_stat = sig_stat, sig_threshold = sig_threshold,
        sig_stroke = sig_stroke, sig_shape = sig_shape, sig_size = sig_size,
        sig_colour = sig_colour
      )

      p <- taxatree_plotSizeScaling(
        p = p, node_size_range = node_size_range, node_size_guide = size_guide,
        edge_width_range = edge_width_range, size_trans = size_trans
      )
      p <- taxatree_plotColourScaling(
        p = p, palette = palette, reverse_palette = reverse_palette,
        colour_lims = colour_lims, colour_oob = colour_oob,
        colour_trans = colour_trans, colour_na = colour_na, l1 = l1, l2 = l2
      )
      p <- taxatree_plot_styling(
        p = p, circular = circular, title_size = title_size
      )
      return(p)
    }
  )

  names(var_plots) <- terms
  return(var_plots)
}

# helper, creates basic tree plot with variable data
taxatree_plot_layout <- function(layout_df,
                                 var_name,
                                 size_stat,
                                 colour_stat,
                                 edge_alpha = 0.7,
                                 add_circles = TRUE) {
  # create plots from graphs
  p <- ggraph::ggraph(graph = layout_df)
  if (isTRUE(add_circles)) {
    p <- taxatree_plotCircles(p = p, layout = layout_df)
  }
  p <- p +
    ggraph::geom_edge_link(
      mapping = ggplot2::aes(
        edge_colour = .data[[colour_stat]],
        edge_width = .data[[size_stat]]
      ),
      alpha = edge_alpha
    ) +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        size = .data[[size_stat]],
        color = .data[[colour_stat]],
        fill = .data[[colour_stat]]
      ),
      shape = "circle filled"
    ) +
    ggplot2::ggtitle(label = var_name)

  p <- taxatree_plotRootNode(p = p, size_stat = size_stat)

  return(p)
}

# helper function for drawing circles in background
taxatree_plotCircles <- function(p, layout) {
  # find radii of circles
  radii <- sqrt(layout[["x"]]^2 + layout[["y"]]^2)
  radii <- unique(round(radii, digits = 5))
  radii <- radii[radii != 0]
  # message(paste(radii, collapse = " "))

  # add background circles
  for (r in radii) {
    x <- 0 + r * cos(seq(from = 0, to = 2 * pi, length.out = 100))
    y <- 0 + r * sin(seq(from = 0, to = 2 * pi, length.out = 100))
    cl <- "grey80"
    if (utils::packageVersion("ggplot2") < "3.4.0") {
      a <- ggplot2::annotate("path", x = x, y = y, color = cl, size = 0.1)
    } else {
      a <- ggplot2::annotate("path", x = x, y = y, color = cl, linewidth = 0.1)
    }
    p <- p + a
  }
  return(p)
}

# taxatree_plot_layout and taxatree_plotkey internal helper
taxatree_plotRootNode <- function(p, size_stat) {
  # central black node for root
  p <- p +
    ggraph::geom_node_point(
      mapping = ggplot2::aes(
        filter = .data[["rank"]] %in% c("Kingdom", "kingdom", "Root", "root"),
        size = .data[[size_stat]]
      ),
      colour = "black",
      shape = "circle"
    )
  return(p)
}

# helper, to add significance markings to some points
taxatree_plot_sig <- function(p,
                              colour_stat,
                              palette,
                              l1, l2,
                              sig_stat,
                              sig_threshold = 0.05,
                              sig_size = 0.75,
                              sig_stroke = 1,
                              sig_colour = "white",
                              sig_shape = "circle filled") {
  if (!identical(sig_stat, NULL)) {
    # convert shape numbers to character names for consistency
    sig_shape <- sapply(sig_shape, shape_number2name)

    # process significance point aesthetics into complete & equal length vecs
    sigNames <- paste(sig_stat, "<", sig_threshold)
    maxLength <- length(sigNames)
    if (length(sig_shape) != maxLength) {
      stop("you must provide one shape per significance level")
    }
    sig <- list(
      stat = sig_stat, threshold = sig_threshold, size = sig_size,
      stroke = sig_stroke, colour = sig_colour, shape = sig_shape
    )
    sig <- lapply(sig, rep_len, maxLength)
    sig <- lapply(sig, `names<-`, sigNames)

    # transpose to being a list per marker and update fill/col by shape type
    sig <- purrr::transpose(sig)
    sig <- lapply(sig, function(x) {
      if (x$shape %in% c(filled_shapes(), 21:25)) {
        x$fill <- x$colour
        x$colour <- "black"
      } else {
        x$fill <- "hotpink" # shouldn't ever show
      }
      return(x)
    })

    # draw the legend
    p <- taxatree_plot_sigLegend(p, sig)

    # stop early if nothing is significant for this variable
    if (all(p[["data"]][, sig_stat] >= max(sig_threshold), na.rm = TRUE)) {
      return(p)
    }

    # extremes of palette
    palArgs <- list(n = 11, palette = palette)
    if (!is.null(l1)) palArgs$l1 <- l1
    if (!is.null(l2)) palArgs$l2 <- l2
    pal <- do.call(colorspace::diverging_hcl, args = palArgs)

    plotdf <- p$data
    plotdf$filledPointCol <- dplyr::if_else(
      condition = plotdf[[colour_stat]] < 0,
      true = pal[[1]], false = pal[[11]]
    )
    sig <- purrr::transpose(sig) # don't unlist

    # identity which are significant at which level
    # levels must start with strictest!
    plotdf$sigName <- NA_character_
    plotdf$.COLOR <- NA
    plotdf$.SHAPE <- NA
    plotdf$.FILL <- NA
    plotdf$.SIZE <- NA
    plotdf$.STROKE <- NA

    for (i in seq_len(maxLength)) {
      yes <- is.na(plotdf$sigName) & plotdf[[sig$stat[[i]]]] < sig$threshold[[i]]
      yes[is.na(yes)] <- FALSE
      plotdf$sigName[yes] <- sigNames[[i]]
      # filled shape colours (for outline) are range extremes
      if (sig$shape[[i]] %in% c(filled_shapes(), 21:25)) {
        plotdf[yes, ".COLOR"] <- plotdf$filledPointCol[yes]
      } else {
        plotdf[yes, ".COLOR"] <- sig$colour[[i]] # unfilled shape colour fixed
      }
      plotdf[yes, ".SHAPE"] <- sig$shape[[i]]
      plotdf[yes, ".FILL"] <- sig$fill[[i]]
      plotdf[yes, ".SIZE"] <- sig$size[[i]]
      plotdf[yes, ".STROKE"] <- sig$stroke[[i]]
    }

    plotdf <- plotdf[!is.na(plotdf$sigName), , drop = FALSE]

    p <- p + ggplot2::geom_point(
      data = plotdf,
      mapping = ggplot2::aes(x = .data$x, y = .data$y),
      shape = plotdf$.SHAPE,
      size = plotdf$.SIZE,
      stroke = plotdf$.STROKE,
      fill = plotdf$.FILL,
      colour = plotdf$.COLOR
    )
  }
  return(p)
}

# helper for taxatree_plot_sig, to convert shape numbers to names
shape_number2name <- function(shape_number) {
  namedShapeNums <- c(
    `square open` = 0, `circle open` = 1, `triangle open` = 2,
    plus = 3, cross = 4, `diamond open` = 5, `triangle down open` = 6,
    `square cross` = 7, asterisk = 8, `diamond plus` = 9,
    `circle plus` = 10, star = 11, `square plus` = 12, `circle cross` = 13,
    `square triangle` = 14, `triangle square` = 14, square = 15,
    `circle small` = 16, triangle = 17, diamond = 18, circle = 19,
    bullet = 20, `circle filled` = 21, `square filled` = 22,
    `diamond filled` = 23, `triangle filled` = 24, `triangle down filled` = 25
  )

  # return or stop early if shape_number is character
  if (is.character(shape_number)) {
    if (shape_number %in% names(namedShapeNums)) {
      return(shape_number)
    }
    stop(shape_number, " is not a valid shape name", call. = FALSE)
  } else if (is.numeric(shape_number) && shape_number %in% 0:25) {
    shape_name <- names(namedShapeNums[namedShapeNums == shape_number])
  } else if (is.numeric(shape_number)) {
    stop(shape_number, " is not a valid shape number, must be from 0 to 25")
  } else {
    stop("shapes must be named or numbered, not class: ", class(shape_number))
  }

  return(shape_name)
}

# helper for taxatree_plot_sig, draws custom shape legend on plot p
# sigMarkerList is a list of length n, for n legend entries: each named item
# is itself a list of all the aesthetics for that entry, named correctly.
taxatree_plot_sigLegend <- function(p, sigMarkerList) {
  # increase size and stroke for legend
  sigLegend <- lapply(sigMarkerList, function(x) {
    x[c("stroke", "size")] <- lapply(x[c("stroke", "size")], `*`, 1.5)
    x$colour <- "black"
    return(x)
  })
  sigNames <- names(sigLegend)
  sigLegend <- purrr::transpose(sigLegend)
  sigLegend <- lapply(sigLegend, unlist)
  sig_shape <- sigLegend[["shape"]]

  # add a layer with nothing, just to give a shape legend
  invisibleData <- data.frame(.shape. = sig_shape)
  names(sig_shape) <- sig_shape # named for use as manual scale values
  p <- p +
    ggplot2::geom_point(
      data = invisibleData, x = 0, y = 0, alpha = 0,
      mapping = ggplot2::aes(shape = .data[[".shape."]]), show.legend = TRUE
    ) +
    ggplot2::scale_shape_manual(
      values = sig_shape, labels = sigNames,
      guide = ggplot2::guide_legend(
        title = NULL, order = 3, override.aes = c(sigLegend, list(alpha = 1))
      )
    )
  return(p)
}

# helper to set size scales
taxatree_plotSizeScaling <- function(p,
                                     node_size_range,
                                     node_size_guide,
                                     edge_width_range,
                                     size_trans) {
  # set size ranges
  p <- p +
    ggplot2::scale_size_continuous(
      range = node_size_range, guide = node_size_guide, trans = size_trans
    ) +
    ggraph::scale_edge_width_continuous(
      range = edge_width_range, guide = "none", trans = size_trans
    )
  return(p)
}

# helper to set colour scale
taxatree_plotColourScaling <- function(p,
                                       palette = "Green-Brown",
                                       reverse_palette = FALSE,
                                       colour_lims,
                                       colour_oob = scales::oob_squish,
                                       colour_trans = "abs_sqrt",
                                       colour_na = "grey35",
                                       l1 = NULL,
                                       l2 = NULL) {
  # get colour transformation function referenced by character
  if (identical(colour_trans, "abs_sqrt")) {
    colour_trans <- abs_sqrt
  } else {
    colour_trans <- utils::getFromNamespace(
      x = paste0(colour_trans, "_trans"), ns = "scales"
    )
  }

  args <- list(
    palette = palette,
    na.value = colour_na,
    aesthetics = c("edge_colour", "fill", "colour"),
    limits = colour_lims,
    oob = colour_oob,
    rev = reverse_palette,
    trans = colour_trans(),
    guide = ggplot2::guide_colourbar(
      order = 1,
      frame.colour = "black",
      ticks.colour = "black" # ,
      # barwidth = grid::unit(0.05, "npc")
    )
  )
  # add luminance range limits if not NULL
  if (!is.null(l1)) args["l1"] <- l1
  if (!is.null(l2)) args["l2"] <- l2

  # set colour scale
  p <- p + do.call(colorspace::scale_colour_continuous_diverging, args = args)
  # NOTE:
  # Previously colorspace block was THROWING ERRORS about
  # can't find objects specified in its args... when used INSIDE a function:
  # solution, bizarrely, was just to use scale_colour_continuous_diverging
  # (with aesthetics = "fill") instead of scale_fill_continuous_diverging
  # which fails to find the args, seemingly given the resulting location
  # of the do.call(parent.env()) call....???

  return(p)
}

# helper, to set theme and coordinates of tree_plots
taxatree_plot_styling <- function(p,
                                  circular,
                                  expand = FALSE,
                                  title_size = 10) {
  p <- p +
    ggraph::theme_graph(
      base_family = "sans",
      title_size = title_size,
      plot_margin = grid::unit(x = rep(0.03, 4), "npc")
    )

  if (isTRUE(circular)) {
    # trick to set new coordinates as "default"
    # see: https://github.com/tidyverse/ggplot2/issues/2799
    cf <- ggplot2::coord_fixed(expand = expand, clip = "off")
    cf$default <- TRUE
    p <- p + cf
  } else {
    p <- p + ggplot2::coord_cartesian(clip = "off", default = TRUE)
  }

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

# helper function checks presence of variables in taxatree_stats dataframe
taxatree_plots_statsCheck <- function(stats, vars, colour_stat, sig_stat) {
  if (identical(stats, NULL) || !inherits(stats, "data.frame")) {
    rlang::abort(call = rlang::caller_env(), message = c(
      "data must be a psExtra object with taxatree_stats data.frame attached",
      "*" = "Did you forget to run taxatree_models2stats first?"
    ))
  }
  # missing required taxon or rank columns?
  if (any(!c("taxon", "rank") %in% colnames(stats))) {
    missing <- setdiff(c("taxon", "rank"), colnames(stats))
    missing <- paste(missing, collapse = " & ")
    rlang::abort(call = rlang::caller_env(), message = c(
      "taxatree_stats df attached to data must have 'taxon' & 'rank' columns",
      i = paste("It is missing the column(s):", missing),
      i = "Did you make the data object with taxatree_models2stats?"
    ))
  }
  # check user-supplied arguments
  toCheck <- list(vars = vars, colour_stat = colour_stat)
  for (n in names(toCheck)) {
    arg <- toCheck[[n]]
    if (n != "sig_stat" && identical(arg, NULL)) { # sig_stat can be null
      rlang::abort(call = rlang::caller_env(), message = c(
        paste(n, "must be a column in the taxatree_stats data.frame, not NULL")
      ))
    }
    if (length(arg) != 1 || !inherits(arg, "character") || rlang::is_na(arg)) {
      rlang::abort(call = rlang::caller_env(), message = c(
        paste(n, "must be the name of a column in the taxatree_stats data.frame")
      ))
    }
    if (!arg %in% colnames(stats)) {
      rlang::abort(call = rlang::caller_env(), message = c(
        paste(n, "must be the name of a column in the taxatree_stats data.frame"),
        i = paste0("'", arg, "' is not in the taxatree_stats data.frame")
      ))
    }
  }
}
