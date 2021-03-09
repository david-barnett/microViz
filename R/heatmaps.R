#' @title Microbe-to-sample-data correlation heatmap
#'
#' @description Plot correlations between (transformed) microbial abundances and (selected) numeric-like sample_data variables from a phyloseq object.
#'
#' @details Using a data.frame for the data argument is also possible, in which case the (selected) numeric-like variables will be correlated which each other,
#' and all arguments relating to taxa will be ignored.
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include, or NA for all
#' @param anno_tax optional annotation of taxa distributions: tax_anno() list output, or a pre-made ComplexHeatmap HeatmapAnnotation
#' @param vars selection of variable names from sample_data
#' @param anno_vars optional annotation of variable distributions: var_anno() list output, or a pre-made ComplexHeatmap HeatmapAnnotation
#' @param taxa_which row or column (which represents a taxon)
#' @param cor name of correlation method
#' @param cor_use passed to cor(use = cor_use)
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param tax_transform transformation applied to otu_table before correlating (and BEFORE selection of taxa)
#' @param gridlines list output of heat_grid() for setting gridline style
#' @param ... extra args, for cor_heatmap passed to internal function viz_heatmap (for heat_numbers() dots are passed to grid::gpar for grid::grid.text)
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#' # create a couple of numerical variables to use
#' psq <- dietswap %>%
#'   ps_mutate(
#'     weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'     female = if_else(sex == "female", true = 1, false = 0),
#'     african = if_else(nationality == "AFR", true = 1, false = 0)
#'   )
#' psq <- tax_filter(ps = psq, min_prevalence = 1 / 10, min_sample_abundance = 1 / 10)
#' psq <- tax_agg(psq, "Genus")
#'
#' # randomly select 30 taxa from the 50 most abundant taxa
#' set.seed(123)
#' taxa <- sample(microbiome::top_taxa(ps_get(psq))[1:50], size = 30)
#'
#' # make simple correlation heatmap with all numeric-like variables
#' p <- cor_heatmap(psq, taxa, anno_tax = tax_anno(undetected = 50))
#' # NOTE: detection threshold set to 50 as HITchip example data seems to have background noise
#' p
#'
#' # or with a selection of numeric-like variables
#' cor_heatmap(
#'   psq, taxa,
#'   vars = c("african", "female", "weight"), anno_tax = tax_anno(undetected = 50)
#' )
#'
#' # also with alternative correlation measures
#' cor_heatmap(psq, taxa, cor = "spearman", anno_tax = tax_anno(undetected = 50))
#'
#' # annotating variables is possible, and easy with var_anno() which defaults to a boxplot
#' cor_heatmap(psq, taxa, anno_vars = var_anno(), anno_tax = tax_anno(undetected = 50))
#'
#' # other and multiple annotations
#' cor_heatmap(
#'   psq, taxa,
#'   anno_tax = tax_anno(undetected = 50),
#'   anno_vars = var_anno(
#'     annos = c("var_hist", "var_box"),
#'     funs = list("identity", function(x) log10(x + 1)),
#'     names = c("x", "log10(x+1)"), rel_sizes = c(1, 2)
#'   )
#' )
#'
#' # optionally you can set the absolute width and height of the heatmap body manually
#' p1 <- cor_heatmap(psq, taxa,
#'   anno_tax = tax_anno(undetected = 50),
#'   width = grid::unit(5, "cm"), height = grid::unit(15, "cm")
#' )
#' p1
#'
#' # you can also change the annotation sizes
#' p2 <- cor_heatmap(psq, taxa,
#'   anno_tax = tax_anno(undetected = 50, size = 45, rel_sizes = c(1, 2)),
#'   width = grid::unit(5, "cm"), height = grid::unit(15, "cm")
#' )
#' p2
#'
#' # make the same correlation heatmap, but rotated
#' p3 <- cor_heatmap(
#'   psq, taxa,
#'   taxa_which = "column",
#'   anno_tax = tax_anno(undetected = 50, which = "column")
#' )
#' p3
#'
#' # customising annotation styles is possible using the args argument in tax_anno or var_anno
#' # but it is tricky: pass a list of lists, the inner lists are named to match the annotation type
#' # this approach might be simplified/changed in future versions
#' extra_args <- list(prev = list(gp = grid::gpar(fill = "white", lwd = 1), bar_width = 0.3))
#' cor_heatmap(
#'   psq, taxa,
#'   anno_tax = tax_anno(undetected = 50, prev = 1, abund = NA, args = extra_args)
#' )
#'
#' extra_args2 <- list(
#'   var_hist = list(gp = grid::gpar(fill = "black", lwd = 1)),
#'   var_box = list(point_size = 3, box_width = 0.9)
#' )
#' var_annotations <- var_anno(annos = c("var_hist", "var_box"), args = extra_args2)
#' cor_heatmap(psq, taxa, anno_vars = var_annotations, anno_tax = tax_anno(undetected = 50))
cor_heatmap <- function(data,
                        taxa = phyloseq::taxa_names(ps_get(data)),
                        anno_tax = tax_anno(),
                        vars = NA,
                        anno_vars = NULL,
                        cor = c("pearson", "kendall", "spearman")[1],
                        cor_use = "everything",
                        colors = heat_palette(palette = "Green-Orange", sym = TRUE),
                        numbers = heat_numbers(),
                        taxa_which = "row",
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method,
                        seriation_dist_col = seriation_dist,
                        tax_transform = "identity",
                        gridlines = heat_grid(lwd = 0.5),
                        ...) {
  if (inherits(data, "data.frame")) {
    otu_mat <- NULL # causes cor to only use x (meta_mat)
    meta_mat <- df_to_numeric_matrix(data, vars = vars)
  } else if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    ps <- ps_get(data)
    # handle otu_mat and taxa annotations if relevant
    if (identical(taxa, NULL)) {
      otu_mat <- NULL
    } else {
      # handle otu_table data
      otu <- otu_get(data)
      otu <- microbiome::transform(otu, transform = tax_transform)
      otu <- otu[, taxa, drop = FALSE]
      otu_mat <- unclass(otu)

      # create taxa annotation object if "instructions" given
      if (inherits(anno_tax, "list")) {
        anno_args <- c(anno_tax, list(data = ps, taxa = taxa))
        anno_tax <- do.call(what = taxAnnotation, args = anno_args)
      }
      # check anno_tax matches taxa_which
      if (!identical(anno_tax, NULL) &&
        methods::is(anno_tax, "HeatmapAnnotation") &&
        !identical(taxa_which, anno_tax@which)
      ) {
        stop("anno_tax annotation which arg should match taxa_which arg: both 'row' or both 'column'")
      }
    }
    # handle sample metadata
    samdat <- phyloseq::sample_data(ps)
    meta_mat <- df_to_numeric_matrix(samdat, vars = vars)
  } else {
    stop("data must be phyloseq, ps_extra, or data.frame, not: ", paste(class(data), collapse = " "))
  }

  # create variable annotation object if "instructions" given
  if (inherits(anno_vars, "list")) {
    anno_args <- c(anno_vars, list(data = data, vars = vars))
    anno_vars <- do.call(what = varAnnotation, args = anno_args)
  }
  # check anno_vars doesn't match taxa_which (if taxa are used/relevant)
  if (!inherits(data, "data.frame") &&
    !identical(taxa, NULL) &&
    !identical(anno_vars, NULL) &&
    methods::is(anno_vars, "HeatmapAnnotation") &&
    identical(taxa_which, anno_vars@which)
  ) {
    stop("anno_vars annotation which arg should NOT match taxa_which arg: one should be 'row' and the other 'column'")
  }

  # correlate datasets (x to y or just within x if y is NULL)
  cor_mat <- stats::cor(x = meta_mat, y = otu_mat, use = cor_use, method = cor)

  args <- list(
    colors = colors,
    numbers = numbers,
    seriation_method = seriation_method,
    seriation_dist = seriation_dist,
    seriation_method_col = seriation_method_col,
    seriation_dist_col = seriation_dist_col,
    column_names_rot = 45,
    column_dend_side = "bottom",
    column_names_side = "top",
    rect_gp = gridlines
  )

  args[["mat"]] <- cor_mat
  args[["numbers_mat"]] <- cor_mat # this differs in comp_heatmap (is there value in it being separable in this function?)

  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    # TODO allow bottom, top, left and right in addition to rows and columns
    if (identical(taxa_which, "row")) {
      args[["right_annotation"]] <- anno_tax
      args[["top_annotation"]] <- anno_vars
      args[["mat"]] <- t(args[["mat"]])
      args[["numbers_mat"]] <- t(args[["numbers_mat"]])
    } else if (identical(taxa_which, "column")) {
      args[["bottom_annotation"]] <- anno_tax
      args[["right_annotation"]] <- anno_vars
    } else {
      stop("taxa_which must be row or column, it is: ", taxa_which)
    }
  } else if (inherits(data, "data.frame") && !identical(anno_vars, NULL)) {
    if (anno_vars@which == "row") args[["right_annotation"]] <- anno_vars
    if (anno_vars@which == "column") args[["top_annotation"]] <- anno_vars
  }
  # use extra args (overwriting any set including with anno_tax [relevant if anno_tax = NULL])
  dots <- list(...)
  args[names(dots)] <- dots

  p <- do.call(viz_heatmap, args = args)
  p
}


#' Heatmap of taxonomic composition across samples
#'
#' Heatmap made with ComplexHeatmap, with optional (but default) annotation of taxa prevalence and abundance.
#' If seriated, always sorts by the values of the colours shown.
#' Any cell numbers printed can be set to different values, and do not affect ordering.
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include (selection occurs AFTER any tax_transform and scaling)
#' @param samples list of taxa to include (selection occurs AFTER any tax_transform and scaling)
#' @param anno_tax NULL or tax_anno() list output
#' @param anno_samples NULL only support so far TODO
#' @param taxa_which row or column (which represents a taxon)
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers NULL or output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param tax_transform_colors transformation applied to otu_table used for colours and sorting
#' @param tax_scale_colors scaling applied to otu_table after transformation
#' @param tax_transform_numbers transformation applied to otu_table used only for any numbers printed
#' @param tax_scale_numbers scaling applied to numbers otu_table after transformation
#' @param gridlines list output of heat_grid() for setting gridline style
#' @param name used as legend title (colourbar)
#' @param ... extra args, passed to viz_heatmap internal function
#'
#' @export
#' @examples
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#' psq <- tax_filter(dietswap, min_prevalence = 1 / 10, min_sample_abundance = 1 / 10)
#' psq <- tax_agg(psq, "Genus")
#'
#' set.seed(123)
#' taxa <- sample(microbiome::top_taxa(ps_get(psq))[1:50], size = 30)
#'
#' p <- comp_heatmap(data = psq, taxa = taxa, anno_tax = tax_anno(undetected = 50))
#' p
#'
#' # you can place the legend at the bottom, but it is a little complicated
#' p <- comp_heatmap(
#'   data = psq, taxa = taxa, anno_tax = tax_anno(undetected = 50),
#'   heatmap_legend_param = list(direction = "horizontal", title_position = "lefttop")
#' )
#' ComplexHeatmap::draw(p, heatmap_legend_side = "bottom", adjust_annotation_extension = FALSE)
#'
#' p2 <- comp_heatmap(
#'   psq,
#'   taxa = taxa,
#'   taxa_which = "column",
#'   anno_tax = tax_anno(undetected = 50, which = "column")
#' )
#' p2
comp_heatmap <- function(data,
                         taxa = phyloseq::taxa_names(ps_get(data)),
                         samples = phyloseq::sample_names(ps_get(data)),
                         anno_tax = NULL,
                         anno_samples = NULL,
                         colors = heat_palette(palette = "Greens", rev = TRUE),
                         numbers = NULL, # or list made by heat_numbers() or a function in format of a ComplexHeatmap cell_fun
                         tax_transform_colors = "clr",
                         tax_scale_colors = "neither",
                         taxa_which = "row",
                         # stuff just passed to viz_heatmap
                         seriation_method = "OLO_ward",
                         seriation_dist = "euclidean",
                         seriation_method_col = seriation_method,
                         seriation_dist_col = seriation_dist,
                         # numbers
                         tax_transform_numbers = tax_transform_colors,
                         tax_scale_numbers = tax_scale_colors,
                         gridlines = heat_grid(lwd = 0.1, col = "black"),
                         name = "mat",
                         ... # passed to viz_heatmap
) {
  ps <- ps_get(data)

  # create taxa annotation object if "instructions" given
  if (inherits(anno_tax, "list")) {
    anno_args <- c(anno_tax, list(data = ps, taxa = taxa))
    anno_tax <- do.call(what = taxAnnotation, args = anno_args)
  }
  # anno_tax suitable for taxa_which?
  if (!identical(anno_tax, NULL) && grepl("Annotation", x = class(anno_tax)[[1]])) {
    if (!identical(taxa_which, anno_tax@which)) stop("anno_tax annotation which arg should match taxa_which arg: both 'row' or both 'column'")
  }

  # handle otu_table data
  otu_mat <- otu_get(microbiome::transform(ps, transform = tax_transform_colors)) # used for colours and seriation
  otu_mat <- tax_scale(data = otu_mat, do = tax_scale_colors)
  otu_mat <- unclass(otu_mat[samples, taxa, drop = FALSE])
  otu_numbers <- otu_get(microbiome::transform(ps, transform = tax_transform_numbers)) # used for numbers only
  otu_numbers <- tax_scale(data = otu_numbers, do = tax_scale_numbers)
  otu_numbers <- unclass(otu_numbers[samples, taxa, drop = FALSE])

  args <- list(
    name = name,
    colors = colors,
    numbers = numbers,
    seriation_method = seriation_method,
    seriation_dist = seriation_dist,
    seriation_method_col = seriation_method_col,
    seriation_dist_col = seriation_dist_col,
    rect_gp = gridlines,
    column_names_rot = -45,
    column_dend_side = "top",
    column_names_side = "bottom",
    heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 8))
  )

  args[["mat"]] <- otu_mat
  args[["numbers_mat"]] <- otu_numbers

  # TODO allow bottom, top, left and right in addition to rows and columns
  if (identical(taxa_which, "row")) {
    args[["right_annotation"]] <- anno_tax
    args[["mat"]] <- t(args[["mat"]])
    args[["numbers_mat"]] <- t(args[["numbers_mat"]])
    args[["show_column_names"]] <- FALSE
  } else if (identical(taxa_which, "column")) {
    args[["bottom_annotation"]] <- anno_tax
    args[["show_row_names"]] <- FALSE
  } else {
    stop("taxa_which must be row or column, it is: ", taxa_which)
  }
  # use extra args (overwriting any set including with anno_tax [relevant if anno_tax = NULL])
  dots <- list(...)
  args[names(dots)] <- dots

  p <- do.call(viz_heatmap, args = args)
  p
}

#' @title Easy palettes for ComplexHeatmap
#'
#' @description Pass a named colorspace hcl palette to circlize::colorRamp2
#'
#' @param palette named palette from colorspace::hcl_palettes() diverging/sequential or a vector of colour names/hexcodes
#' @param breaks integer number of breaks, or numeric vector of values for colour scale breaks (including ends)
#' @param range NA to return palette generating function that takes a range, or numeric vector indicating the range, to return a palette
#' @param rev reverse the palette?
#' @param sym makes palette range symmetrical around 0 if TRUE
#'
#' @return circlize::colorRamp2 palette if !is.na(range), or function returning a palette when given a range
#' @export
#' @rdname heat_palette
heat_palette <- function(palette = "Greens", breaks = 5, range = NA, rev = FALSE, sym = FALSE) {
  n_breaks <- if (length(breaks) > 1) length(breaks) else breaks
  if (length(palette) == 1) {
    palette <-
      if (palette %in% rownames(colorspace::hcl_palettes(type = "diverging"))) {
        colorspace::diverge_hcl(palette = palette, n = n_breaks)
      } else if (palette %in% rownames(colorspace::hcl_palettes(type = "sequential"))) {
        colorspace::sequential_hcl(palette = palette, n = n_breaks)
      } else {
        stop("Invalid palette, try `colorspace::hcl_palettes()` and pick a diverging/sequential palette name")
      }
  } else if (length(palette) == 1) {
    stop("palette must be a vector of colours or a palette name from colorspace::hcl_palettes() diverging or sequential")
  }
  if (isTRUE(rev)) palette <- base::rev(palette)
  col_fun <- function(range) {
    if (isTRUE(sym)) range <- c(-max(abs(range)), max(abs(range)))
    breaks <- seq(from = range[[1]], to = range[[2]], length.out = n_breaks)
    circlize::colorRamp2(breaks = breaks, colors = palette)
  }
  if (identical(range, NA)) {
    return(col_fun)
  } else if (is.numeric(range)) {
    range <- base::range(range) # in case range set with e.g. -2:2 for convenience
    return(col_fun(range))
  }
}

#' @title Aesthetic settings for drawing numbers on heatmap tiles
#'
#' @description Works with comp_heatmap() and cor_heatmap(). See the help for those functions.
#'
#' @param decimals number of decimal places to print
#' @param fontsize fontsize specification,
#' @param col colour of font
#' @param fontface plain, bold, italic
#' @param fmt NULL or number print format, see ?sprintf, overrides decimals arg if set
#' @param ... passed to grid::gpar() for grid.text
#' @return list
#' @export
#' @rdname heat_numbers
heat_numbers <- function(decimals = 1, fontsize = 7, col = "black", fontface = "plain", fmt = NULL, ...) {
  if (identical(fmt, NULL)) fmt <- paste0("%.", decimals, "f")
  gp <- grid::gpar(fontsize = fontsize, col = col, fontface = fontface, ...)
  list(fmt = fmt, gp = gp)
}


#' @title set options for drawing gridlines on heatmaps
#'
#' @param col Colour for lines and borders.
#' @param alpha Alpha channel for transparency
#' @param lty Line type
#' @param lwd Line width
#' @param lex Multiplier applied to line width
#' @param lineend Line end style (round, butt, square)
#' @param linejoin Line join style (round, mitre, bevel)
#'
#' @export
#' @rdname heat_grid
heat_grid <- function(col = "white",
                      alpha = 1,
                      lty = 1,
                      lwd = 0.5,
                      lex = 1,
                      lineend = "round",
                      linejoin = "round") {
  grid::gpar(
    col = col,
    alpha = alpha,
    lty = lty,
    lwd = lwd,
    lex = lex,
    lineend = lineend,
    linejoin = linejoin
  )
}
