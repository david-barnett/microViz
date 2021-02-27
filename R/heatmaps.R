#' @title Microbe-to-sample-data correlation heatmap
#'
#' @description Correlate microbial abundances to numeric sample_data variables
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include, or NA for all
#' @param anno_tax taxAnnotation output describing taxa distribution or NULL
#' @param vars selection of variable names from sample_data
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
#' @param ... extra args, for cor_heatmap passed to viz_heatmap (for heat_numbers() dots are passed to grid::gpar for grid::grid.text)
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
cor_heatmap <- function(data,
                        taxa = phyloseq::taxa_names(ps_get(data)),
                        anno_tax = tax_anno(),
                        vars = NA,
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
  ps <- ps_get(data)

  # create taxa annotation object if "instructions" given
  if (inherits(anno_tax, "list")) {
    anno_args <- c(anno_tax, list(data = ps, taxa = taxa))
    anno_tax <- do.call(what = taxAnnotation, args = anno_args)
  }
  # check anno_tax matches taxa_which
  if (
    !identical(anno_tax, NULL) &&
      grepl("Annotation", x = class(anno_tax)[[1]]) &&
      !identical(taxa_which, anno_tax@which)
  ) {
    stop("anno_tax annotation which arg should match taxa_which arg: both 'row' or both 'column'")
  }


  # handle otu_table data
  otu <- otu_get(data)
  otu <- microbiome::transform(otu, transform = tax_transform)
  otu <- otu[, taxa, drop = FALSE]
  otu_mat <- unclass(otu)

  # handle sample metadata
  samdat <- phyloseq::sample_data(ps)
  meta_mat <- as.matrix.data.frame(samdat[, sapply(samdat, function(x) is.numeric(x) | is.logical(x) | is.integer(x))])

  if (!identical(vars, NA)) {
    stopifnot(is.character(vars))
    if (all(vars %in% colnames(meta_mat))) {
      meta_mat <- meta_mat[, vars, drop = FALSE]
    } else {
      stop(
        paste(vars[!vars %in% colnames(meta_mat)], collapse = " "), " is/are not valid variable names in the sample_data\n",
        "Possible numeric/integer/logical variables include:\n", paste(colnames(meta_mat), collapse = " ")
      )
    }
  }

  # correlate datasets
  cor_mat <- stats::cor(x = otu_mat, y = meta_mat, use = cor_use, method = cor)

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

  # TODO allow bottom, top, left and right in addition to rows and columns
  if (identical(taxa_which, "row")) {
    args[["right_annotation"]] <- anno_tax
  } else if (identical(taxa_which, "column")) {
    args[["bottom_annotation"]] <- anno_tax
    args[["mat"]] <- t(args[["mat"]])
    args[["numbers_mat"]] <- t(args[["numbers_mat"]])
  } else {
    stop("taxa_which must be row or column, it is: ", taxa_which)
  }
  # use extra args (overwriting any set including with anno_tax [relevant if anno_tax = NULL])
  dots <- list(...)
  args[names(dots)] <- dots

  p <- do.call(viz_heatmap, args = args)
  p
}


#' Heatmap of taxonomic composition across samples
#'
#' If seriated, always sorts by the values of the colours shown.
#' Any cell numbers printed can be set to different values, and do not affect ordering.
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include (selection occurs BEFORE any tax_transform)
#' @param samples list of taxa to include (selection occurs BEFORE any tax_transform)
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
#' @param tax_show selection of taxa to show on heatmap, AFTER transformation (and scaling) of taxa
#' @param samples_show selection of samples to show on heatmap, AFTER transformation (and scaling) of taxa
#' @param gridlines list output of heat_grid() for setting gridline style
#'
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
                         tax_show = taxa,
                         samples_show = samples,
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

#' @title helps for drawing numbers on heatmap tiles
#'
#' @param what what numbers to print (currently only "values" is supported)
#' @param fmt number print format
#' @param fontsize fontsize specification,
# @param ... passed to grid::gpar() for grid.text
#'
#' @export
#' @rdname heat_numbers
heat_numbers <- function(decimals = 1, fmt = "%.1f", fontsize = 7, col = "black", fontface = "plain", ...) {
  fmt <- paste0("%.", decimals, "f")
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

#' @title Create list describing taxa annotation
#' @description  main usefulness of tax_anno function is to give sane defaults and prompt user which annotation options are available
#'
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which "row" or "column" annotation?
#' @param prev order in which prevalence annotation shown (number, or NA)
#' @param abund order in which abundance annotation shown (number, or NA)
#' @param size total size of all annotations in mm, excluding gaps (used as width or height, depending on value of which)
#' @param gap size of gap between annotations (in mm)
#' @param rel_sizes relative sizes of non-NA annotations (in given order)
#' @param ... further args passed HeatmapAnnotation
#'
#' @export
#' @rdname heatmap-annotations
tax_anno <- function(undetected = 0, which = "row", prev = 1, abund = 2, size = 30, gap = 2, rel_sizes = c(1, 1), ...) {
  annos <- c(prev = prev, abund = abund) # written to support further annos in future
  annos <- sort(annos[!is.na(annos)])

  if (length(unique(annos)) < length(annos) || max(c(annos, 1) > length(annos))) {
    stop("prev and abund must not be the same number, and neither should have a value greater than the number of non-NA entries")
    # TODO ensure that the order of values is used instead of the values (and remove the last part of this error)
  }
  if (length(rel_sizes) != length(annos)) {
    stop("length of rel_sizes must be equal to the number of non-NA annotations")
  }
  sizes <- size * rel_sizes / sum(rel_sizes, na.rm = TRUE)
  list(what = names(annos), undetected = undetected, which = which, sizes = sizes, gap = gap, ...)
}


#' @title taxAnnotation for ComplexHeatmap
#' @description taxAnnotation is used for cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @param data phyloseq or ps-extra
#' @param taxa names of taxa
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which 'row' or 'column' annotation
#' @param ... additional args passed to ComplexHeatmap::HeatmapAnnotation
#' @param what ordered names of annotations (part of function names)
#' @param sizes ordered sizes of annotations, in mm
#'
#' @noRd
taxAnnotation <- function(data, taxa, undetected = 0, which = "row", what = c("prev", "abund"), sizes = c(15, 15), gap = 2, ...) {
  ps <- ps_get(data)
  dots <- list(...)
  # start building args list
  args <- list(
    annotation_name_gp = grid::gpar(fontsize = 8, fontface = "bold"),
    gap = grid::unit(gap, "mm"), which = which
  )
  for (i in seq_along(what)) {
    name <- what[[i]]
    args[[name]] <- do.call(
      what = paste0("anno_", name),
      args = list(data = ps, taxa = taxa, undetected = undetected, which = which, size = sizes[[i]])
    )
  }
  # overwrite and add further args
  args[names(dots)] <- dots
  do.call(ComplexHeatmap::HeatmapAnnotation, args = args)
}

# @param ... args passed to anno_barplot
# @return a ComplexHeatmap anno_barplot
#' @export
#' @rdname heatmap-annotations
anno_prev <- function(data, taxa, undetected = 0, which = "row", size = 15, ...) {
  dots <- list(...)
  prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = prevs, bar_width = 0.4, ylim = 0:1, which = which)
  if (identical(which, "row")) args[["width"]] <- grid::unit(size, "mm")
  if (identical(which, "column")) args[["height"]] <- grid::unit(size, "mm")
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_barplot, args = args)
  return(anno)
}

# @param ... args passed to anno_boxplot
# @return anno_boxplot
#' @export
#' @rdname heatmap-annotations
anno_abund <- function(data, taxa, undetected = 0, which = "row", size = 15, ...) {
  dots <- list(...)
  abunds <- abund_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = abunds, size = grid::unit(1, "mm"), box_width = 0.4, which = which)
  if (identical(which, "row")) {
    args[["x"]] <- t(abunds)
    args[["width"]] <- grid::unit(size, "mm")
  } else if (identical(which, "column")) {
    args[["height"]] <- grid::unit(size, "mm")
  } else {
    stop("which must be either 'row' or 'column', not: ", which)
  }
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_boxplot, args = args)
  return(anno)
}
