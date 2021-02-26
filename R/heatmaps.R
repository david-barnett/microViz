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
#' psq <- tax_agg(psq, "Genus") %>% ps_get()
#'
#' set.seed(123)
#' taxa <- sample(microbiome::top_taxa(psq)[1:50], size = 30)
#'
#' p <- cor_heatmap(psq, taxa, anno_tax = taxAnnotation(psq, taxa, undetected = 50))
#' p
#'
#' p2 <- cor_heatmap(psq, taxa, anno_tax = taxAnnotation(psq, taxa, undetected = 50, which = "column"), taxa_which = "column")
#' p2
#'
#' p3 <- comp_heatmap(data = psq, taxa = taxa)
#' p3
#' @rdname heatmaps
cor_heatmap <- function(data,
                        taxa = NA,
                        anno_tax = NULL,
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
                        ...) {

  # check anno_tax matches taxa_which
  if (
    !identical(anno_tax, NULL) &&
      grepl("Annotation", x = class(anno_tax)[[1]]) &&
      !identical(taxa_which, anno_tax@which)
  ) {
    stop("anno_tax annotation which arg should match taxa_which arg: both 'row' or both 'column'")
  }

  ps <- ps_get(data)

  # use all available taxa if none specified
  if (identical(taxa, NA)) taxa <- phyloseq::taxa_names(ps)

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
    ...
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
#'
#' @param samples
#' @param anno_samples
#' @param tax_transform_colors
#' @param tax_scale_colors
#' @param tax_select
#' @param sample_select
#' @param tax_transform_numbers
#' @param tax_scale_numbers
#'
#' @export
comp_heatmap <- function(data,
                         taxa = NA, # selection of taxa BEFORE any tax_transform
                         samples = NA, # selection of samples BEFORE any tax_transform
                         anno_tax = NULL,
                         anno_samples = NULL,
                         colors = heat_palette(), # just passed to viz_heatmap
                         tax_transform_colors = "clr",
                         tax_scale_colors = "neither",
                         tax_select = NA, # selection of samples to show AFTER transformation of taxa
                         sample_select = NA, # selection of samples to show AFTER transformation of taxa
                         taxa_which = "row",
                         # stuff just passed to viz_heatmap
                         seriation_method = "OLO_ward",
                         seriation_dist = "euclidean",
                         seriation_method_col = seriation_method,
                         seriation_dist_col = seriation_dist,
                         # numbers
                         numbers = NULL, # or list made by heat_numbers() or a function in format of a ComplexHeatmap cell_fun
                         tax_transform_numbers = tax_transform_colors,
                         tax_scale_numbers = tax_scale_colors,
                         ... # passed to viz_heatmap
) {
  ps <- ps_get(data)
  # use all taxa if NA given
  if (identical(taxa, NA)) taxa <- phyloseq::taxa_names(ps)
  if (identical(samples, NA)) samples <- phyloseq::sample_names(ps)

  # create taxa annotation object if "instructions" given
  # TODO

  # TODO needs updating to handle specification of tax annotation manually or inside function?
  # anno_tax suitable for taxa_which
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
    colors = colors,
    numbers = numbers,
    seriation_method = seriation_method,
    seriation_dist = seriation_dist,
    seriation_method_col = seriation_method_col,
    seriation_dist_col = seriation_dist_col,
    rect_gp = grid::gpar(lwd = 0)
  )

  args[["mat"]] <- otu_mat
  args[["numbers_mat"]] <- otu_numbers

  # TODO allow bottom, top, left and right in addition to rows and columns
  if (identical(taxa_which, "row")) {
    args[["right_annotation"]] <- anno_tax
    args[["mat"]] <- t(args[["mat"]])
    args[["numbers_mat"]] <- t(args[["numbers_mat"]])
  } else if (identical(taxa_which, "column")) {
    args[["bottom_annotation"]] <- anno_tax
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
#' @description Pass a named diverging colorspace hcl palette to circlize::colorRamp2
#'
#' @param palette named palette from colorspace::hcl_palettes(type = "diverging") or a vector of colour names (same length as indicated by breaks)
#' @param breaks integer number of breaks, or numeric vector of values for colour scale breaks (including ends)
#' @param range NA to return palette generating function that takes a range, or numeric vector indicating the range, to return a palette
#' @param rev_palette reverse the colorspace palette
#'
#' @return circlize::colorRamp2 palette if !is.na(range), or function returning a palette when given a range
#' @export
#' @rdname heat_palette
heat_palette <- function(palette = "Greens", breaks = 5, range = NA, rev_palette = FALSE, sym = FALSE) {
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
  } else if (length(palette) != 3) {
    stop("palette must be a vector of colours or a palette name from colorspace::hcl_palettes() diverging/sequential")
  }
  if (rev_palette) palette <- base::rev(palette)
  col_fun <- function(range){
    if (isTRUE(sym)) range <- c(-max(abs(range)), max(abs(range)))
    breaks <- seq(from = range[[1]], to = range[[2]], length.out = n_breaks)
    circlize::colorRamp2(breaks = breaks, colors = palette)
  }
  if (identical(range, NA)){
    return(col_fun)
  } else if (is.numeric(range)){
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
heat_numbers <- function(decimals = 1, fmt = "%.1f", fontsize = 6, col = "black", fontface = "plain", ...) {
  fmt <- paste0("%.", decimals, "f")
  gp <- grid::gpar(fontsize = fontsize, col = col, fontface = fontface, ...)
  list(fmt = fmt, gp = gp)
}

#' @title Create list describing taxa annotation
#' @description  main usefulness of tax_anno function is to give sane defaults and prompt user which annotation options are available
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which "row" or "column" annotation?
#' @param prev order in which prevalence annotation shown (number, or NA)
#' @param abund order in which abundance annotation shown (number, or NA)
#' @param ... further args passed HeatmapAnnotation
#'
#' @export
#' @rdname heatmap-annotations
tax_anno <- function(undetected = 0, which = "row", prev = 1, abund = 2, ...) {
  annos <- c(prev = prev, abund = abund) # written to support further annos in future
  annos <- sort(annos[!is.na(annos)])

  if (length(unique(annos)) < length(annos) || max(c(annos, 1) > length(annos))) {
    stop("prev and abund must not be the same number, and neither should have a value greater than the number of non-NA entries")
    # TODO ensure that the order of values is used instead of the values (and remove the last part of this error)
  }
  list(annos = names(annos), undetected = undetected, which = which, ...)
}


#' @title taxAnnotation for ComplexHeatmap
#' @description taxAnnotation is used for cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @param data phyloseq or ps-extra
#' @param taxa names of taxa
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which 'row' or 'column' annotation
#' @param prev if TRUE, prevalence annotation plot drawn with anno_prev
#' @param abun if TRUE, abundance annotation plot drawn with anno_abund
#' @param ... additional args passed to ComplexHeatmap::HeatmapAnnotation
#'
#' @export
#' @noRd
taxAnnotation <- function(data, taxa, undetected = 0, which = "row", prev = TRUE, abun = TRUE, ...) {
  ps <- ps_get(data)
  dots <- list(...)
  # start building args list
  args <- list(gap = grid::unit(2, "mm"), which = which)
  if (isTRUE(prev)) args[["prev"]] <- anno_prev(data = ps, taxa = taxa, undetected = undetected, which = which)
  if (isTRUE(abun)) args[["abun"]] <- anno_abund(data = ps, taxa = taxa, undetected = undetected, which = which)
  # overwrite and add further args
  args[names(dots)] <- dots
  do.call(ComplexHeatmap::HeatmapAnnotation, args = args)
}

# @param ... args passed to anno_barplot
# @return a ComplexHeatmap anno_barplot
#' @export
#' @rdname heatmap-annotations
anno_prev <- function(data, taxa, undetected = 0, which = "row", ...) {
  dots <- list(...)
  prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = prevs, bar_width = 0.4, ylim = 0:1, which = which)
  if (identical(which, "row")) args[["width"]] <- grid::unit(15, "mm")
  if (identical(which, "column")) args[["height"]] <- grid::unit(15, "mm")
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_barplot, args = args)
  return(anno)
}

# @param ... args passed to anno_boxplot
# @return anno_boxplot
#' @export
#' @rdname heatmap-annotations
anno_abund <- function(data, taxa, undetected = 0, which = "row", ...) {
  dots <- list(...)
  abunds <- abund_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = abunds, size = grid::unit(1, "mm"), box_width = 0.4, which = which)
  if (identical(which, "row")) {
    args[["x"]] <- t(abunds)
    args[["width"]] <- grid::unit(15, "mm")
  } else if (identical(which, "column")) {
    args[["height"]] <- grid::unit(15, "mm")
  } else {
    stop("which must be either 'row' or 'column', not: ", which)
  }
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_boxplot, args = args)
  return(anno)
}
