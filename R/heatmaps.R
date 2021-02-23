#' @title Microbe-to-sample-data correlation heatmap
#'
#' @description Correlate microbial abundances to numeric sample_data variables
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include, or NA for all
#' @param tax_anno taxAnnotation output describing taxa distribution or NULL
#' @param vars selection of variable names from sample_data
#' @param taxa_which row or column (which represents a taxon)
#' @param cor name of correlation method
#' @param cor_use passed to cor(use = cor_use)
#' @param colors output of heat_colors() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param tax_transform transformation applied to otu_table before correlating
#' @param ...
#'
#' @return ComplexHeatmap heatmap
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
#'
#' taxa <- sample(microbiome::top_taxa(psq)[1:50], size = 30)
#'
#' p <- cor_heatmap(psq, taxa, tax_anno = taxAnnotation(psq, taxa, undetected = 50))
#' p
#'
#' p2 <- cor_heatmap(psq, taxa, tax_anno = taxAnnotation(psq, taxa, undetected = 50, which = "column"), taxa_which = "column")
#' p2
#' @rdname heatmaps
cor_heatmap <- function(data,
                        taxa = NA,
                        tax_anno = NULL,
                        vars = NA,
                        cor = c("pearson", "kendall", "spearman")[1],
                        cor_use = "everything",
                        colors = heat_colors(),
                        numbers = heat_numbers(),
                        taxa_which = "row",
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method,
                        seriation_dist_col = seriation_dist,
                        tax_transform = "identity",
                        ...) {

  # check tax_anno matches taxa_which
  if (
    !identical(tax_anno, NULL) &&
    grepl("Annotation", x = class(tax_anno)[[1]]) &&
    !identical(taxa_which, tax_anno@which)
  ) {
    stop("tax_anno annotation which arg should match taxa_which arg: both 'row' or both 'column'")
  }

  ps <- ps_get(data)

  # handle otu_table data
  otu <- otu_get(data)
  otu <- microbiome::transform(otu, transform = tax_transform)
  otu <- otu[, taxa, drop = FALSE]
  otu_mat <- unclass(otu)

  # handle sample metadata
  samdat <- phyloseq::sample_data(ps)
  meta_mat <- as.matrix.data.frame(samdat[, sapply(samdat, function(x) is.numeric(x) | is.logical(x) | is.integer(x))])

  if (!identical(vars, NA)) {
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

  if (identical(taxa_which, "row")) {
    args[["mat"]] <- cor_mat
    args[["right_annotation"]] = tax_anno
  } else if (identical(taxa_which, "column")) {
    args[["mat"]] = t(cor_mat)
    args[["bottom_annotation"]] = tax_anno
  } else {
    stop("taxa_which must be row or column, it is: ", taxa_which)
  }
  p <- do.call(viz_heatmap, args = args)
  p
}

viz_heatmap <- function(mat,
                        colors = heat_colors(),
                        numbers = heat_numbers(),
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method, seriation_dist_col = seriation_dist,
                        right_annotation = NULL,
                        ...) {
  dots <- list(...)
  # order matrix
  ser <- mat_seriate(mat = mat, method = seriation_method, dist = seriation_dist)

  # work in progress handling
  if (identical(numbers, NULL)){
    cell_fun = NULL
  } else if (inherits(numbers, "function")){
    cell_fun = numbers
    # set closure env's parent env to current env so cell_fun can find object mat!
    # ref https://bookdown.org/rdpeng/rprogdatascience/scoping-rules-of-r.html
    # ref https://adv-r.hadley.nz/function-factories.html
    parent.env(environment(cell_fun)) <- environment()
  } else {
    cell_fun <- switch(
      EXPR = numbers,
      "values" = {
        function(j, i, x, y, width, height, fill) {
          val <- mat[i, j]
          grid::grid.text(label = sprintf("%.1f", val), x = x, y = y, gp = grid::gpar(fontsize = 7))
        }
      }
      # TODO support compositions by transforming mat
    )
  }
  args <- list(
    matrix = mat, name = "Value", col = colors,
    right_annotation = right_annotation,
    row_order = ser$row_order,
    cluster_rows = ser$row_tree,
    column_order = ser$col_order,
    cluster_columns = ser$col_tree,
    column_names_rot = 45,
    column_dend_side = "bottom",
    column_names_side = "top",
    row_names_gp = grid::gpar(fontsize = 8),
    column_names_gp = grid::gpar(fontsize = 8),
    rect_gp = grid::gpar(col = "white", lwd = 0.75),
    cell_fun = cell_fun
  )
  args[names(dots)] <- dots

  p <- do.call(ComplexHeatmap::Heatmap, args = args)
  return(p)
}

#' @title Easy palettes for ComplexHeatmap
#'
#' @description Pass a named diverging colorspace hcl palette to circlize::colorRamp2
#'
#' @param palette named palette from colorspace::hcl_palettes(type = "diverging") or a vector of 3 colour names (low, middle, high)
#' @param breaks numeric vector of values for colour scale breaks (low, middle, high)
#' @param rev_palette reverse the colorspace palette
#'
#' @export
#' @rdname heatmaps
heat_colors <- function(palette = "Green-Orange", breaks = c(-1, 0, 1), rev_palette = FALSE){
  if(length(palette) == 1 && palette %in% rownames(colorspace::hcl_palettes(type = "diverging"))){
    palette <- colorspace::diverge_hcl(palette = palette, n = 3)
  } else if (length(palette) != 3){
    stop("palette must be a vector of three colours, or a valid colour palette from colorspace::hcl_palettes(type = 'diverging')")
  }
  if (rev_palette) palette <- base::rev(palette)
  col_fun = circlize::colorRamp2(breaks = breaks, colors = palette)
  return(col_fun)
}

#' @title Setup for drawing numbers on heatmap tiles
#'
#' @description
#'
#' @param what what numbers to print (currently only "values" is supported)
#' @param fmt number print format
#' @param fontsize fontsize specification,
#' @param ... passed to grid::gpar() for grid.text
#'
#' @export
#' @rdname heatmaps
heat_numbers <- function(what = "values", fmt = "%.1f", fontsize = 7, ...){
    force(fmt)
    force(fontsize)
  if (identical(what, "values")){
    function(j, i, x, y, width, height, fill) {
      grid::grid.text(label = sprintf(fmt, mat[i, j]), x = x, y = y, gp = grid::gpar(fontsize = fontsize, ...))
    }
  } else {
    "currently only 'values' is supported"
  }
}

#' @title taxAnnotation for ComplexHeatmap
#' @description taxAnnotation is used for cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @export
#' @rdname heatmaps
taxAnnotation <- function(data, taxa, undetected = 0, which = "row", prev = TRUE, abun = TRUE, ...){
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
anno_prev <- function(data, taxa, undetected = 0, which = "row", ...){
  dots <- list(...)
  prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = prevs, bar_width = 0.4, ylim = 0:1, which = which)
  if (identical(which, "row")) args[["width"]] = grid::unit(15,"mm")
  if (identical(which, "column")) args[["height"]] = grid::unit(15,"mm")
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_barplot, args = args)
  return(anno)
}


# Computing a complexheatmap annotation for abundance distributions
# @param ... args passed to anno_boxplot
# @return anno_boxplot
anno_abund <- function(data, taxa, undetected = 0, which = "row", ...){
  dots <- list(...)
  abunds <- abund_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = abunds, size = grid::unit(1, "mm"), box_width = 0.4, which = which)
  if (identical(which, "row")){
    args[['x']] <- t(abunds)
    args[["width"]] = grid::unit(15,"mm")
  } else if (identical(which, "column")){
    args[["height"]] = grid::unit(15,"mm")
  } else {
    stop("which must be either 'row' or 'column', not: ", which)
  }
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_boxplot, args = args)
  return(anno)
}

