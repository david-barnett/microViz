# main heatmap functions --------------------------------------------------

#' @title Microbe-to-sample-data correlation heatmap
#'
#' @description
#' Plot correlations between (transformed) microbial abundances and
#' (selected) numeric-like sample_data variables from a phyloseq object.
#'
#' @details
#' Using a data.frame for the data argument is also possible,
#' in which case the (selected) numeric-like variables will be correlated
#' with each other, and all arguments relating to taxa will be ignored.
#'
#' @inheritDotParams ComplexHeatmap::Heatmap show_heatmap_legend
#' row_dend_side row_dend_width show_row_dend row_dend_gp
#' row_names_side show_row_names row_names_gp row_names_rot row_names_centered
#'
#' @param data phyloseq or phyloseq extra
#' @param taxa list of taxa to include, or NA for all
#' @param tax_anno
#' NULL or annotation function for taxa: taxAnnotation() output.
#' @param vars selection of variable names from sample_data
#' @param var_anno
#' NULL or annotation function for variables: varAnnotation() output.
#' @param taxa_side
#' "top"/"right"/"bottom"/"left": controls heatmap orientation and where any
#' annotations specified in tax_anno are placed
#' @param vars_side
#' which side to place any variable annotations specified in var_anno,
#' must be an adjacent side to taxa_side
#' @param cor
#' correlation coefficient. pearson/kendall/spearman,
#' can be abbreviated (used as legend title)
#' @param cor_use passed to cor(use = cor_use)
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col
#'  method to order the columns (in seriation::seriate)
#' @param seriation_dist_col
#' distance to use in seriation_method_col (if needed)
#' @param tax_transform
#' transformation applied to otu_table before correlating
#' (and before selection of taxa by name, so e.g. proportions use all taxa)
#' @param gridlines list output of heat_grid() for setting gridline style
#' @param var_fun
#' a function (or name of) to be applied to columns of a matrix of vars
#' before correlating (but not used in any variable annotations)
#' @param anno_tax
#' DEPRECATED:
#' optional annotation of taxa distributions: tax_anno() list output,
#' or a pre-made ComplexHeatmap HeatmapAnnotation
#' @param anno_vars
#' DEPRECATED: use var_anno argument instead.
#' Optional annotation of variable distributions:
#' var_anno() list output, or a pre-made ComplexHeatmap HeatmapAnnotation
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use
#' psq <- dietswap %>%
#'   ps_mutate(
#'     weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'     female = if_else(sex == "female", true = 1, false = 0),
#'     african = if_else(nationality == "AFR", true = 1, false = 0)
#'   )
#' psq <- tax_filter(psq, min_prevalence = 1 / 10, min_sample_abundance = 1 / 10)
#' psq <- tax_agg(psq, "Genus")
#'
#' # randomly select 20 taxa from the 50 most abundant taxa
#' set.seed(123)
#' taxa <- sample(tax_top(psq, n = 50), size = 20)
#'
#' # NOTE: detection threshold set to 50 as HITchip example data seems to have background noise
#' ud <- 50
#'
#' # make simple correlation heatmap with all numeric-like variables
#' cor_heatmap(
#'   data = psq, taxa = taxa,
#'   tax_anno = taxAnnotation(
#'     Prv. = anno_tax_prev(undetected = ud),
#'     Abd. = anno_tax_box(undetected = ud)
#'   )
#' )
#'
#' # You can create an annotation object separately in advance
#' taxAnno <- taxAnnotation(
#'   Prv. = anno_tax_prev(undetected = ud), Abd. = anno_tax_box(undetected = ud)
#' )
#' class(taxAnno) # "function"
#'
#' # You can select which numeric-like variables to correlate taxa with
#' cor_heatmap(
#'   psq, taxa,
#'   vars = c("african", "female", "weight"), tax_anno = taxAnno
#' )
#'
#' # Also you can choose alternative correlation measures
#' cor_heatmap(psq, taxa, cor = "spearman", tax_anno = taxAnno)
#'
#' # Annotating variables is possible, and easy with varAnnotation()
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno,
#'   var_anno = varAnnotation(Val. = anno_var_box(size = grid::unit(2, "cm")))
#' )
#'
#' # you can transform the variables before correlating by var_fun
#' # notice this does not affect the data used for annotations
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno, var_fun = "exp",
#'   var_anno = varAnnotation(Val. = anno_var_box(size = grid::unit(2, "cm")))
#' )
#'
#' # other and multiple annotations
#' cor_heatmap(
#'   data = psq, taxa = taxa[1:10], vars = c("african", "weight", "female"),
#'   tax_anno = taxAnno,
#'   var_anno = varAnnotation(
#'     value = anno_var_hist(size = grid::unit(15, "mm")),
#'     log10p = anno_var_box(function(x) log10(x + 1))
#'   )
#' )
#'
#' # make the same heatmap, but rotated
#' cor_heatmap(
#'   data = psq, taxa = taxa[1:10], vars = c("african", "weight", "female"),
#'   tax_anno = taxAnno, taxa_side = "top",
#'   var_anno = varAnnotation(
#'     value = anno_var_hist(size = grid::unit(15, "mm")),
#'     log10p = anno_var_box(function(x) log10(x + 1))
#'   )
#' )
#'
#' # You can change the colour scheme used, using heat_palette()
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno,
#'   colors = heat_palette("Green-Orange", rev = TRUE, sym = TRUE)
#' )
#'
#' # You can hide or change the style of the numbers with heat_numbers()
#' cor_heatmap(data = psq, taxa = taxa, tax_anno = taxAnno, numbers = NULL)
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno,
#'   colors = heat_palette("Berlin", rev = TRUE, sym = TRUE),
#'   numbers = heat_numbers(decimals = 2, col = "white", fontface = "bold")
#' )
#'
#' # You can hide or change the style of the grid lines with grid_col & grid_lwd
#' cor_heatmap(psq, taxa = taxa, tax_anno = taxAnno, grid_col = NA) # hidden
#' cor_heatmap(psq, taxa = taxa, tax_anno = taxAnno, grid_lwd = 3) # bigger
#'
#' # You can pass any other argument from `ComplexHeatmap::Heatmap()` to `...`
#'
#' # e.g. You can set the absolute width and height of the heatmap body
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno,
#'   width = grid::unit(40, "mm"), height = grid::unit(10, "cm")
#' )
#'
#' # e.g. You can suppress the legend
#' cor_heatmap(
#'   data = psq, taxa = taxa, tax_anno = taxAnno, show_heatmap_legend = FALSE,
#'   width = grid::unit(40, "mm"), height = grid::unit(10, "cm")
#' )
cor_heatmap <- function(data,
                        taxa = NA,
                        tax_anno = taxAnnotation(
                          Prev. = anno_tax_prev(), Abun. = anno_tax_box()
                        ),
                        taxon_renamer = identity,
                        vars = NA,
                        var_anno = NULL,
                        cor = c("pearson", "kendall", "spearman"),
                        cor_use = "everything",
                        colors = heat_palette(
                          palette = "Blue-Red 2", sym = TRUE
                        ),
                        numbers = heat_numbers(
                          decimals = 1, col = "black", fontface = "plain"
                        ),
                        taxa_side = "right",
                        vars_side = adjacent_side(taxa_side),
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method,
                        seriation_dist_col = seriation_dist,
                        var_fun = "identity",
                        grid_col = "white",
                        grid_lwd = 0.5,
                        anno_tax = NULL,
                        anno_vars = NULL,
                        ...) {

  # check correlation type argument
  cor <- match.arg(cor)

  taxa_which <- annoWhichFromAnnoSide(taxa_side, argName = "taxa_side")
  vars_which <- annoWhichFromAnnoSide(vars_side, argName = "vars_side")

  if (inherits(data, "data.frame")) {
    otu_mat <- NULL # causes cor to only use x (meta_mat)
    meta_mat <- df_to_numeric_matrix(data, vars = vars, trans_fun = var_fun)
  } else if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    if (identical(taxa_which, vars_which)) {
      stop("vars and taxa sides must be adjacent, not the same or opposite")
    }

    ps <- ps_get(data)

    # handle sample metadata
    samdat <- phyloseq::sample_data(ps)
    meta_mat <- df_to_numeric_matrix(samdat, vars = vars, trans_fun = var_fun)

    # default taxa names is all taxa names
    if (identical(taxa, NA)) taxa <- phyloseq::taxa_names(ps)

    # handle otu_table data if relevant
    if (identical(taxa, NULL)) {
      otu_mat <- NULL
    } else {
      otu_mat <- unclass(otu_get(data)[, taxa, drop = FALSE])

      # rename taxa
      colnames(otu_mat) <- taxon_renamer(colnames(otu_mat))
    }
  } else {
    stop(
      "data must be phyloseq, ps_extra, or data.frame, not: ",
      paste(class(data), collapse = " ")
    )
  }

  # correlate datasets (x to y or just pairs within x if y is NULL)
  cor_mat <- stats::cor(x = meta_mat, y = otu_mat, use = cor_use, method = cor)
  if (identical(taxa_which, "row")) cor_mat <- t(cor_mat)

  # compute order and any h clustering trees for matrix rows and columns
  ser <- mat_seriate(
    mat = cor_mat, method = seriation_method, dist = seriation_dist,
    col_method = seriation_method_col, col_dist = seriation_dist_col
  )

  # getting colour range from data if necessary
  # the formalArgs condition is to exclude the grDevices::colors function!
  if ("range" %in% methods::formalArgs(def = colors)) {
    colors <- colors(range = range(cor_mat, na.rm = TRUE, finite = TRUE))
  }

  # cell label function from `numbers` (function or arg list) & number matrix
  # this is a ComplexHeatmap-specific format of function
  cell_fun <- heatmapMakeCellFun(numbers = numbers, numbers_mat = cor_mat)

  # annotations --------------------------------------------------------------

  # possibly create taxa annotation if otu_table extracted (i.e. taxa used)
  if (!identical(otu_mat, NULL)) {
    if (!identical(anno_tax, NULL)) {
      # create taxa annotation object if "instructions" given
      anno_tax <- old_anno_tax_helper(
        anno_tax = anno_tax, ps = ps, taxa = taxa, side = taxa_side
      )
    } else if (inherits(tax_anno, "function")) {
      anno_tax <- tax_anno(.data = data, .taxa = taxa, .side = taxa_side)
    } else if (methods::is(tax_anno, "HeatmapAnnotation")) {
      annoWhichMatchCheck(which = taxa_which, anno = tax_anno)
      anno_tax <- tax_anno
    } else {
      anno_tax <- NULL
    }
  }

  # create variable annotation object if "instructions" given
  if (inherits(anno_vars, "list")) {
    warning("anno_vars argument is deprecated, used var_anno instead!")
    anno_args <- c(anno_vars, list(data = data, vars = vars))
    anno_vars <- do.call(what = varAnnotate, args = anno_args)
  } else if (identical(anno_vars, NULL)) {
    if (identical(var_anno, NULL)) {
      anno_vars <- var_anno
    } else if (inherits(var_anno, "function")) {
      anno_vars <- var_anno(.data = data, .vars = vars, .side = vars_side)
    }
  }

  # Heatmap args ------------------------------------------------------------

  # get other args
  dots <- list(...)
  if ("gridlines" %in% names(dots)) stop("gridlines is deprecated, use grid_col and grid_lwd instead")

  args <- list(
    matrix = cor_mat,
    name = cor,
    col = colors,
    cell_fun = cell_fun,
    row_order = ser$row_order,
    cluster_rows = ser$row_tree,
    column_order = ser$col_order,
    cluster_columns = ser$col_tree,
    column_names_gp = grid::gpar(fontsize = 7),
    row_names_gp = grid::gpar(fontsize = 7),
    column_names_rot = 45,
    column_dend_side = "bottom",
    column_names_side = "top",
    rect_gp = grid::gpar(col = grid_col, lwd = grid_lwd),
    heatmap_legend_param = list(
      title = cor, title_gp = grid::gpar(fontsize = 9, fontface = "bold"),
      labels_gp = grid::gpar(fontsize = 7)
    )
  )

  # add taxa annotation (is NULL if no taxa were used)
  args[[paste0(taxa_side, "_annotation")]] <- anno_tax
  # add variable annotation
  args[[paste0(vars_side, "_annotation")]] <- anno_vars

  # use extra args, adding or overwriting
  args[names(dots)] <- dots

  p <- do.call(ComplexHeatmap::Heatmap, args = args)
  return(p)
}


#' @title
#' Heatmap of taxonomic composition across samples
#'
#' @description
#' Heatmap made with ComplexHeatmap, with optional (but default) annotation of taxa prevalence and abundance.
#'
#' Transform your data with tax_transform prior to plotting (and/or scale with tax_scale).
#' Plotting "compositional" data can give an idea of the dominant taxa in each sample.
#' Plotting some form of log or clr transformed (or scaled) microbial features can highlight
#' other patterns.
#' The transformed data will be ordered via your selected seriation methods and distances.
#' Any cell numbers printed can be set to different values, and do not affect ordering.
#'
#' @inheritDotParams ComplexHeatmap::Heatmap
#' row_dend_side row_dend_width show_row_dend row_dend_gp
#' row_names_side show_row_names row_names_gp row_names_rot row_names_centered
#'
#' @param data phyloseq extra output of tax_transform or tax_agg
#' @param taxa list of taxa to include (selection occurs AFTER any tax_transform and scaling)
#' @param samples list of taxa to include (selection occurs AFTER any tax_transform and scaling)
#' @param anno_tax NULL or tax_anno() list output
#' @param anno_samples NULL only support so far TODO
#' @param taxa_side controls heatmap orientation and where any anno_tax annotations are placed (top/bottom/left/right)
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers NULL or output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param tax_transform_numbers transformation applied to otu_table used only for any numbers printed
#' @param tax_scale_numbers scaling applied to numbers otu_table after transformation
#' @param gridlines list output of heat_grid() for setting gridline style
#' @param name used as legend title (colourbar)
#'
#' @export
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
#' psq <- tax_filter(psq, min_prevalence = 1 / 10, min_sample_abundance = 1 / 10)
#' psq <- tax_agg(psq, "Genus")
#'
#' # randomly select 20 taxa from the 40 top taxa, and 40 random samples
#'
#' set.seed(123)
#' taxa <- sample(tax_top(psq, n = 40), size = 20)
#' samples <- sample(1:122, size = 40)
#'
#' comp_heatmap(data = psq, taxa = taxa, samples = samples)
#'
#' # transforming taxon abundances #
#'
#' # NOTE: if you plan on transforming taxa (e.g. to compositional data or clr)
#' # but only want to plot a subset of the taxa (e.g. most abundant)
#' # you should NOT subset the original phyloseq before transformation!
#' # Instead, choose the subset of taxa plotted with:
#'
#' # Note 2, choose a symmetrical palette for clr-transformed data
#' psq %>%
#'   tax_transform("clr", zero_replace = "halfmin") %>%
#'   comp_heatmap(
#'     taxa = taxa, samples = samples, colors = heat_palette(sym = TRUE)
#'   )
#'
#' # Almost all the taxa have high values (>> 0) because they are a highly
#' # abundant subset taken after clr transformation was calculated on all taxa
#'
#' # See how just taking the first 30 taxa from the dataset gives more balance
#' psq %>%
#'   tax_transform("clr", zero_replace = "halfmin") %>%
#'   comp_heatmap(
#'     taxa = 1:30, samples = samples, colors = heat_palette(sym = TRUE)
#'   )
#'
#' # annotating taxa #
#'
#' # Notes:
#' # - Unlike cor_heatmap, taxa are not annotated by default
#' # - Detection threshold set to 50 as HITchip example data seems to have background noise
#'
#' comp_heatmap(
#'   data = psq, taxa = taxa, samples = samples,
#'   tax_anno = taxAnnotation(Prev = anno_tax_prev(undetected = 50))
#' )
#'
#' # annotating samples #
#'
#' htmp <- psq %>%
#'   tax_transform("clr", zero_replace = "halfmin") %>%
#'   comp_heatmap(
#'     taxa = taxa, samples = samples, colors = heat_palette(sym = TRUE),
#'     sample_anno = sampleAnnotation(
#'       Nation. = anno_sample_cat("nationality", legend_title = "Nation.")
#'     )
#'   )
#' htmp
#'
#' # legends from `anno_sample_cat()` are stored as an attribute of the Heatmap
#' ComplexHeatmap::draw(
#'   object = htmp,
#'   annotation_legend_list = attr(htmp, "AnnoLegends"), merge_legends = TRUE
#' )
comp_heatmap <- function(data,
                         taxa = NA,
                         taxa_side = "right",
                         tax_anno = NULL,
                         taxon_renamer = identity,
                         samples = NA,
                         sample_side = adjacent_side(taxa_side),
                         sample_anno = NULL,
                         sample_names_show = FALSE,
                         colors = heat_palette(palette = "Rocket", rev = TRUE),
                         numbers = NULL, # or list made by heat_numbers()
                         sample_seriation = "OLO_ward",
                         sample_ser_dist = "euclidean",
                         sample_ser_counts =
                           !sample_ser_dist %in% c("euclidean", "maximum", "manhattan", "canberra", "binary"),
                         sample_ser_trans = NULL,
                         # TODO sample_ser_taxa = "shown",
                         # "shown", "all" or vector of names/numbers
                         # (or 'auto'?, to depend on whether ecological distance measure used?)
                         tax_seriation = "OLO_ward",
                         tax_ser_dist = "euclidean",
                         tax_ser_counts = FALSE,
                         tax_ser_trans = NULL,
                         # TODO tax_ser_samples = "shown",
                         # "shown", "all" or vector of names/numbers
                         #
                         # numbers
                         numbers_trans = NULL,
                         numbers_zero_replace = 0,
                         numbers_use_counts = TRUE,
                         grid_col = "white",
                         grid_lwd = 0.1,
                         name = "Abd.",
                         anno_tax = NULL,
                         ...) {
  # infer whether taxa and samples are rows or columns from side arguments
  taxa_which <- annoWhichFromAnnoSide(taxa_side, argName = "taxa_side")
  sample_which <- annoWhichFromAnnoSide(sample_side, argName = "sample_side")
  if (identical(taxa_which, sample_which)) {
    stop("taxa and sample sides must be adjacent, not the same or opposite")
  }

  # default to all taxa and all samples
  if (identical(taxa, NA)) taxa <- phyloseq::taxa_names(ps_get(data))
  if (identical(samples, NA)) samples <- phyloseq::sample_names(ps_get(data))

  # get otu_table data (used for colours and seriation)
  # any transformation must be done in advance
  otu_mat <- otu_get(data, samples = samples, taxa = taxa, counts = FALSE)

  # rename taxa
  colnames(otu_mat) <- taxon_renamer(colnames(otu_mat))

  # get automatic name for colourbar legend
  if (identical(name, "auto")) name <- info_get(data)[["tax_transform"]]

  # get matrix for printing numbers on cells
  if (identical(numbers, NULL)) {
    # avoid computation if otu_numbers won't be shown anyway
    otu_numbers <- otu_mat
  } else {
    otu_numbers <- taxCalcAbund(
      data = data, taxa = taxa, trans = numbers_trans,
      use_counts = numbers_use_counts, zero_replace = numbers_zero_replace
    )[samples, , drop = FALSE]
  }

  # getting colour range from data if necessary
  # the formalArgs condition is to exclude the grDevices::colors function!
  if ("range" %in% methods::formalArgs(def = colors)) {
    colors <- colors(range = range(otu_mat, na.rm = TRUE, finite = TRUE))
  }

  # rotate matrices if taxa are rows of heatmap
  if (identical(taxa_which, "row")) {
    otu_mat <- t(otu_mat)
    otu_numbers <- t(otu_numbers)
  }

  # seriate taxa and samples appropriately
  otuTax <- taxCalcAbund(
    data = data, taxa = taxa, use_counts = tax_ser_counts,
    trans = tax_ser_trans, zero_replace = "halfmin"
  )[samples, , drop = FALSE]
  otuSam <- taxCalcAbund(
    data = data, taxa = taxa, use_counts = sample_ser_counts,
    trans = sample_ser_trans, zero_replace = "halfmin"
  )[samples, , drop = FALSE]
  ser <- list()
  ser[[taxa_which]] <- rowSeriationInfo(
    mat = t(otuTax), method = tax_seriation, dist = tax_ser_dist
  )
  ser[[sample_which]] <- rowSeriationInfo(
    mat = otuSam, method = sample_seriation, dist = sample_ser_dist
  )

  # remove taxa_are_rows attr. which can remain from comp_heatmap otu_table
  # (caused warning when Heatmap sets class(mat))
  # must be done only after seriation
  otu_mat <- methods::as(otu_mat, Class = "matrix")
  otu_numbers <- methods::as(otu_numbers, Class = "matrix")

  # cell label function from `numbers` (function or arg list) & number matrix
  # this is a ComplexHeatmap-specific format of function
  cell_fun <- heatmapMakeCellFun(numbers = numbers, numbers_mat = otu_numbers)

  # Annotations ---------------------------------------------------------------

  # create heatmap annotation object for taxa if "instructions" given
  if (!identical(anno_tax, NULL)) {
    # create taxa annotation object if "instructions" given
    anno_tax <- old_anno_tax_helper(
      anno_tax = anno_tax, ps = ps, taxa = taxa, side = taxa_side
    )
  } else if (inherits(tax_anno, "function")) {
    anno_tax <- tax_anno(.data = data, .taxa = taxa, .side = taxa_side)
  } else if (methods::is(tax_anno, "HeatmapAnnotation")) {
    annoWhichMatchCheck(which = taxa_which, anno = tax_anno)
    anno_tax <- tax_anno
  } else {
    anno_tax <- NULL
  }

  if (inherits(sample_anno, "function")) {
    sample_anno <- sample_anno(
      .data = data, .samples = samples, .side = sample_side
    )
  } else if (methods::is(sample_anno, "HeatmapAnnotation")) {
    annoWhichMatchCheck(
      which = sample_which, anno = sample_anno, context = "sample"
    )
  } else if (!is.null(sample_anno)) {
    stop(
      "sample_anno must be output of sampleAnnotation() or a HeatmapAnnotation"
    )
  }

  # Heatmap args --------------------------------------------------------------
  # build list of arguments for ComplexHeatmap::Heatmap

  # get other args
  dots <- list(...)
  if ("gridlines" %in% names(dots)) {
    stop("argument gridlines is deprecated: use grid_col and grid_lwd instead")
  }

  args <- list(
    name = name,
    matrix = otu_mat,
    col = colors,
    cell_fun = cell_fun,
    row_order = ser$row$order,
    cluster_rows = ser$row$tree,
    column_order = ser$column$order,
    cluster_columns = ser$column$tree,
    rect_gp = grid::gpar(col = grid_col, lwd = grid_lwd),
    heatmap_legend_param = list(labels_gp = grid::gpar(fontsize = 8)),
    column_names_gp = grid::gpar(fontsize = 7),
    row_names_gp = grid::gpar(fontsize = 7)
  )

  # add taxa and sample annotation objects to correct side arguments
  args[[paste0(taxa_side, "_annotation")]] <- anno_tax
  args[[paste0(sample_side, "_annotation")]] <- sample_anno

  # heuristics for decent automatic placing of dendrograms and names
  args$column_dend_side <- ifelse(is.null(args$top_annotation), "top", "bottom")
  args$column_names_side <- opposite_side(args$column_dend_side)
  args$column_names_rot <- ifelse(args$column_names_side == "top", 45, -45)
  # row dends and names
  args$row_dend_side <- ifelse(is.null(args$left_annotation), "left", "right")
  args$row_names_side <- opposite_side(args$row_dend_side)
  # suppress showing names of samples on whichever side they appear
  args[[paste0("show_", sample_which, "_names")]] <- sample_names_show


  # use extra args passed to dots (overwriting any already set)
  args[names(dots)] <- dots

  p <- do.call(ComplexHeatmap::Heatmap, args = args)

  # add legends as attribute
  attr(p, "AnnoLegends") <- attr(sample_anno, "Legends")

  return(p)
}

# helper functions ------------------------------------------------------------

#' @title Easy palettes for ComplexHeatmap
#'
#' @description
#' Pass a named colorspace hcl palette to circlize::colorRamp2.
#'
#'  - If you do not specify a range this function returns a function and
#'   the heatmap color palette will use the range of the data automatically
#'  - If you do specify a range, this returns a colour palette with that range
#'
#' @param palette
#' named palette from colorspace::hcl_palettes() diverging/sequential
#' or a vector of colour names/hexcodes
#' @param breaks integer number of breaks
#' @param range
#' NA to return palette generating function that takes range
#' or numeric vector indicating the range, to return a palette
#' @param rev reverse the palette?
#' @param sym makes palette range symmetrical around 0 if TRUE
#'
#' @return
#' circlize::colorRamp2 palette if range = NA,
#' or function returning a palette when given a range
#' @export
#' @rdname heat_palette
heat_palette <- function(palette = ifelse(sym, "Blue-Red 3", "Rocket"),
                         breaks = 7,
                         range = NA,
                         sym = FALSE,
                         rev = !sym) {
  n_breaks <- if (length(breaks) > 1) length(breaks) else breaks

  # palette arg of length 1 must be valid colorspace::hcl_palette()
  validDiverging <- rownames(colorspace::hcl_palettes(type = "diverging"))
  validDivergingX <- rownames(colorspace::divergingx_palettes())
  validSequential <- rownames(colorspace::hcl_palettes(type = "sequential"))
  # convert this to a set of colours n_breaks long
  if (length(palette) == 1) {
    if (palette %in% validDiverging) {
      palette <- colorspace::diverge_hcl(palette = palette, n = n_breaks)
    } else if (palette %in% validDivergingX) {
      palette <- colorspace::divergex_hcl(palette = palette, n = n_breaks)
    } else if (palette %in% validSequential) {
      palette <- colorspace::sequential_hcl(palette = palette, n = n_breaks)
    } else {
      stop(
        call. = FALSE,
        "\nheat_palette() palette argument must be either a:\n",
        "- vector of colours, or\n",
        "- palette name from colorspace::hcl_palettes(type = 'diverging')\n",
        "- palette name from colorspace::divergingx_palettes()\n",
        "- palette name from colorspace::hcl_palettes(type = 'sequential')"
      )
    }
  }
  # reverse palette direction if requested
  if (isTRUE(rev)) palette <- base::rev(palette)

  # set up colour function
  col_fun <- function(range) {
    if (isTRUE(sym)) range <- c(-max(abs(range)), max(abs(range)))
    breaks <- seq(from = range[[1]], to = range[[2]], length.out = n_breaks)
    pal <- circlize::colorRamp2(breaks = breaks, colors = palette)
    return(pal)
  }
  # return or evaluate colour function, dependent on whether range given
  if (identical(range, NA)) {
    return(col_fun)
  } else if (inherits(range, "numeric") || inherits(range, "integer")) {
    # in case range set with e.g. -2:2 for convenience
    range <- base::range(range)
    colorscale <- col_fun(range = range)
    return(colorscale)
  } else {
    stop(
      "range argument must be NA, numeric or integer, not: ",
      paste(class(range), collapse = " ")
    )
  }
}

#' @title Aesthetic settings for drawing numbers on heatmap tiles
#'
#' @description
#' Works with comp_heatmap() and cor_heatmap().
#' See the help for those functions.
#'
#' @param decimals number of decimal places to print
#' @param fontsize fontsize specification,
#' @param col colour of font
#' @param fontface plain, bold, italic
#' @param fmt
#' NULL or number print format, see ?sprintf, overrides decimals arg if set
#' @param ... passed to grid::gpar() for grid.text
#' @return list
#' @export
#' @rdname heat_numbers
heat_numbers <- function(decimals = 0,
                         fontsize = 7,
                         col = "darkgrey",
                         fontface = "bold",
                         fmt = NULL,
                         ...) {
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
  stop("heat_grid is deprecated, use grid_col and grid_lwd args instead!")
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

# helper function to convert top or bottom to column, and left or right to row
# with helpful error for invalid argument, used inside cor_heatmap and comp_heatmap
annoWhichFromAnnoSide <- function(side, argName = "side") {
  opts <- c("top", "bottom", "left", "right")
  if (!inherits(side, "character") || length(side) != 1 || !side %in% opts) {
    stop(
      argName, " must be one of: '", paste(opts, collapse = "' / '"),
      "'\nnot: ", paste(side, collapse = " ")
    )
  }
  if (side %in% c("top", "bottom")) Which <- "column"
  if (side %in% c("left", "right")) Which <- "row"
  return(Which)
}

# internal helper checks if which ("row"/"column") inferred from a *side
# argument and the @which slot in a prebuilt HeatmapAnnotation match
annoWhichMatchCheck <- function(which, anno, context = "taxa") {
  if (!identical(which, anno@which)) {
    stop(
      context, " annotation side & HeatmapAnnotation `which` are not compatible"
    )
  }
}


# deprecated anno_tax_helper =================================================

# anno_tax input --> output possibilities:
# * NULL--> NULL,
# * list output of tax_anno() --> HeatmapAnnotation object,
# * HeatmapAnnotation object --> HeatmapAnnotation object (checked)
#
# ps is phyloseq extracted from heatmap data arg, if phyloseq or ps_extra
# side takes taxa_side argument as passed to heatmap function
#
# used inside cor_heatmap (when given phyloseq as data) and comp_heatmap (always)
old_anno_tax_helper <- function(anno_tax, ps, taxa, side) {
  warning("anno_tax argument is deprecated, use tax_anno arg instead")

  # infer row or column from side specification
  taxa_which <- annoWhichFromAnnoSide(side, argName = "taxa_side")

  # create taxa annotation object if "instructions" list given
  if (inherits(anno_tax, "list")) {
    if (identical(anno_tax$which, NA)) anno_tax$which <- taxa_which
    anno_args <- c(anno_tax, list(data = ps, taxa = taxa))
    anno_tax <- do.call(what = taxAnnotate, args = anno_args)
  }

  # check anno_tax suitable for taxa_which?
  if (methods::is(anno_tax, "HeatmapAnnotation")) {
    if (!identical(taxa_which, anno_tax@which)) {
      stop(
        "\nYou specified the `which` argument to anno_tax() as:\n\t'",
        anno_tax@which,
        "'\nYou specified the taxa_side argument to the heatmap as:\n\t'",
        side,
        "'\nThese are incompatible options.",
        "\n--> anno_tax which = 'row' matches taxa_side = 'left'/'right'",
        "\n--> anno_tax which = 'column' matches taxa_side = 'top'/'bottom'."
      )
    }
  } else {
    stop(
      call. = FALSE,
      "heatmap anno_tax argument must be one of the following:\n",
      "* NULL\n",
      "* list output of tax_anno()\n",
      "* class HeatmapAnnotation object\n\n",
      "However, it is class:\n",
      paste(class(anno_tax), collapse = " ")
    )
  }
  return(anno_tax)
}
