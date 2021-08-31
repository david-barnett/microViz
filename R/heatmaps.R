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
#' @param taxa_side controls heatmap orientation and where any anno_tax annotations are placed (top/bottom/left/right)
#' @param cor correlation coefficient. pearson/kendall/spearman, can be abbreviated (used as legend title)
#' @param cor_use passed to cor(use = cor_use)
#' @param colors output of heat_palette() to set heatmap fill color scheme
#' @param numbers output of heat_numbers() to draw numbers on heatmap cells
#' @param seriation_method method to order the rows (in seriation::seriate)
#' @param seriation_dist distance to use in seriation_method (if needed)
#' @param seriation_method_col method to order the columns (in seriation::seriate)
#' @param seriation_dist_col distance to use in seriation_method_col (if needed)
#' @param tax_transform transformation applied to otu_table before correlating (and BEFORE selection of taxa)
#' @param gridlines list output of heat_grid() for setting gridline style
#' @param var_fun character: name of a function to be applied (columns) to a matrix of vars before correlating (but not used in any variable annotations)
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
#' # you can transform the variables before correlating by giving var_fun a function name
#' # this does not affect the data used for annotations
#' cor_heatmap(
#'   psq, taxa,
#'   anno_vars = var_anno(), anno_tax = tax_anno(undetected = 50),
#'   var_fun = "exp"
#' )
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
#' p3 <- cor_heatmap(psq, taxa,
#'   taxa_side = "top", anno_tax = tax_anno(undetected = 50)
#' )
#' p3
#'
#' # or rotated with taxa annotations at bottom
#' p4 <- cor_heatmap(psq, taxa,
#'   taxa_side = "bottom", anno_tax = tax_anno(undetected = 50)
#' )
#' p4
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
                        taxa_side = "right",
                        seriation_method = "OLO_ward",
                        seriation_dist = "euclidean",
                        seriation_method_col = seriation_method,
                        seriation_dist_col = seriation_dist,
                        tax_transform = "identity",
                        var_fun = "identity",
                        gridlines = heat_grid(lwd = 0.5),
                        ...) {
  if (inherits(data, "data.frame")) {
    otu_mat <- NULL # causes cor to only use x (meta_mat)
    meta_mat <- df_to_numeric_matrix(data, vars = vars, trans_fun = var_fun)
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

      taxa_which <- taxa_which_from_taxa_side(taxa_side)
      # create taxa annotation object if "instructions" given
      anno_tax <- anno_tax_helper(anno_tax, ps = ps, taxa = taxa, side = taxa_side)
    }
    # handle sample metadata
    samdat <- phyloseq::sample_data(ps)
    meta_mat <- df_to_numeric_matrix(samdat, vars = vars, trans_fun = var_fun)
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
    name = cor,
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
  args[[paste0(taxa_side, "_annotation")]] <- anno_tax

  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    # TODO allow bottom, top, left and right in addition to rows and columns
    if (identical(taxa_which, "row")) {
      args[["top_annotation"]] <- anno_vars
      args[["mat"]] <- t(args[["mat"]])
      args[["numbers_mat"]] <- t(args[["numbers_mat"]])
    } else if (identical(taxa_which, "column")) {
      args[["right_annotation"]] <- anno_vars
    }
  } else if (inherits(data, "data.frame") && !identical(anno_vars, NULL)) {
    # TODO add vars_side arg allow anno_vars side instead of which
    if (anno_vars@which == "row") args[["right_annotation"]] <- anno_vars
    if (anno_vars@which == "column") args[["top_annotation"]] <- anno_vars
  }
  # use extra args (overwriting any set including with anno_tax [relevant if anno_tax = NULL])
  dots <- list(...)
  args[names(dots)] <- dots

  p <- do.call(viz_heatmap, args = args)
  p
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
#' @param ... extra args, passed to viz_heatmap internal function
#'
#' @export
#' @examples
#' data("dietswap", package = "microbiome")
#' psq <- tax_filter(dietswap, min_prevalence = 1 / 10, min_sample_abundance = 1 / 10)
#' psq <- tax_agg(psq, "Genus")
#'
#' set.seed(123)
#' taxa <- sample(microbiome::top_taxa(ps_get(psq))[1:50], size = 30)
#'
#' p <- psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(taxa = taxa, anno_tax = tax_anno(undetected = 50))
#' p
#'
#' # set the colour range yourself
#' psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(
#'     taxa = taxa, anno_tax = tax_anno(undetected = 50),
#'     colors = heat_palette(palette = "Greens", rev = TRUE, range = 0:10)
#'   )
#' psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(
#'     taxa = taxa, anno_tax = tax_anno(undetected = 50),
#'     colors = heat_palette(palette = "Green-Orange", range = 0:5, sym = TRUE)
#'   )
#'
#' # supply a different colour palette to heat_palette (match breaks to length)
#' psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(
#'     taxa = taxa, anno_tax = tax_anno(undetected = 50),
#'     colors = heat_palette(palette = viridis::turbo(12), breaks = 12)
#'   )
#'
#' # you can place the legend at the bottom, but it is a little complicated
#' p <- psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(
#'     taxa = taxa, anno_tax = tax_anno(undetected = 50), name = "auto",
#'     heatmap_legend_param = list(direction = "horizontal", title_position = "lefttop")
#'   )
#' ComplexHeatmap::draw(p,
#'   heatmap_legend_side = "bottom", adjust_annotation_extension = FALSE
#' )
#'
#' # rotate plot to have taxa as columns, annotated at the bottom
#' p2 <- psq %>%
#'   tax_transform("clr") %>%
#'   comp_heatmap(
#'     taxa = taxa, taxa_side = "bottom", anno_tax = tax_anno(undetected = 50)
#'   )
#' p2
#'
#' # log2 transform data before plotting and automatic naming of scale
#' psq %>%
#'   tax_transform("log2", zero_replace = 1) %>%
#'   comp_heatmap(taxa, anno_tax = tax_anno(undetected = 50), name = "auto")
comp_heatmap <- function(data,
                         taxa = phyloseq::taxa_names(ps_get(data)),
                         samples = phyloseq::sample_names(ps_get(data)),
                         anno_tax = NULL,
                         anno_samples = NULL,
                         colors = heat_palette(palette = "Greens", rev = TRUE),
                         numbers = NULL, # or list made by heat_numbers() or a function in format of a ComplexHeatmap cell_fun
                         taxa_side = "right",
                         # stuff just passed to viz_heatmap
                         seriation_method = "OLO_ward",
                         seriation_dist = "euclidean",
                         seriation_method_col = seriation_method,
                         seriation_dist_col = seriation_dist,
                         # numbers
                         tax_transform_numbers = "identity",
                         tax_scale_numbers = "neither",
                         gridlines = heat_grid(lwd = 0.1, col = "black"),
                         name = "mat",
                         ... # passed to viz_heatmap
) {
  # get otu_table data (used for colours and seriation)
  # any transformation must be done in advance
  otu_mat <- otu_get(data)
  otu_mat <- otu_mat[samples, taxa, drop = FALSE]

  # get automatic name for colourbar legend
  if (identical(name, "auto")) name <- info_get(data)[["tax_transform"]]

  # get phyloseq with stored counts if needed for annotation or numbers
  if (!identical(numbers, NULL) || !identical(anno_tax, NULL)) {
    psCounts <- ps_counts(data, warn = TRUE)
  }

  # create heatmap annotation object for taxa if "instructions" given
  if (!identical(anno_tax, NULL)) {
    anno_tax <- anno_tax_helper(
      anno_tax = anno_tax, ps = psCounts, taxa = taxa, side = taxa_side
    )
  }

  if (identical(numbers, NULL)) {
    # avoid computation if otu_numbers won't be shown anyway
    otu_numbers <- otu_mat
  } else {
    # used for numbers only
    otu_numbers <- otu_get(tax_transform(psCounts, trans = tax_transform_numbers))
    otu_numbers <- tax_scale(data = otu_numbers, do = tax_scale_numbers)
    otu_numbers <- otu_numbers[samples, taxa, drop = FALSE]
  }

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

  taxa_which <- taxa_which_from_taxa_side(taxa_side)
  args[["mat"]] <- otu_mat
  args[["numbers_mat"]] <- otu_numbers
  args[[paste0(taxa_side, "_annotation")]] <- anno_tax
  sam_which <- ifelse(taxa_which == "column", yes = "row", no = "column")
  args[[paste0("show_", sam_which, "_names")]] <- FALSE

  # rotate matrices if taxa are rows
  if (identical(taxa_which, "row")) {
    args[["mat"]] <- t(args[["mat"]])
    args[["numbers_mat"]] <- t(args[["numbers_mat"]])
  }
  # use extra args (overwriting any set including with anno_tax [relevant if anno_tax = NULL])
  dots <- list(...)
  args[names(dots)] <- dots

  p <- do.call(viz_heatmap, args = args)
  p
}

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
heat_palette <- function(palette = "Greens", breaks = 5, range = NA, rev = FALSE, sym = FALSE) {
  n_breaks <- if (length(breaks) > 1) length(breaks) else breaks

  # palette arg of length 1 must be valid colorspace::hcl_palette()
  validDiverging <- rownames(colorspace::hcl_palettes(type = "diverging"))
  validSequential <- rownames(colorspace::hcl_palettes(type = "sequential"))
  # convert this to a set of colours n_breaks long
  if (length(palette) == 1) {
    if (palette %in% validDiverging) {
      palette <- colorspace::diverge_hcl(palette = palette, n = n_breaks)
    } else if (palette %in% validSequential) {
      palette <- colorspace::sequential_hcl(palette = palette, n = n_breaks)
    } else {
      stop(
        call. = FALSE,
        "\nheat_palette() palette argument must be either a:\n",
        "- vector of colours, or\n",
        "- palette name from colorspace::hcl_palettes(type = 'diverging')",
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
heat_numbers <- function(decimals = 1,
                         fontsize = 7,
                         col = "black",
                         fontface = "plain",
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
# with helpful error for invalid argument, used inside cor_heatmap (and comp_heatmap)
# taxa_which is used for rotating heatmap matrix and checking clashes with other annotations
# taxa_side actually specifies the side taxa annotations are drawn on
taxa_which_from_taxa_side <- function(taxa_side) {
  taxa_side_options <- c("top", "bottom", "left", "right")
  if (!inherits(taxa_side, "character") || length(taxa_side) != 1 || !taxa_side %in% taxa_side_options) {
    stop("taxa_side must be one of: '", paste(taxa_side_options, collapse = "' / '"), "'\nnot: ", paste(taxa_side, collapse = " "))
  }
  if (taxa_side %in% c("top", "bottom")) taxa_which <- "column"
  if (taxa_side %in% c("left", "right")) taxa_which <- "row"
  return(taxa_which)
}

# anno_tax input --> output possibilities:
# * NULL--> NULL,
# * list output of tax_anno() --> HeatmapAnnotation object,
# * HeatmapAnnotation object --> HeatmapAnnotation object (checked)
#
# ps is phyloseq extracted from heatmap data arg, if phyloseq or ps_extra
# side takes taxa_side argument as passed to heatmap function
#
# used inside cor_heatmap (when given phyloseq as data) and comp_heatmap (always)
anno_tax_helper <- function(anno_tax, ps, taxa, side) {
  # infer row or column from side specification
  taxa_which <- taxa_which_from_taxa_side(side)

  # create taxa annotation object if "instructions" list given
  if (inherits(anno_tax, "list")) {
    if (identical(anno_tax$which, NA)) anno_tax$which <- taxa_which
    anno_args <- c(anno_tax, list(data = ps, taxa = taxa))
    anno_tax <- do.call(what = taxAnnotation, args = anno_args)
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
