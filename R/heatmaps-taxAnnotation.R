# a function akin to HeatmapAnnotation, but it makes a heatmap annotation
# takes a named list of functions that generate AnnotationFunction objects, when given data and a side

#' Helper to specify a HeatmapAnnotation for taxa
#'
#' @inheritParams ComplexHeatmap::HeatmapAnnotation
#'
#' @param ...
#' Name-value pairs where the names correspond to annotation names and values
#' are the output of taxon annotation functions such as anno_tax_prev() or
#' manually specified AnnotationFunction objects
#' @param .side
#'
#' @param .data
#' OPTIONAL phyloseq or ps_extra,
#' only set this to override use of same data as in heatmap
#' @param .taxa
#' OPTIONAL selection vector of taxa (names, numbers or logical),
#' only set this if providing .data argument to override default
#' @param .side
#' OPTIONAL string, indicating the side the taxa annotation should be placed:
#' only set this to override default
#' @param annotation_name_gp
#' Graphic parameters for annotation names. Graphic parameters can be vectors.
#'
#' @return HeatmapAnnotation object
#' @export
#'
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' psq <- tax_filter(ibd_phylo, min_prevalence = 5)
#' psq <- tax_mutate(psq, Species = NULL)
#' psq <- tax_fix(psq)
#' psq <- tax_agg(psq, rank = "Family")
#' taxa <- tax_top(psq, n = 15, rank = "Family")
#'
#' customAxis <- list(labels_rot = 0, at = c(0, 0.5, 1))
#'
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- taxAnnotation(
#'   gap = grid::unit(2.5, "mm"),
#'   Prev. = anno_tax_prev(axis_param = customAxis, ylim = c(0, 1), extend = 0),
#'   `Prop. Abd.` = anno_tax_box(size = unit(40, "mm"), axis_param = customAxis),
#'   `Log10p Abd.` = anno_tax_density(type = "heatmap")
#' )
#'
#' # manually specify the prevalence barplot function by giving it data etc.
#' heatmapAnnoFunction <- fun(.data = psq, .side = "top", .taxa = taxa)
#'
#' # draw the annotation without a heatmap, you will never normally do this!
#' grid.newpage()
#' vp <- viewport(width = 0.65, height = 0.75)
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#'
#' # try again as a row annotation
#' grid.newpage()
#' pushViewport(vp)
#' draw(fun(.data = psq, .side = "right", .taxa = rev(taxa)))
taxAnnotation <- function(...,
                          name,
                          annotation_legend_param = list(),
                          show_legend = TRUE,
                          gp = grid::gpar(col = NA),
                          border = FALSE,
                          gap = grid::unit(2, "mm"),
                          show_annotation_name = TRUE,
                          annotation_label = NULL,
                          annotation_name_gp = grid::gpar(),
                          annotation_name_offset = NULL,
                          annotation_name_rot = NULL,
                          annotation_name_align = TRUE,
                          annotation_name_side = "auto",
                          .data = NULL,
                          .taxa = NULL,
                          .side = NULL) {
  annotations <- list(...)

  # set annotation style arguments
  style <- list(
    annotation_legend_param = annotation_legend_param,
    show_legend = show_legend,
    gp = gp,
    border = border,
    gap = gap,
    show_annotation_name = show_annotation_name,
    annotation_label = annotation_label,
    annotation_name_gp = annotation_name_gp,
    annotation_name_offset = annotation_name_offset,
    annotation_name_side = annotation_name_side,
    annotation_name_rot = annotation_name_rot,
    annotation_name_align = annotation_name_align
  )

  # create a function that only needs data, taxa and side specification
  # which it will pass to each of the annotation functions
  annoFun <- function(.data, .taxa, .side, annos = annotations, args = style) {
    # infer which and "auto" side for placing annotation name
    which <- annoWhichFromAnnoSide(.side, argName = ".side")
    if (identical(args[["annotation_name_side"]], "auto")) {
      args[["annotation_name_side"]] <- ifelse(
        test = which == "column", yes = "right", no = "bottom"
      )
    }

    # convert all functions to AnnotationFunctions
    for (annName in names(annos)) {
      ann <- annos[[annName]]
      if (inherits(ann, "function")) {
        annos[[annName]] <- ann(data = .data, taxa = .taxa, which = which)
      }
      if (!methods::is(annos[[annName]], "AnnotationFunction")) {
        stop("all arguments must be or generate an AnnotationFunction object")
      }
    }
    args <- c(annos, args, list(which = which))
    out <- do.call(what = ComplexHeatmap::HeatmapAnnotation, args = args)
    return(out)
  }

  # return function or call it if .data given
  if (identical(.data, NULL)) {
    out <- annoFun
  } else {
    if (identical(.taxa, NULL)) stop(".taxa must not be NULL if .data given")
    if (identical(.side, NULL)) stop(".side must not be NULL if .data given")
    out <- annoFun(
      .data = .data, .taxa = .taxa,
      .side = ifelse(is.null(.side), "right", .side)
    )
  }
  return(out)
}

# function that makes a function that takes a phyloseq or dataframe/matrix
# that creates a ComplexHeatmap AnnotationFunction object by calling anno_barplot

#' Helper to specify heatmap annotation for showing taxa prevalence as barplot
#'
#' Use this as an argument to taxAnnotation(),
#' which itself is used by cor_heatmap and comp_heatmap as tax_anno argument.
#'
#' @inheritParams ComplexHeatmap::anno_barplot
#' @inheritDotParams ComplexHeatmap::anno_barplot axis_param
#'
#' @param undetected
#' the value above which taxa are classed as detected/present in a sample
#' @param use_counts try to retrieve counts from data object?
#' @param size width or height as a grid unit object
#' @param data
#' OPTIONAL phyloseq or ps_extra,
#' only set this to override use of same data as in heatmap
#' @param taxa
#' OPTIONAL selection vector of taxa (names, numbers or logical),
#' only set this if providing data argument to override default
#' @param which
#' OPTIONAL indicating if it is a 'column' or a 'row' annotation,
#' only set this if providing data argument to override default
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' psq <- tax_filter(ibd_phylo, min_prevalence = 5)
#' psq <- tax_mutate(psq, Species = NULL)
#' psq <- tax_fix(psq)
#' psq <- tax_agg(psq, rank = "Family")
#' taxa <- tax_top(psq, n = 15, rank = "Family")
#'
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- anno_tax_prev()
#'
#' # manually specify the prevalence barplot function by giving it data etc.
#' heatmapAnnoFunction <- fun(data = psq, which = "row", taxa = taxa)
#'
#' # draw the barplot without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#'
#' grid::grid.newpage()
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#'
#' # let's change some style options and specify the data up front
#' grid::grid.newpage()
#' pushViewport(vp)
#' anno_tax_prev(
#'   data = psq, taxa = taxa, which = "column",
#'   gp = grid::gpar(fill = "red", lwd = 3, alpha = 0.5),
#'   border = FALSE, bar_width = 1
#' ) %>%
#'   draw()
#'
#' # clear drawings
#' grid::grid.newpage()
anno_tax_prev <- function(undetected = 0,
                          use_counts = TRUE,
                          size = grid::unit(20, "mm"),
                          baseline = 0,
                          border = TRUE,
                          bar_width = 0.6,
                          gp = grid::gpar(fill = "#CCCCCC"),
                          ylim = NULL,
                          extend = 0.05,
                          axis = TRUE,
                          ...,
                          data = NULL,
                          taxa = NULL,
                          which = NULL) {
  force(undetected)
  force(use_counts)
  .size <- size # to avoid recursive default argument reference error

  barArgs <- c(
    list(
      baseline = baseline, border = border, bar_width = bar_width,
      gp = gp, ylim = ylim, extend = extend, axis = axis
    ),
    list(...)
  )

  # create AnnotationFunction-making function
  FUN <- function(data, taxa, which, size = .size, barplotArgs = barArgs) {
    # create x from data
    if (isTRUE(use_counts)) data <- ps_counts(data)
    prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)

    barplotArgs[c("x", "which")] <- list(prevs, which)
    if (identical(which, "row")) barplotArgs$width <- size
    if (identical(which, "column")) barplotArgs$height <- size

    # make annotation function object
    out <- do.call(what = ComplexHeatmap::anno_barplot, args = barplotArgs)
    return(out)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    return(FUN)
  } else {
    if (identical(taxa, NULL)) stop("taxa must not be NULL if data given")
    return(
      FUN(data, taxa = taxa, which = ifelse(is.null(which), "row", which))
    )
  }
}

#' Helper to specify heatmap annotation for showing taxa abundance on boxplot
#'
#' Use this as an argument to taxAnnotation(),
#' which itself is used by cor_heatmap and comp_heatmap as tax_anno argument.
#'
#' @inheritParams ComplexHeatmap::anno_boxplot
#' @inheritParams anno_tax_prev
#' @inheritDotParams ComplexHeatmap::anno_boxplot axis_param
#'
#' @param only_detected
#' only plot values for samples where the taxon abundance is > undetected
#' @param trans
#' name of transformation suitable for tax_transform,
#' or a function calling tax_transform, and/or tax_scale,
#' (a function must take a phyloseq or ps_extra, and return one)
#' @param zero_replace
#' zero_replace value for for tax_transform, ignored if trans is a function
#' @param pointsize size of outlier points, as grid::unit() object
#' @param which
#' OPTIONAL indicating if it is a 'column' or a 'row' annotation,
#' only set this if providing data argument to override default
#' @param size width or height as a grid unit object
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#'
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' psq <- tax_filter(ibd_phylo, min_prevalence = 5)
#' psq <- tax_mutate(psq, Species = NULL)
#' psq <- tax_fix(psq)
#' psq <- tax_agg(psq, rank = "Family")
#' taxa <- tax_top(psq, n = 15, rank = "Family")
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- anno_tax_box()
#' # manually specify the prevalence barplot function by giving it data etc.
#' heatmapAnnoFunction <- fun(data = psq, which = "column", taxa = taxa)
#' # draw the barplot without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#' grid.newpage()
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#'
#' # let's change some style options and specify the data up front
#' grid::grid.newpage()
#' pushViewport(vp)
#' draw(anno_tax_box(
#'   data = psq, taxa = taxa, which = "row", pointsize = grid::unit(1, "mm"),
#'   gp = grid::gpar(fill = "red"), border = FALSE, box_width = 0.2
#' ))
#'
#' # clear drawings
#' grid::grid.newpage()
anno_tax_box <- function(undetected = 0,
                         only_detected = TRUE,
                         trans = "compositional",
                         zero_replace = 0,
                         use_counts = TRUE,
                         size = grid::unit(30, "mm"),
                         border = TRUE,
                         gp = grid::gpar(fill = "#CCCCCC"),
                         ylim = NULL,
                         extend = 0.05,
                         outline = TRUE,
                         box_width = 0.6,
                         pch = 1,
                         pointsize = grid::unit(0.5, "mm"),
                         axis = TRUE,
                         ...,
                         data = NULL,
                         taxa = NULL,
                         which = NULL) {
  force(undetected)
  force(only_detected)
  force(trans)
  force(use_counts)
  .size <- size # to avoid recursive default argument reference error

  boxArgs <- c(
    list(
      border = border, gp = gp, ylim = ylim, extend = extend,
      outline = outline, box_width = box_width, pch = pch,
      size = pointsize, axis = axis
    ),
    list(...)
  )

  # create AnnotationFunction-making function
  FUN <- function(data, taxa, which, size = .size, boxplotArgs = boxArgs) {
    # get taxon abundances (with undetected replaced with NaN, optionally)
    x <- taxCalcAbund(
      data = data, use_counts = use_counts, taxa = taxa, trans = trans,
      zero_replace = zero_replace, undetected = undetected,
      only_detected = only_detected
    )
    if (identical(which, "row")) x <- t(x)

    boxplotArgs[c("x", "which")] <- list(x, which)
    if (identical(which, "row")) boxplotArgs$width <- size
    if (identical(which, "column")) boxplotArgs$height <- size

    # make annotation function object
    out <- do.call(what = ComplexHeatmap::anno_boxplot, args = boxplotArgs)
    return(out)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    return(FUN)
  } else {
    if (identical(taxa, NULL)) stop("taxa must not be NULL if data given")
    return(
      FUN(data, taxa = taxa, which = ifelse(is.null(which), "row", which))
    )
  }
}

#' Helper to specify heatmap annotation for showing taxa abundance density plot
#'
#' Use this as an argument to taxAnnotation(),
#' which itself is used by cor_heatmap and comp_heatmap as tax_anno argument.
#'
#' @inheritParams ComplexHeatmap::anno_density
#' @inheritParams anno_tax_box
#' @inheritDotParams ComplexHeatmap::anno_density axis_param
#'
#' @param size width or height as a grid unit object
#' @param which
#' OPTIONAL indicating if it is a 'column' or a 'row' annotation,
#' only set this if providing data argument to override default
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#'
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' psq <- tax_filter(ibd_phylo, min_prevalence = 5)
#' psq <- tax_mutate(psq, Species = NULL)
#' psq <- tax_fix(psq)
#' psq <- tax_agg(psq, rank = "Family")
#' taxa <- tax_top(psq, n = 15, rank = "Family")
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- anno_tax_density()
#' # manually specify the density plot function by giving it data etc.
#' heatmapAnnoFunction <- fun(data = psq, which = "column", taxa = taxa)
#'
#' # draw the density plot without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#' grid.newpage()
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#'
#' # let's change some style options and specify the data up front
#' grid.newpage()
#' pushViewport(vp)
#' draw(anno_tax_density(
#'   data = psq, taxa = taxa, which = "row",
#'   gp = grid::gpar(fill = "red"), border = FALSE
#' ))
#'
#' # heatmap type, with alternative transformation and axis_param
#' grid.newpage()
#' pushViewport(vp)
#' draw(anno_tax_density(
#'   data = psq, taxa = taxa, which = "row", type = "heatmap",
#'   trans = "log2", zero_replace = "halfmin", axis_param = list(labels_rot = 0)
#' ))
#'
#' grid.newpage()
anno_tax_density <- function(undetected = 0,
                             only_detected = TRUE,
                             trans = "log10p",
                             zero_replace = 0,
                             use_counts = TRUE,
                             size = grid::unit(30, "mm"),
                             type = c("lines", "violin", "heatmap"),
                             xlim = NULL,
                             heatmap_colors = c("white", "forestgreen"),
                             joyplot_scale = 1.5,
                             border = TRUE,
                             gp = grid::gpar(fill = "lightgrey"),
                             axis = TRUE,
                             ...,
                             data = NULL,
                             taxa = NULL,
                             which = NULL) {
  force(undetected)
  force(only_detected)
  force(trans)
  force(use_counts)
  .size <- size # to avoid recursive default argument reference error

  densityArgs <- c(
    list(
      type = type, xlim = xlim, heatmap_colors = heatmap_colors,
      joyplot_scale = joyplot_scale, border = border, gp = gp, axis = axis
    ),
    list(...)
  )

  # create AnnotationFunction-making function
  FUN <- function(data, taxa, which, size = .size, args = densityArgs) {
    # get taxon abundances (with undetected replaced with NaN, optionally)
    x <- taxCalcAbund(
      data = data, use_counts = use_counts, taxa = taxa, trans = trans,
      zero_replace = zero_replace, undetected = undetected,
      only_detected = only_detected
    )
    if (identical(which, "row")) x <- t(x)

    args[c("x", "which")] <- list(x, which)
    if (identical(which, "row")) args$width <- size
    if (identical(which, "column")) args$height <- size

    # make annotation function object
    out <- do.call(what = ComplexHeatmap::anno_density, args = args)
    return(out)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    return(FUN)
  } else {
    if (identical(taxa, NULL)) stop("taxa must not be NULL if data given")
    return(
      FUN(data, taxa = taxa, which = ifelse(is.null(which), "row", which))
    )
  }
}

#' Internal helper for anno_tax_box and anno_tax_density
#'
#' Get taxon abundance matrix (with undetected replaced with NaN, optionally)
#'
#' @param data ps_extra or phyloseq
#' @param use_counts use count data from data if possible
#' @param taxa vector of taxa to return abundances for
#' @param trans transformation for tax_transform
#' @param zero_replace zero_replace for tax_transform
#' @param undetected
#' the value above which taxa are classed as detected/present in a sample
#' @param only_detected
#' replace values with NaN for samples where taxon abundance is <= undetected
#'
#' @return numeric matrix, taxa as columns
#' @noRd
taxCalcAbund <- function(data,
                         use_counts,
                         taxa,
                         trans,
                         zero_replace,
                         undetected = NaN,
                         only_detected = FALSE) {
  if (isTRUE(use_counts)) data <- ps_counts(data)

  # mark values in otu matrix that are detected
  if (isTRUE(only_detected)) {
    otu <- otu_get(data)[, taxa, drop = FALSE]
    keep <- otu > undetected
  } else {
    # everything is kept if only_detected is FALSE
    keep <- TRUE
  }

  # transform and subset data
  if (inherits(trans, "function")) {
    data <- trans(data)
  } else if (inherits(trans, "character")) {
    data <- tax_transform(data, trans = trans, zero_replace = zero_replace, chain = TRUE)
  } else if (!is.null(trans)) {
    stop("trans must be transform name, a function, or NULL")
  }
  otu <- otu_get(data)[, taxa, drop = FALSE]

  # replace undetected with NaN to avoid them showing in anno_tax_box
  otu[!keep] <- NaN

  return(otu)
}
