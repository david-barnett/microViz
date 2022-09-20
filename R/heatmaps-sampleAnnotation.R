#' Helper to specify a HeatmapAnnotation for variables in cor_heatmap
#'
#' @inheritParams varAnnotation
#' @inheritParams ComplexHeatmap::HeatmapAnnotation
#'
#' @param ...
#' Name-value pairs where the names correspond to annotation names and values
#' are the output of sample annotation functions
#' such as anno_sample(), or manually specified AnnotationFunction objects
#' @param .samples
#' OPTIONAL selection vector of sample names,
#' only set this if providing .data argument to override default
#' @param .side
#' OPTIONAL string, indicating the side for the variable annotations:
#' only set this to override default
#'
#' @return HeatmapAnnotation object
#' @seealso [taxAnnotation()]
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
#' samples <- phyloseq::sample_names(psq)
#'
#' set.seed(42) # random colours used in first example
#' # sampleAnnotation returns a function that takes data, samples, and which
#' fun <- sampleAnnotation(
#'   gap = grid::unit(2.5, "mm"),
#'   Dis1 = anno_sample(var = "DiseaseState"),
#'   IBD = anno_sample_cat(var = "ibd"),
#'   Dis2 = anno_sample_cat(var = "DiseaseState", col = 1:4)
#' )
#'
#' # manually specify the sample annotation function by giving it data etc.
#' heatmapAnnoFunction <- fun(.data = psq, .side = "top", .samples = samples)
#'
#' # draw the annotation without a heatmap, you will never normally do this!
#' grid.newpage()
#' vp <- viewport(width = 0.65, height = 0.75)
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#' pushViewport(viewport(x = 0.7, y = 0.6))
#' draw(attr(heatmapAnnoFunction, "Legends"))
sampleAnnotation <- function(...,
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
                             annotation_name_align = FALSE,
                             annotation_name_side = "auto",
                             .data = NULL,
                             .samples = NULL,
                             .side = NULL) {
  anns <- list(...)

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

  # create a function that only needs data, vars and side specification
  # which it will pass to each of the annotation functions
  annoFun <- function(.data, .samples, .side, annos = anns, args = style) {
    # infer which and "auto" side for placing annotation name
    which <- annoWhichFromAnnoSide(.side, argName = ".side")
    if (identical(args[["annotation_name_side"]], "auto")) {
      args[["annotation_name_side"]] <- ifelse(
        test = which == "column", yes = "right", no = "bottom"
      )
    }

    # convert all functions to AnnotationFunctions
    for (a in names(annos)) {
      ann <- annos[[a]]
      if (inherits(ann, "function")) {
        annos[[a]] <- ann(data = .data, samples = .samples, which = which)
      }
      if (!is.vector(annos[[a]]) && !methods::is(annos[[a]], "AnnotationFunction")) {
        stop(
          "all arguments must be either:\n",
          " - a vector or an AnnotationFunction object\n",
          " - a function that generates one (e.g. output of anno_simple)"
        )
      }
    }
    args <- c(annos, args, list(which = which))
    out <- do.call(what = ComplexHeatmap::HeatmapAnnotation, args = args)

    # collect legends
    legs <- sapply(annos, function(a) attr(a, "Legend"))
    noLegs <- sapply(legs, is.null)
    if (!all(noLegs)) {
      attr(out, "Legends") <- ComplexHeatmap::packLegend(list = legs[!noLegs])
    }
    return(out)
  }

  # return function or call it if .data given
  if (identical(.data, NULL)) {
    out <- annoFun
  } else {
    if (identical(.samples, NULL)) {
      stop(".samples must not be NULL if .data given")
    }
    if (identical(.side, NULL)) stop(".side must not be NULL if .data given")
    out <- annoFun(.data, .samples, side = ifelse(is.null(.side), "top", .side))
  }
  return(out)
}

#' Helper to specify comp_heatmap annotation for categorical sample data
#'
#' Use this as an argument to sampleAnnotation(),
#' which itself is used by comp_heatmap() as sample_anno argument.
#'
#' @inheritParams anno_tax_prev
#' @inheritParams anno_cat
#' @inheritParams ComplexHeatmap::anno_simple
#' @inheritDotParams anno_cat
#'
#' @param var name of variable to use for annotation data
#' @param renamer function to rename levels of variable `var`
#' @param samples
#' OPTIONAL selection vector of sample names,
#' only set this if providing data argument to override default
#'
#' @return vector of values
#' @export
#'
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' psq <- ibd_phylo
#' samples <- phyloseq::sample_names(psq)
#'
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- anno_sample_cat(var = "ibd")
#'
#' # manually specify the prevalence barplot function by giving it data etc.
#' heatmapAnnoFunction <- fun(data = psq, which = "row", samples = samples)
#'
#' # draw the barplot without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#'
#' grid::grid.newpage()
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
#' # A legend is attached by default to anno_cat() output, let's plot that.
#' pushViewport(viewport(x = 0.75))
#' draw(attr(heatmapAnnoFunction, "Legend"))
#'
#' # change some options and specify the data up front
#' grid::grid.newpage()
#' pushViewport(vp)
#' anno_sample_cat(
#'   data = psq, var = "DiseaseState", samples = samples, which = "column",
#'   size = grid::unit(5, "cm"), col = distinct_palette(pal = "kelly")
#' ) %>%
#'   draw()
anno_sample_cat <- function(var,
                            col = distinct_palette(),
                            renamer = identity,
                            size = grid::unit(5, "mm"),
                            legend = TRUE,
                            legend_title = "",
                            box_col = "white",
                            box_lwd = 0.5,
                            border_col = NA,
                            border_lwd = 1,
                            data = NULL,
                            samples = NULL,
                            which = NULL,
                            ...) {
  stopifnot(inherits(var, "character"))
  stopifnot(inherits(renamer, "function"))
  stopifnot(inherits(legend_title, "character"))
  .size <- size # to avoid recursive default argument reference error

  # other arguments for anno_cat
  Args <- c(
    list(
      col = col, renamer = renamer, legend = legend,
      box_col = box_col, box_lwd = box_lwd,
      border_col = border_col, border_lwd = border_lwd
    ),
    list(...)
  )

  # create AnnotationFunction-making function
  FN <- function(data, samples, which, args = Args) {
    # extract (sample)data to matrix
    if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
      data <- samdatAsDataframe(ps_get(data))
    }
    if (inherits(data, "data.frame") || inherits(data, "matrix")) {
      if (!var %in% colnames(data)) stop(var, " is not a variable in data")
      x <- data[samples, var, drop = TRUE]
    } else {
      stop("data must be phyloseq/ps_extra or data.frame/matrix")
    }

    args[c("x", "which")] <- list(x, which)
    args$legend_title <- legend_title
    if (identical(which, "row")) args$width <- size
    if (identical(which, "column")) args$height <- size

    # make annotation function object
    out <- do.call(what = anno_cat, args = args)
    return(out)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    o <- FN
  } else {
    if (identical(samples, NULL)) stop("samples must not be NULL if data given")
    o <- FN(data, samples, which = ifelse(is.null(which), "column", which))
  }
  return(o)
}

#' Create colored rectangle annotations for categorical data
#'
#' Similar to anno_simple but with individual boxes!
#'
#' @param x data vector, treated as categorical
#' @param renamer function renaming variable values for legend
#' @param width grid unit object or NULL
#' @param height grid unit object or NULL
#' @param col
#' colors vector, at least as long as unique(x), optionally named by x levels
#' @param box_col colour of boxes around individual cells
#' @param box_lwd line width of boxes around individual cells
#' @param border_col colour of border around all cells
#' @param border_lwd line width of border around all cells
#' @param legend
#' generate legend for this annotation
#' (attached as attribute of heatmap, and not automatically included in plot)
#' @param legend_title title for legend, if drawn
#'
#' @inheritParams ComplexHeatmap::anno_simple
#'
#' @return AnnotationFunction
#' @export
#'
#' @examples
#' library(ComplexHeatmap)
#' # draw the annotation without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#'
#' grid::grid.newpage()
#' pushViewport(vp)
#' cats <- letters[1:4]
#' draw(anno_cat(cats, which = "row"))
#'
#' grid::grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_cat(
#'     x = cats, col = structure(names = cats, 1:4), which = "column",
#'     box_col = "black", box_lwd = 5
#'   )
#' )
#'
#' # developer note #
#' # list of annotations can be split and ordered (adding NULL makes a list)
#' # https://jokergoo.github.io/ComplexHeatmap-reference/book/a-list-of-heatmaps.html
#' # (section #4.8 concatenate-only-the-annotations)
#' grid::grid.newpage()
#' pushViewport(vp)
#' annoList <- rowAnnotation(
#'   hi = anno_cat(cats, which = "row", border_col = "black")
#' ) +
#'   NULL
#' draw(object = annoList, row_split = c(1, 1:3), row_order = 4:1)
#' pushViewport(viewport(x = 0.6))
#' draw(anno_cat(cats, "row", legend_title = "abcd") %>% attr("Legend"))
anno_cat <- function(x,
                     which,
                     renamer = identity,
                     col = distinct_palette(),
                     width = NULL,
                     height = NULL,
                     box_col = "white",
                     box_lwd = 0.5,
                     border_col = NA,
                     border_lwd = 1,
                     legend = TRUE,
                     legend_title = "") {
  if (!inherits(x, "character")) {
    warning("coercing non-character anno_cat annotation data to character")
    x <- as.character(x)
  }
  gp <- grid::gpar(col = box_col, lwd = box_lwd)
  border_gp <- grid::gpar(lwd = border_lwd, col = border_col, fill = NA)

  # process colours, checking against data to ensure all levels have a colour
  col <- anno_catColors(x = x, col = col)

  # apply renamer: mostly/only? relevant for legend
  x <- renamer(x)
  names(col) <- renamer(names(col))

  # convert list of name-color pairs to special class of object
  colorMap <- ComplexHeatmap::ColorMapping(colors = col)

  # function to draw annotation
  fun <- function(index, k, n) {
    n <- length(index)

    # set up viewport creating helper function
    if (identical(which, "column")) {
      vpMaker <- function(i, n) {
        grid::viewport(
          x = i / n, width = 1 / n, just = "right", default.units = "npc"
        )
      }
    } else if (identical(which, "row")) {
      vpMaker <- function(i, n) {
        grid::viewport(
          y = 1 - i / n, height = 1 / n, just = "bottom", default.units = "npc"
        )
      }
    } else {
      stop("which must be 'column' or 'row'")
    }
    # draw the cells
    for (i in seq_along(index)) {
      grid::pushViewport(vpMaker(i = i, n = n))
      # update gpar with cell-specific fill
      fill <- ComplexHeatmap::map_to_colors(colorMap, x[index[i]])
      gp <- c(gp, list(fill = fill))
      class(gp) <- "gpar"

      grid::pushViewport(grid::viewport())
      grid::grid.rect(gp = gp)
      grid::popViewport()
      grid::popViewport()
    }
    # draw the border (NA col makes invisible)
    grid::grid.rect(gp = border_gp)
  }

  # breaking "subsettable" argument name change
  annoFun <- if (utils::packageVersion("ComplexHeatmap") > 2.11) {
    ComplexHeatmap::AnnotationFunction(
      fun = fun, var_import = list(x, colorMap, gp, border_gp),
      which = which, width = width, height = height,
      subsettable = TRUE, n = length(x)
    )
  } else {
    ComplexHeatmap::AnnotationFunction(
      fun = fun, var_import = list(x, colorMap, gp, border_gp),
      which = which, width = width, height = height,
      subsetable = TRUE, n = length(x)
    )
  }

  # create legend and attach as an attribute (shouldn't interfere with class)
  if (isTRUE(legend)) {
    attr(annoFun, which = "Legend") <- anno_cat_legend(
      col = col, legend_gp = gp, title = legend_title
    )
  }

  return(annoFun)
}

#' Convenience function for generating a legend for anno_cat annotations.
#'
#' @param col
#' vector of colors, named by all levels of data (e.g. x) or not named
#' @param x
#' optional: vector of data to pair with unnamed col or check against named col
#' @param title title of legend
#' @param renamer function applied to generate labels: from names(col) or levels of x
#'
#' @inheritDotParams
#' ComplexHeatmap::Legend labels nrow ncol by_row grid_height grid_width gap
#' row_gap labels_gp labels_rot border type direction
#' title_gp title_position title_gap
#'
#' @return a ComplexHeatmap Legend class object
#' @export
#'
#' @examples
#' grid::grid.newpage()
#' ComplexHeatmap::draw(
#'   anno_cat_legend(
#'     col = c("ibd" = "blue", "nonibd" = "grey90"),
#'     renamer = toupper, title = "Hi there, I'm a title"
#'   )
#' )
anno_cat_legend <- function(col,
                            x = NULL,
                            renamer = identity,
                            title = "",
                            ...) {
  if (!is.null(x)) col <- anno_catColors(x = x, col = col)
  args <- c(list(at = names(col), title = title), list(...))
  args[["at"]] <- renamer(args[["at"]])
  args[["legend_gp"]] <- c(list(fill = col), args[["legend_gp"]])
  class(args[["legend_gp"]]) <- "gpar"
  leg <- do.call(ComplexHeatmap::Legend, args = args)
  return(leg)
}

# internal helper
# x is vector of categorical data
# col is vector of colors, named by all levels of x or not named
anno_catColors <- function(x, col) {
  # ensure col is suitable color scheme for x data
  xlevels <- unique(x)
  if (length(col) < length(xlevels)) {
    stop(
      length(col), " colors provided for ", length(xlevels), " categories\n",
      paste(utils::head(xlevels, 6), collapse = ", "), " ..."
    )
  }
  if (is.null(names(col))) {
    col <- col[seq_len(length(xlevels))]
    names(col) <- xlevels
  } else if (!all(xlevels %in% names(col))) {
    stop(
      "some levels of data are missing from color names:\n",
      paste(xlevels[!xlevels %in% names(col)], collapse = "\n")
    )
  }
  return(col)
}

#' Helper to specify simple comp_heatmap annotation for other sample data
#'
#' @description
#' Use this as an argument to sampleAnnotation(),
#' which itself is used by comp_heatmap() as sample_anno argument.
#'
#' This creates a vector, which sampleAnnotation() interprets as a
#' simple annotation, so then you set colours and legend parameters
#' for each simple annotation as further arguments in sampleAnnotation.
#'
#'
#' @inheritParams anno_tax_prev
#' @inheritParams ComplexHeatmap::anno_simple
#'
#' @param var name of variable to use for annotation data
#' @param fun function to transform variable `var`
#' @param samples
#' OPTIONAL selection vector of sample names,
#' only set this if providing data argument to override default
#'
#' @return vector of values
#' @export
#'
#' @seealso [sampleAnnotation()]
#'
#' @examples
#' # see `?sampleAnnotation()`
anno_sample <- function(var,
                        fun = identity,
                        data = NULL,
                        samples = NULL) {
  stopifnot(inherits(var, "character"))
  stopifnot(inherits(fun, "function"))

  # create AnnotationFunction-making function
  FN <- function(data, samples, ...) {
    # extract (sample)data to matrix
    if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
      data <- samdatAsDataframe(ps_get(data))
    }
    if (inherits(data, "data.frame") || inherits(data, "matrix")) {
      x <- fun(data[, var, drop = TRUE])
    } else {
      stop("data must be phyloseq/ps_extra or data.frame/matrix")
    }
    return(x)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    o <- FN
  } else {
    if (identical(samples, NULL)) stop("samples must not be NULL if data given")
    o <- FN(data, samples)
  }
  return(o)
}
