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
#' customAxis <- list(labels_rot = 0, at = c(0, 0.5, 1))
#'
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- sampleAnnotation(
#'   gap = grid::unit(2.5, "mm"),
#'   IBD = anno_sample(var = "ibd"),
#'   Dis. = anno_sample(var = "DiseaseState")
#' )
#'
#' # manually specify the prevalence barplot function by giving it data etc.
#' heatmapAnnoFunction <- fun(.data = psq, .side = "top", .samples = samples)
#'
#' # draw the annotation without a heatmap, you will never normally do this!
#' grid.newpage()
#' vp <- viewport(width = 0.65, height = 0.75)
#' pushViewport(vp)
#' draw(heatmapAnnoFunction)
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
  annots <- list(...)

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
  annoFun <- function(.data, .samples, .side, annos = annots, args = style) {
    # infer which and "auto" side for placing annotation name
    which <- annoWhichFromAnnoSide(.side, argName = ".side")
    if (identical(args[["annotation_name_side"]], "auto")) {
      args[["annotation_name_side"]] <- ifelse(
        test = which == "column", yes = "right", no = "bottom"
      )
    }

    # convert all functions to AnnotationFunctions
    for (aName in names(annos)) {
      ann <- annos[[aName]]
      if (inherits(ann, "function")) {
        annos[[aName]] <- ann(data = .data, samples = .samples)
      }
      if (!is.vector(annos[[aName]]) && !methods::is(annos[[aName]], "AnnotationFunction")) {
        stop("all arguments must be a vector or an AnnotationFunction object")
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
    if (identical(.samples, NULL)) {
      stop(".samples must not be NULL if .data given")
    }
    if (identical(.side, NULL)) stop(".side must not be NULL if .data given")
    out <- annoFun(.data, .samples, side = ifelse(is.null(.side), "top", .side))
  }
  return(out)
}

#' Helper to specify comp_heatmap annotation for other sample data
#'
#' Use this as an argument to sampleAnnotation(),
#' which itself is used by comp_heatmap as sample_anno argument.
#'
#' @inheritParams anno_tax_prev
#' @inheritParams ComplexHeatmap::anno_simple
#' @inheritDotParams ComplexHeatmap::anno_simple col na_col
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
#' @examples
#' library("ComplexHeatmap")
#' data("ibd_phylo", package = "corncob")
#' samples <- phyloseq::sample_names(psq)
#'
#' # makes a function that takes data, taxa and which (at minimum)
#' fun <- anno_sample(var = "ibd", col = c("ibd" = "red", "nonibd" = "white"))
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
#'
#' # change some options and specify the data up front
#' grid::grid.newpage()
#' pushViewport(vp)
#' anno_sample(
#'   data = psq, var = "DiseaseState", samples = samples, which = "column",
#'   border = TRUE, size = grid::unit(5, "cm"),
#'   col = c(UC = "red", CD = "green", nonIBD = "white", IBDundef = "grey")
#' ) %>%
#'   draw()
#'
#' # multi-pack
#' grid::grid.newpage()
#' pushViewport(vp)
#' anno_sample(
#'   data = psq, var = c("ibd", "DiseaseState"), samples = samples, which = "column",
#'   size = grid::unit(5, "cm"),
#'   col = c(
#'   UC = "red", CD = "green", nonIBD = "white", IBDundef = "grey",
#'   ibd = "black", nonibd = "white"
#'   )
#' ) %>%
#'   draw()
#'
#' # clear drawings
#' grid::grid.newpage()
anno_sample <- function(var,
                        fun = identity,
                        data = NULL,
                        samples = NULL
                        ) {
  stopifnot(inherits(var, "character"))
  stopifnot(inherits(fun, "function"))

  # create AnnotationFunction-making function
  FN <- function(data, samples) {
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
