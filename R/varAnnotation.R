#' Helper to specify a HeatmapAnnotation for variables in cor_heatmap
#'
#' @inheritParams taxAnnotation
#' @inheritParams ComplexHeatmap::HeatmapAnnotation
#'
#' @param ...
#' Name-value pairs where the names correspond to annotation names and values
#' are the output of variable annotation functions
#' such as anno_var_box(), or manually specified AnnotationFunction objects
#' @param .vars
#' OPTIONAL selection vector of variables (names, numbers or logical),
#' only set this if providing .data argument to override default
#' @param .side
#' OPTIONAL string, indicating the side for the variable annotations:
#' only set this to override default
#'
#' @return HeatmapAnnotation object
#' @export
varAnnotation <- function(...,
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
                          .vars = NULL,
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

  # create a function that only needs data, vars and side specification
  # which it will pass to each of the annotation functions
  annoFun <- function(.data, .vars, .side, annos = annotations, args = style) {
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
        annos[[annName]] <- ann(data = .data, vars = .vars, which = which)
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
    if (identical(.vars, NULL)) stop(".vars must not be NULL if .data given")
    if (identical(.side, NULL)) stop(".side must not be NULL if .data given")
    out <- annoFun(
      data = .data, vars = .vars, side = ifelse(is.null(.side), "top", .side)
    )
  }
  return(out)
}
