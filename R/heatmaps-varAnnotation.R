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
#' @seealso [taxAnnotation()]
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


#' Helper to specify heatmap annotation for showing variable distributions
#'
#' Use this as an argument to varAnnotation(),
#' which itself is used by cor_heatmap as var_anno() argument.
#'
#' @inheritParams anno_tax_box
#' @inheritParams ComplexHeatmap::anno_boxplot
#' @inheritDotParams ComplexHeatmap::anno_boxplot axis_param
#'
#' @param fun function applied to all variables, with apply()
#' @param vars
#' OPTIONAL selection vector of variable names,
#' only set this if providing data argument to override default
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#'
#' @examples
#' library(ComplexHeatmap)
#' set.seed(123)
#' fakeData <- as.data.frame.matrix(matrix(rnorm(500, 10, 3), ncol = 10))
#' names(fakeData) <- paste0("var_", 1:10)
#'
#' # draw the boxplot without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_box(data = fakeData, vars = names(fakeData), which = "column")
#' )
#'
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_box(
#'     data = fakeData, fun = function(x) log(x + 1),
#'     vars = rev(names(fakeData)),
#'     which = "row"
#'   )
#' )
anno_var_box <- function(fun = identity,
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
                         vars = NULL,
                         which = NULL) {
  force(fun)
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
  FN <- function(data, vars, which, size = .size, boxplotArgs = boxArgs) {
    # extract (sample)data to matrix
    if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
      data <- samdatAsDataframe(ps_get(data))
    }
    if (inherits(data, "data.frame") || inherits(data, "matrix")) {
      x <- df_to_numeric_matrix(data, vars = vars, trans_fun = fun)
    } else {
      stop("data must be phyloseq/ps_extra or data.frame/matrix")
    }

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
    o <- FN
  } else {
    if (identical(vars, NULL)) stop("vars must not be NULL if data given")
    o <- FN(data, vars = vars, which = ifelse(is.null(which), "column", which))
  }
  return(o)
}

#' Helper to specify heatmap annotation for variable distribution density plot
#'
#' Use this as an argument to varAnnotation(),
#' which itself is used by cor_heatmap var_anno argument.
#'
#' @inheritParams anno_var_box
#' @inheritParams anno_tax_density
#' @inheritDotParams ComplexHeatmap::anno_density axis_param
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#'
#' @examples
#' library(ComplexHeatmap)
#' set.seed(123)
#' fakeData <- as.data.frame.matrix(matrix(rnorm(500, 10, 3), ncol = 10))
#' names(fakeData) <- paste0("var_", 1:10)
#'
#' # draw the plots without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_density(data = fakeData, vars = names(fakeData), which = "row")
#' )
#'
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_density(
#'     data = fakeData, fun = function(x) log(x + 1),
#'     vars = rev(names(fakeData)), type = "heatmap",
#'     which = "column"
#'   )
#' )
anno_var_density <- function(fun = identity,
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
                             vars = NULL,
                             which = NULL) {
  force(fun)
  .size <- size # to avoid recursive default argument reference error

  densityArgs <- c(
    list(
      type = type, xlim = xlim, heatmap_colors = heatmap_colors,
      joyplot_scale = joyplot_scale, border = border, gp = gp, axis = axis
    ),
    list(...)
  )

  # create AnnotationFunction-making function
  FN <- function(data, vars, which, size = .size, args = densityArgs) {

    # extract (sample)data to matrix
    if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
      data <- samdatAsDataframe(ps_get(data))
    }
    if (inherits(data, "data.frame") || inherits(data, "matrix")) {
      x <- df_to_numeric_matrix(data, vars = vars, trans_fun = fun)
    } else {
      stop("data must be phyloseq/ps_extra or data.frame/matrix")
    }

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
    o <- FN
  } else {
    if (identical(vars, NULL)) stop("vars must not be NULL if data given")
    o <- FN(data, vars = vars, which = ifelse(is.null(which), "column", which))
  }
  return(o)
}

#' Helper to specify heatmap annotation for variable distribution density plot
#'
#' Use this as an argument to varAnnotation(),
#' which itself is used by cor_heatmap var_anno argument.
#'
#' @param n_breaks number of breaks
#' @inheritParams anno_var_density
#' @inheritDotParams ComplexHeatmap::anno_density axis_param
#'
#' @return function or ComplexHeatmap AnnotationFunction object
#' @export
#'
#' @examples
#' library(ComplexHeatmap)
#' set.seed(123)
#' fakeData <- as.data.frame.matrix(matrix(rnorm(500, 10, 3), ncol = 10))
#' names(fakeData) <- paste0("var_", 1:10)
#'
#' # draw the histograms without a heatmap, you will never normally do this!
#' vp <- viewport(width = 0.75, height = 0.75)
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_hist(data = fakeData, vars = names(fakeData), which = "row")
#' )
#'
#' grid.newpage()
#' pushViewport(vp)
#' draw(
#'   anno_var_hist(
#'     data = fakeData, fun = sqrt,
#'     vars = rev(names(fakeData)), n_breaks = 5,
#'     which = "column", gp = grid::gpar(fill = 2:6, lwd = c(0.9, 2.5))
#'   )
#' )
anno_var_hist <- function(fun = identity,
                          size = grid::unit(30, "mm"),
                          n_breaks = 11,
                          border = FALSE,
                          gp = grid::gpar(fill = "#CCCCCC"),
                          axis = TRUE,
                          ...,
                          data = NULL,
                          vars = NULL,
                          which = NULL) {
  force(fun)
  .size <- size # to avoid recursive default argument reference error

  histArgs <- c(
    list(n_breaks = n_breaks, border = border, gp = gp, axis = axis),
    list(...)
  )

  # create AnnotationFunction-making function
  FN <- function(data, vars, which, size = .size, args = histArgs) {

    # extract (sample)data to matrix
    if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
      data <- samdatAsDataframe(ps_get(data))
    }
    if (inherits(data, "data.frame") || inherits(data, "matrix")) {
      x <- df_to_numeric_matrix(data, vars = vars, trans_fun = fun)
    } else {
      stop("data must be phyloseq/ps_extra or data.frame/matrix")
    }

    if (identical(which, "row")) x <- t(x)
    args[c("x", "which")] <- list(x, which)
    if (identical(which, "row")) args$width <- size
    if (identical(which, "column")) args$height <- size

    # make annotation function object
    out <- do.call(what = ComplexHeatmap::anno_histogram, args = args)
    return(out)
  }

  # return function or call it if data given
  if (identical(data, NULL)) {
    o <- FN
  } else {
    if (identical(vars, NULL)) stop("vars must not be NULL if data given")
    o <- FN(data, vars = vars, which = ifelse(is.null(which), "column", which))
  }
  return(o)
}
