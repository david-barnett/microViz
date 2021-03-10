# taxa --------------------

#' @name heatmap-annotations
#' @title Heatmap annotations helpers
#' @description
#' Functions to easily define ComplexHeatmap annotations for taxa and/or variables
#' - tax_anno creates list describing taxa annotation (for cor_heatmap or comp_heatmap)
#' - var_anno creates list describing variable annotation (for cor_heatmap)
#'
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which "row" or "column" annotation?
#' @param prev order in which prevalence annotation shown (number, or NA)
#' @param abund order in which abundance annotation shown (number, or NA)
#' @param size total size of all annotations in mm, excluding gaps (used as width or height, depending on value of which)
#' @param gap size of gap between annotations (in mm)
#' @param rel_sizes relative sizes of non-NA annotations (in given order) or NA (for all equal sizes)
#' @param args extra args passed to each annotation, give as list of lists (one inner list per arg, named, e.g. list(prev = list(whatever = whatever))
#' @param ... further args passed HeatmapAnnotation
#'
#' @export
#' @rdname heatmap-annotations
tax_anno <- function(undetected = 0, which = "row", prev = 1, abund = 2, size = 30, gap = 2, rel_sizes = NA, args = NULL, ...) {
  annos <- c(prev = prev, abund = abund) # written to support further annos in future
  annos <- sort(annos[!is.na(annos)])
  if (identical(rel_sizes, NA)) rel_sizes <- rep_len(1, length.out = length(annos))
  if (!identical(args, NULL)) {
    if (!inherits(args, "list") || inherits(args, "list") && length(args) != length(annos) || !names(args) %in% names(annos)) {
      stop("args must be list of annotation-named lists, the same length as number of non-NA annotations, or NULL")
    }
    is_list <- sapply(args, function(x) inherits(x, "list"))
    if (any(!is_list)) stop("some args entries are not lists: ", paste(names(args)[!is_list], collapse = " "))
  }

  if (length(unique(annos)) < length(annos) || max(c(annos, 1) > length(annos))) {
    stop("prev and abund must not be the same number, and neither should have a value greater than the number of non-NA entries")
    # TODO ensure that the order of values is used instead of the values (and remove the last part of this error)
  }
  if (length(rel_sizes) != length(annos)) {
    stop("length of rel_sizes must be equal to the number of non-NA annotations")
  }
  sizes <- size * rel_sizes / sum(rel_sizes, na.rm = TRUE)
  out_list <- list(what = names(annos), undetected = undetected, which = which, sizes = sizes, gap = gap, args = args, ...)
  return(out_list)
}


#' @title taxAnnotation for ComplexHeatmap
#' @description taxAnnotation is used for cor_heatmap, comp_heatmap & tax_model_heatmap (will be)
#'
#' @param data phyloseq or ps-extra
#' @param taxa names of taxa
#' @param undetected value above which taxa are considered present/detected in a sample
#' @param which 'row' or 'column' annotation
#' @param what ordered names of annotations (part of function names)
#' @param sizes ordered sizes of annotations, in mm
#' @param gap size of gap between annotations in mm
#' @param ... additional args passed to ComplexHeatmap::HeatmapAnnotation
#'
#' @noRd
taxAnnotation <- function(data, taxa, undetected = 0, which = "row", what = c("prev", "abund"), sizes = c(15, 15), gap = 2, args = NULL, ...) {
  ps <- ps_get(data)
  dots <- list(...)
  # start building args list
  ha_args <- list(
    annotation_name_gp = grid::gpar(fontsize = 8, fontface = "bold"),
    gap = grid::unit(gap, "mm"), which = which
  )
  for (i in seq_along(what)) {
    name <- what[[i]]
    ha_args[[name]] <- do.call(
      what = paste0("anno_", name),
      args = c(list(data = ps, taxa = taxa, undetected = undetected, which = which, size = sizes[[i]]), args[[name]])
    )
  }
  # use dots args (overwrite defaults or add more)
  ha_args[names(dots)] <- dots
  do.call(ComplexHeatmap::HeatmapAnnotation, args = ha_args)
}

# @param ... args passed to anno_barplot
# @return a ComplexHeatmap anno_barplot
#' @param data phyloseq or ps-extra (or even a data.frame or matrix for anno_var_* functions)
#' @param taxa names of taxa to plot
#' @param size size in mm (width or height, based on which)
#' @param bar_width relative width of barchart bars
#' @param gp a grid::gpar() object for graphics parameter settings like fill or lwd
#'
#' @export
#' @rdname heatmap-annotations
anno_prev <- function(data, taxa, undetected = 0, which = "row", size = 15, bar_width = 0.4, gp = grid::gpar(fill = "grey"), ...) {
  dots <- list(...)
  prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = prevs, bar_width = bar_width, ylim = 0:1, which = which, gp = gp)
  if (identical(which, "row")) args[["width"]] <- grid::unit(size, "mm")
  if (identical(which, "column")) args[["height"]] <- grid::unit(size, "mm")
  # overwrite any args given in dots
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_barplot, args = args)
  return(anno)
}

# @param ... args passed to anno_boxplot
# @return anno_boxplot
#' @param point_size size of outlier points in mm
#' @param box_width relative width of boxplot boxes
#'
#' @export
#' @rdname heatmap-annotations
anno_abund <- function(data, taxa, undetected = 0, which = "row", size = 15, point_size = 1, box_width = 0.4, ...) {
  dots <- list(...)
  abunds <- abund_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(x = abunds, size = grid::unit(point_size, "mm"), box_width = box_width, which = which)
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

# variables -------------------------

#' @param annos name(s) of annotation(s) to show, in order (e.g. 'var_box', 'var_hist')
#' @param funs function(s) to transform matrix of variable values before plotting (length must be 1 or same length as annos)
#' @param names names to use for each annotation in annos
#' @param which "row" or "column" annnotation
#' @param size total size in mm of annotations (width or height depending on which)
#' @param gap gap in mm between annotations
#' @param rel_sizes relative sizes of annotations (NA for equal sizes, or same length as annos)
#' @param ... further named args to be passed on (to list)
#'
#' @export
#' @rdname heatmap-annotations
#' @examples
#' library(phyloseq)
var_anno <- function(annos = "var_box",
                     funs = "identity",
                     names = NA,
                     which = "column",
                     size = 15 * length(annos),
                     gap = 2,
                     rel_sizes = NA,
                     args = NULL,
                     ...) {
  # fill empty names and sizes
  if (identical(names, NA)) names <- annos
  if (identical(rel_sizes, NA)) rel_sizes <- rep_len(1, length.out = length(annos))
  # check matching lengths
  if (length(funs) < length(annos)) funs <- rep_len(funs, length.out = length(annos))
  if (length(annos) != length(rel_sizes)) {
    stop("length of rel_sizes must be NA or equal to the length of annos")
  }
  if (length(annos) > length(names)) stop("names must be same length as annos, or NA")
  names <- names[seq_along(annos)] # shortens names vector if more names given than annos
  if (!identical(args, NULL)) {
    if (!inherits(args, "list") || inherits(args, "list") && length(args) != length(annos) || any(!names(args) %in% names)) {
      stop("args must be list of annotation-named lists, the same length as number of non-NA annotations, or NULL")
    }
    is_list <- sapply(args, function(x) inherits(x, "list"))
    if (any(!is_list)) stop("some args entries are not lists: ", paste(names(args)[!is_list], collapse = " "))
  }

  # calculate absolute sizes
  sizes <- size * rel_sizes / sum(rel_sizes, na.rm = TRUE)
  # return list
  list(annos = annos, funs = funs, names = names, which = which, sizes = sizes, gap = gap, args = args, ...)
}

#' @noRd
varAnnotation <- function(data, # from heatmap fun # converted to df
                          vars, # from heatmap fun # passed along to var_anno_*
                          annos, # from var_anno / anno_var
                          funs, # from var_anno / anno_var
                          names, # from var_anno / anno_var
                          which, # from var_anno / anno_var
                          sizes, # from var_anno / anno_var
                          gap, # from var_anno / anno_var
                          args, # from var_anno / anno_var
                          ... # from var_anno / anno_var
) {
  dots <- list(...)
  # extract data if phyloseq
  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    data <- data.frame(phyloseq::sample_data(ps_get(data)), check.names = FALSE)
  }
  mat <- df_to_numeric_matrix(data, vars = vars)

  # start building args list
  ha_args <- list(
    annotation_name_gp = grid::gpar(fontsize = 8, fontface = "bold"),
    gap = grid::unit(gap, "mm"), which = which
  )
  # create each annotation and add to args list
  dat <- list() # list to store (transformed) data

  for (i in seq_along(annos)) {
    name <- names[[i]]
    if (length(funs) == 1) funs <- list(funs) # otherwise trying to subset function/closure is error
    if (inherits(funs[[i]], "function")) dat[[i]] <- apply(mat, MARGIN = 2, FUN = funs[[i]])
    if (inherits(funs[[i]], "character")) dat[[i]] <- apply(mat, MARGIN = 2, FUN = function(x) do.call(what = funs[[i]], args = list(x)))
    # check for infinite values in data
    infinite <- is.infinite(dat[[i]])
    if (any(infinite)) {
      dat[[i]][infinite] <- NA
      warning(
        sum(infinite, na.rm = TRUE),
        " infinite values after applying funs[[", i, "]], replaced with NA in anno named: ",
        name, "\nThis could happen when e.g. you log transform data with 0s",
        "\nPerhaps you want 'log1p' or `function(x) log10(x + 1)` or something similar in funs argument?"
      )
    }
    # compute annotation
    ha_args[[name]] <- do.call(
      what = paste0("anno_", annos[[i]]),
      args = c(list(data = dat[[i]], vars = vars, which = which, size = sizes[[i]]), args[[name]])
    )
  }
  # use dots args (overwrite defaults or add more)
  ha_args[names(dots)] <- dots
  # make annotation
  do.call(ComplexHeatmap::HeatmapAnnotation, args = ha_args)
}


#' @param vars names of variables to plot
#' @export
#' @rdname heatmap-annotations
anno_var_hist <- function(data, vars = NA, which = "column", size = 15, ...) {
  dots <- list(...)

  # extract (sample)data to matrix
  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    data <- data.frame(phyloseq::sample_data(ps_get(data)), check.names = FALSE)
  } else if (inherits(data, "data.frame") || inherits(data, "matrix")){
    mat <- df_to_numeric_matrix(data, vars = vars)
  }

  # start args list
  args <- list(x = mat, n_breaks = 19, which = which)

  # handle orientation
  if (identical(which, "row")) {
    args[["x"]] <- t(mat)
    args[["width"]] <- grid::unit(size, "mm")
  } else if (identical(which, "column")) {
    args[["height"]] <- grid::unit(size, "mm")
  } else {
    stop("which must be either 'row' or 'column', not: ", which)
  }

  # make annotation
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_histogram, args = args)
  return(anno)
}

#' @export
#' @rdname heatmap-annotations
anno_var_box <- function(data, vars = NA, which = "column", size = 15, ...) {
  dots <- list(...)

  # extract (sample)data to matrix
  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    data <- data.frame(phyloseq::sample_data(ps_get(data)), check.names = FALSE)
  } else if (inherits(data, "data.frame") || inherits(data, "matrix")){
    mat <- df_to_numeric_matrix(data, vars = vars)
  }

  # start args list
  args <- list(
    x = mat, which = which,
    box_width = 0.4, # this is relative width of boxplot boxes
    size = grid::unit(1, "mm") # this is outlier point size
  )

  # handle orientation
  if (identical(which, "row")) {
    args[["x"]] <- t(mat)
    args[["width"]] <- grid::unit(size, "mm")
  } else if (identical(which, "column")) {
    args[["height"]] <- grid::unit(size, "mm")
  } else {
    stop("which must be either 'row' or 'column', not: ", which)
  }

  # make annotation
  args[names(dots)] <- dots
  anno <- do.call(ComplexHeatmap::anno_boxplot, args = args)
  return(anno)
}
