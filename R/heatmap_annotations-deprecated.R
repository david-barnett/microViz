## Everything in this file is deprecated and will later be removed ##
# taxa --------------------

#' @name deprecated-heatmap-annotations
#' @title DEPRECATED Heatmap annotations helpers
#' @description
#' Functions to easily define ComplexHeatmap annotations for taxa and/or variables
#' - tax_anno creates list describing taxa annotation (for cor_heatmap or comp_heatmap)
#' - var_anno creates list describing variable annotation (for cor_heatmap)
#'
#' @param undetected
#' value above which taxa are considered present/detected in a sample
#' @param which
#' "row" or "column" annotation.
#' The default, NA, leaves this to be inferred by the *_heatmap function
#' @param prev
#' order in which prevalence annotation shown (number, or NA to not show)
#' @param abund
#' order in which abundance annotation shown (number, or NA to not show)
#' @param size
#' total size of all annotations in mm, excluding gaps
#' (used as width or height, depending on value of which)
#' @param gap size of gap between annotations (in mm)
#' @param rel_sizes
#' relative sizes of non-NA annotations (in given order)
#' or NA (for all equal sizes)
#' @param args extra args passed to each annotation: give as list of lists
#' (one inner list per arg, named, e.g. list(prev = list(whatever = whatever))
#' @param ... further args are passed ComplexHeatmap::HeatmapAnnotation()
#'
#' @export
#' @rdname deprecated-heatmap-annotations
tax_anno <- function(undetected = 0,
                     which = NA,
                     prev = 1,
                     abund = 2,
                     size = 30,
                     gap = 2,
                     rel_sizes = NA,
                     args = NULL,
                     ...) {
  .Deprecated(new = "taxAnnotation")
  # written to support further annotations in future
  annos <- c(prev = prev, abund = abund)
  # remove NAs
  annos <- annos[!is.na(annos)]
  # replace values with their order
  annos[names(annos)] <- order(annos)
  # sort annos as their names are used in order later
  annos <- sort(annos)

  # check options and stop if invalid
  anno_checkExtraArgsLists(args = args, annos = annos)

  if (length(unique(annos)) < length(annos)) {
    stop("prev and abund must not be the same number")
  }

  # get absolute sizes from relative sizes (and check right number for annos)
  sizes <- annoSizesCalc(size = size, rel_sizes = rel_sizes, annos = annos)

  # return partial list of instructions for taxAnnotate
  out_list <- list(
    what = names(annos), undetected = undetected,
    which = which, sizes = sizes, gap = gap, args = args, ...
  )
  return(out_list)
}


#' @title taxAnnotate for ComplexHeatmap
#' @description
#' taxAnnotate is used in cor_heatmap & comp_heatmap
#' (& will be used in tax_model_heatmap?)
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
#' @return HeatmapAnnotation object
#' @noRd
taxAnnotate <- function(data,
                        taxa,
                        undetected = 0,
                        which = "row",
                        what = c("prev", "abund"),
                        sizes = c(15, 15),
                        gap = 2,
                        args = NULL,
                        ...) {
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
      args = c(
        list(
          data = ps, taxa = taxa, undetected = undetected,
          which = which, size = sizes[[i]]
        ),
        args[[name]]
      )
    )
  }
  # use dots args (overwrite defaults or add more)
  ha_args[names(dots)] <- dots
  ha <- do.call(ComplexHeatmap::HeatmapAnnotation, args = ha_args)
  return(ha)
}

# @param ... args passed to anno_barplot
# @return a ComplexHeatmap anno_barplot
#' @param data
#' phyloseq or ps-extra (or a data.frame or matrix for anno_var_* functions)
#' @param taxa names of taxa to plot
#' @param size size in mm (width or height, based on which)
#' @param bar_width relative width of barchart bars
#' @param gp
#' a grid::gpar() object for graphics parameter settings like fill or lwd
#'
#' @export
#' @rdname deprecated-heatmap-annotations
anno_prev <- function(data,
                      taxa,
                      undetected = 0,
                      which = "row",
                      size = 15,
                      bar_width = 0.6,
                      gp = grid::gpar(fill = "grey85"),
                      ...) {
  dots <- list(...)
  prevs <- prev_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(
    x = prevs, bar_width = bar_width, ylim = 0:1, which = which, gp = gp
  )
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
#' @rdname deprecated-heatmap-annotations
anno_abund <- function(data,
                       taxa,
                       undetected = 0,
                       which = "row",
                       size = 15,
                       point_size = 0.75,
                       box_width = 0.6,
                       gp = grid::gpar(fill = "grey85"),
                       ...) {
  dots <- list(...)
  abunds <- abund_calc(data = data, taxa = taxa, undetected = undetected)
  args <- list(
    x = abunds, size = grid::unit(point_size, "mm"),
    box_width = box_width, which = which, gp = gp
  )
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

#' @param annos
#' name(s) of annotation(s) to show, in order (e.g. 'var_box', 'var_hist')
#' @param funs
#' function(s) to transform matrix of variable values before plotting
#' (length must be 1 or same length as annos)
#' @param names names to use for each annotation in annos
#' @param which "row" or "column" annnotation
#' @param size total size (mm) of annotations (width/height depending on which)
#' @param gap gap in mm between annotations
#' @param rel_sizes
#' relative sizes of annotations (NA for equal sizes, or same length as annos)
#' @param ... further named args to be passed on (to list)
#'
#' @export
#' @rdname deprecated-heatmap-annotations
var_anno <- function(annos = "var_box",
                     funs = "identity",
                     names = NA,
                     which = "column",
                     size = 15 * length(annos),
                     gap = 2,
                     rel_sizes = NA,
                     args = NULL,
                     ...) {
  .Deprecated(new = "varAnnotation")
  # fill empty names and sizes
  if (identical(names, NA)) names <- annos

  # check matching lengths
  if (length(funs) < length(annos)) funs <- rep_len(funs, length(annos))
  if (length(annos) > length(names)) {
    stop("names must be same length as annos, or NA")
  }
  # shorten names vector if more names given than annos
  names <- names[seq_along(annos)]
  names(annos) <- names

  anno_checkExtraArgsLists(args = args, annos = annos, names = names)

  # get absolute sizes from relative sizes (and check right number for annos)
  sizes <- annoSizesCalc(size = size, rel_sizes = rel_sizes, annos = annos)

  # return list
  list(
    annos = annos, funs = funs, names = names, which = which,
    sizes = sizes, gap = gap, args = args, ...
  )
}

#' @noRd
varAnnotate <- function(data, # from heatmap fun # converted to df
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
    data <- samdatAsDataframe(ps_get(data))
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
    # replace length 1 funs, otherwise subsetting function/closure --> error
    if (length(funs) == 1) funs <- list(funs)
    if (inherits(funs[[i]], "function")) {
      dat[[i]] <- apply(mat, MARGIN = 2, FUN = funs[[i]])
    }
    if (inherits(funs[[i]], "character")) {
      dat[[i]] <- apply(
        X = mat, MARGIN = 2, FUN = function(x) {
          do.call(what = funs[[i]], args = list(x))
        }
      )
    }
    # check for infinite values in data
    infinite <- is.infinite(dat[[i]])
    if (any(infinite)) {
      dat[[i]][infinite] <- NA
      warning(
        sum(infinite, na.rm = TRUE),
        " infinite values after applying funs[[", i, "]]\n",
        " - Replaced with NA in anno named: ", name, "\n",
        " - This could happen when e.g. you log transform data with 0s\n",
        " - Perhaps you want 'log1p' or `function(x) log10(x + 1)` \n\t",
        "or something similar in funs argument?"
      )
    }
    # compute annotation
    ha_args[[name]] <- do.call(
      what = paste0("old_anno_", annos[[i]]),
      args = c(
        list(data = dat[[i]], vars = vars, which = which, size = sizes[[i]]),
        args[[name]]
      )
    )
  }
  # use dots args (overwrite defaults or add more)
  ha_args[names(dots)] <- dots
  # make annotation
  do.call(ComplexHeatmap::HeatmapAnnotation, args = ha_args)
}


#' @param vars names of variables to plot
#' @export
#' @rdname deprecated-heatmap-annotations
old_anno_var_hist <- function(data, vars = NA, which = "column", size = 15, ...) {
  dots <- list(...)

  # extract (sample)data to matrix
  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    data <- samdatAsDataframe(ps_get(data))
  } else if (inherits(data, "data.frame") || inherits(data, "matrix")) {
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
#' @rdname deprecated-heatmap-annotations
old_anno_var_box <- function(data, vars = NA, which = "column", size = 15, ...) {
  dots <- list(...)

  # extract (sample)data to matrix
  if (methods::is(data, "phyloseq") || inherits(data, "ps_extra")) {
    data <- samdatAsDataframe(ps_get(data))
  } else if (inherits(data, "data.frame") || inherits(data, "matrix")) {
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


# shared helpers ----------------------------------------------------------

# user can supply extra arguments to args argument for each anno, but these
# must be supplied as a list of named lists (named by each annotation)
anno_checkExtraArgsLists <- function(args, annos, names = NULL) {
  if (identical(names, NULL)) names <- names(annos)
  if (!identical(args, NULL)) {
    if (!inherits(args, "list") ||
      length(args) != length(annos) ||
      any(!names(args) %in% names)) {
      stop(
        "\n- args can be NULL or list of named lists (named by annotation)",
        "\n- args must be the same length as number of non-NA annotations"
      )
    }
    is_list <- sapply(args, function(x) inherits(x, "list"))
    if (any(!is_list)) {
      stop(
        "some args entries are not lists: ",
        paste(names(args)[!is_list], collapse = " ")
      )
    }
  }
  return(NULL)
}

# for all annotations:
# user provides a vector of relative sizes of length equal to number of annos
# and a `size` specifying the total width or height (in mm)
annoSizesCalc <- function(size, rel_sizes, annos) {
  # equal sizing if NA given for rel_sizes
  if (identical(rel_sizes, NA)) rel_sizes <- rep_len(1, length(annos))
  # check rel_size given for each annotation in annos
  if (length(annos) != length(rel_sizes)) {
    stop("length of rel_sizes must be NA or equal the number of annotations")
  }
  # calculate absolute sizes
  sizes <- size * rel_sizes / sum(rel_sizes, na.rm = TRUE)
  return(sizes)
}
