
ps_extra_arg_deprecation_warning <- function(ps_extra) {
  rlang::warn(call = rlang::caller_env(1), message = c(
    "ps_extra argument deprecated",
    i = "use psExtra argument instead"
  ))
  return(ps_extra)
}

check_is_phyloseq <- function(x, argName = NULL, allow_psExtra = TRUE) {
  stopif_ps_extra(x, argName = argName, Ncallers = 2)
  isPhyloseq <- is(x, "phyloseq") && (allow_psExtra || !is(x, "psExtra"))

  if (!isPhyloseq) {
    CLASSES <- if (allow_psExtra) '"phyloseq" or "psExtra"' else '"phyloseq"'

    rlang::abort(call = rlang::caller_env(), message = c(
      paste("argument", argName, "must be a", CLASSES, "object"),
      i = paste0("argument is class: ", paste(class(x), collapse = " "))
    ))
  }
}

check_is_psExtra <- function(x, argName = NULL) {
  stopif_ps_extra(x, argName = argName, Ncallers = 2)
  if (!is(x, "psExtra")) {
    rlang::abort(call = rlang::caller_env(), message = c(
      paste("argument", argName, 'must be a "psExtra" object'),
      i = paste0("argument is class: ", paste(class(x), collapse = " "))
    ))
  }
}

#' @name psExtra-accessors
#' @title Extract elements from ps_extra class
#'
#' @description
#' - `ps_get`     returns phyloseq
#' - `info_get`   returns psExtraInfo object
#' - `dist_get`   returns distance matrix (or NULL)
#' - `ord_get`    returns ordination object (or NULL)
#' - `perm_get`   returns adonis2() permanova model (or NULL)
#' - `bdisp_get`  returns results of betadisper() (or NULL)
#' - `otu_get`    returns phyloseq otu_table matrix with taxa as columns
#' - `tt_get`     returns phyloseq tax_table
#' - `samdat_tbl` returns phyloseq sample_data as a tibble,
#' with sample_names as new first column called .sample_name
#'
#' @param psExtra psExtra S4 class object
#' @param ps_extra deprecated! don't use this
#'
#' @return element(s) from psExtra object (or NULL)
#' @export
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' psx <- tax_transform(dietswap, "identity", rank = "Genus")
#' psx
#'
#' ps_get(psx)
#' info_get(psx)
#'
#' dist_get(psx) # this psExtra has no dist_calc result
#' ord_get(psx) # this psExtra has no ord_calc result
#' perm_get(psx) # this psExtra has no dist_permanova result
#' bdisp_get(psx) # this psExtra has no dist_bdisp result
#'
#' # these can be returned from phyloseq objects too
#' otu_get(psx)[1:6, 1:4]
#' tt_get(psx) %>% head()
#' samdat_tbl(psx) %>% head()
#' @export
#' @rdname psExtra-accessors
ps_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_phyloseq(psExtra)
  return(as(psExtra, "phyloseq"))
}
#' @rdname psExtra-accessors
#' @export
dist_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_psExtra(psExtra)
  psExtra@dist
}
#' @rdname psExtra-accessors
#' @export
ord_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_psExtra(psExtra)
  psExtra@ord
}
#' @rdname psExtra-accessors
#' @export
info_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_phyloseq(psExtra)
  if (!methods::is(psExtra, "psExtra")) {
    return(new_psExtraInfo())
  }
  return(psExtra@info)
}
#' @rdname psExtra-accessors
#' @export
perm_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_psExtra(psExtra)
  return(psExtra@permanova)
}
#' @rdname psExtra-accessors
#' @export
bdisp_get <- function(psExtra, ps_extra) {
  if (!missing(ps_extra)) psExtra <- ps_extra_arg_deprecation_warning(ps_extra)
  check_is_psExtra(psExtra)
  return(psExtra@bdisp)
}


#' @rdname psExtra-accessors
#' @export
tax_models_get <- function(psExtra) {
  check_is_psExtra(psExtra, argName = "psExtra")
  return(psExtra@tax_models)
}

#' @rdname psExtra-accessors
#' @export
tax_stats_get <- function(psExtra) {
  check_is_psExtra(psExtra, argName = "psExtra")
  return(psExtra@tax_stats)
}

#' @rdname psExtra-accessors
#' @export
taxatree_models_get <- function(psExtra) {
  check_is_psExtra(psExtra, argName = "psExtra")
  return(psExtra@taxatree_models)
}

#' @rdname psExtra-accessors
#' @export
taxatree_stats_get <- function(psExtra) {
  check_is_psExtra(psExtra, argName = "psExtra")
  return(psExtra@taxatree_stats)
}

#' @param data phyloseq or ps_extra
# @return phyloseq otu_table matrix with taxa as columns
#'
#' @param taxa subset of taxa to return, NA for all (default)
#' @param samples subset of samples to return, NA for all (default)
#' @param counts should otu_get ensure it returns counts? if present in object
#'
#' @rdname psExtra-accessors
#' @export
otu_get <- function(data, taxa = NA, samples = NA, counts = FALSE) {
  # get otu_table from object
  if (methods::is(data, "otu_table")) {
    if (isTRUE(counts)) warning("data is otu_table: ignoring `counts = TRUE`")
    otu <- data
  } else {
    ps <- if (isTRUE(counts)) ps_counts(data) else ps_get(data)
    otu <- phyloseq::otu_table(ps)
  }
  if (phyloseq::taxa_are_rows(otu)) otu <- phyloseq::t(otu)

  # subset samples and/or taxa if requested, with slightly more helpful errors
  if (!identical(taxa, NA)) {
    stopifnot(is.character(taxa) || is.numeric(taxa) || is.logical(taxa))
    tmp <- try(expr = otu <- otu[, taxa, drop = FALSE], silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (is.character(taxa)) {
        wrong <- paste(setdiff(taxa, colnames(otu)), collapse = " / ")
        stop("The following taxa were not found in the otu table:\n", wrong)
      } else {
        stop("Invalid taxa selection")
      }
    }
  }
  if (!identical(samples, NA)) {
    stopifnot(is.character(samples) || is.numeric(samples) || is.logical(samples))
    tmp <- try(expr = otu <- otu[samples, , drop = FALSE], silent = TRUE)
    if (inherits(tmp, "try-error")) {
      if (is.character(samples)) {
        wrong <- paste(setdiff(samples, rownames(otu)), collapse = " / ")
        stop("The following samples were not found in the otu table:\n", wrong)
      } else {
        stop("Invalid sample selection")
      }
    }
  }
  return(otu)
}

#' @rdname psExtra-accessors
#' @export
tt_get <- function(data) {
  if (methods::is(data, "taxonomyTable")) {
    return(data)
  }
  tt <- phyloseq::tax_table(ps_get(data))
  return(tt)
}

#' @param data phyloseq or psExtra
# @return phyloseq sample_data as a tibble,
# with sample_names as new first column called .sample_name
#' @param sample_names_col
#' name of column where sample_names are put.
#' if NA, return data.frame with rownames (sample_names)
#' @rdname psExtra-accessors
#' @export
samdat_tbl <- function(data, sample_names_col = ".sample_name") {
  if (is(data, "sample_data") || is(data, "phyloseq")) {
    df <- samdatAsDataframe(data) # also works for psExtra
  } else {
    rlang::abort(message = c(
      "data must be of class 'phyloseq', 'psExtra', or 'sample_data'",
      i = paste("It is class:", paste(class(data), collapse = " "))
    ))
  }
  if (identical(sample_names_col, NA)) {
    return(df)
  } else {
    df <- tibble::rownames_to_column(df, var = sample_names_col)
    return(tibble::as_tibble(df, .name_repair = "check_unique"))
  }
}

# internal helper that get phyloseq sample_data as plain dataframe
# without changing invalid colnames (like microbiome::meta does)
# or losing rownames / sample_names (like data.frame() with defaults does)
samdatAsDataframe <- function(ps) {
  samdat <- phyloseq::sample_data(ps)
  df <- data.frame(samdat, check.names = FALSE)
  return(df)
}

# get phyloseq with counts if available
ps_counts <- function(data, warn = TRUE) {
  check_is_phyloseq(data)
  if (!rlang::is_bool(warn) && !rlang::is_string(warn, string = "error")) {
    stop("warn argument must be TRUE, FALSE, or 'error'")
  }
  counts <- NULL # check this later, warn if still NULL

  # always get ps, regardless of psExtra or phyloseq data or counts presence
  ps <- ps_get(data)

  # get counts and use them if they exist,
  # and check regardless if otutab returned will be counts
  if (is(data, "psExtra")) counts <- data@counts

  # maintain existing taxa_are_rows status for consistency
  if (phyloseq::taxa_are_rows(ps) && !is.null(counts)) counts <- phyloseq::t(counts)
  # put non-null counts table in otu table slot
  if (!is.null(counts)) phyloseq::otu_table(ps) <- counts

  if (isFALSE(warn)) {
    return(ps)
  }

  mess <- paste0(
    "otu_table of counts is NOT available!\n",
    "Available otu_table contains non-zero values that are less than 1"
  )

  # lastly check ps otu_table is counts
  test_matrix <- unclass(otu_get(ps))
  if (any(test_matrix < 1 & test_matrix != 0)) {
    if (identical(warn, "error")) stop(mess)
    if (isTRUE(warn)) warning(mess)
  }
  return(ps)
}
