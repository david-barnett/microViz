#' @name ps_extra-accessors
#' @title Extract elements from ps_extra class
#'
#' @description
#' - `ps_get`     returns phyloseq
#' - `info_get`   returns ps_extra_info object
#' - `dist_get`   returns distance matrix (or NULL)
#' - `ord_get`    returns ordination object (or NULL)
#' - `perm_get`   returns adonis2() permanova model (or NULL)
#' - `bdisp_get`  returns results of betadisper() (or NULL)
#' - `otu_get`    returns phyloseq otu_table matrix with taxa as columns
#' - `samdat_tbl` returns phyloseq sample_data as a tibble, with sample_names as new first column called .sample_name
#'
#' @param ps_extra ps_extra class object
#'
#' @return element of ps_extra class object (or NULL)
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("esophagus")
#' @export
#' @rdname ps_extra-accessors
ps_get <- function(ps_extra) {
  if (inherits(ps_extra, "ps_extra")) {
    return(ps_extra[["ps"]])
  } else if (methods::is(ps_extra, "phyloseq")) {
    return(ps_extra)
  } else {
    stop(
      'class of argument should be "ps_extra" or "phyloseq", not: ',
      class(ps_extra)
    )
  }
}
#' @rdname ps_extra-accessors
#' @export
dist_get <- function(ps_extra) {
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["dist"]]
}
#' @rdname ps_extra-accessors
#' @export
ord_get <- function(ps_extra) {
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["ord"]]
}
#' @rdname ps_extra-accessors
#' @export
info_get <- function(ps_extra) {
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["info"]]
}
#' @rdname ps_extra-accessors
#' @export
perm_get <- function(ps_extra) {
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["permanova"]]
}
#' @rdname ps_extra-accessors
#' @export
bdisp_get <- function(ps_extra) {
  stopifnot(inherits(ps_extra, "ps_extra"))
  ps_extra[["bdisp"]]
}


#' @param data phyloseq or ps_extra
# @return phyloseq otu_table matrix with taxa as columns
#' @rdname ps_extra-accessors
#' @export
otu_get <- function(data) {
  if (methods::is(data, "otu_table")) {
    otu <- data
  } else {
    ps <- ps_get(data)
    otu <- phyloseq::otu_table(ps)
  }
  if (phyloseq::taxa_are_rows(otu)) otu <- phyloseq::t(otu)
  return(otu)
}

# get tax_table, currently just an internal helper
tt_get <- function(data) {
  if (!methods::is(data, "taxonomyTable")) {
    ps <- ps_get(data)
    tt <- phyloseq::tax_table(ps)
  } else {
    tt <- data
  }
  return(tt)
}

#' @param data phyloseq or ps_extra
# @return phyloseq sample_data as a tibble,
# with sample_names as new first column called .sample_name
#' @param sample_names_col
#' name of column where sample_names are put.
#' if NA, return data.frame with rownames (sample_names)
#' @rdname ps_extra-accessors
#' @export
samdat_tbl <- function(data, sample_names_col = ".sample_name") {
  if (inherits(data, "ps_extra")) data <- ps_get(data)
  if (methods::is(data, "phyloseq")) data <- phyloseq::sample_data(data)
  if (methods::is(data, "sample_data")) {
    df <- base::data.frame(data, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    stop(
      "data must be of class 'phyloseq', 'ps_extra', or 'sample_data', not: ",
      paste(class(data), collapse = " ")
    )
  }
  if (identical(sample_names_col, NA)) {
    return(df)
  } else {
    df <- tibble::rownames_to_column(df, var = sample_names_col)
    return(tibble::as_tibble(df))
  }
}

# get phyloseq with counts if available
ps_counts <- function(data, warn = TRUE){
  # always get ps, regardless of ps_extra or phyloseq data or counts presence
  ps <- ps_get(data)
  # checking names of a ps will return NULL (and x %in% NULL returns FALSE)
  if ("counts" %in% names(data)){
    # get counts and use them if they exist,
    # and check regardless if otutab returned will be counts
    counts <- data[["counts"]]
    # maintain existing taxa_are_rows status for consistency
    if (phyloseq::taxa_are_rows(ps)) counts <- phyloseq::t(counts)
    phyloseq::otu_table(ps) <- counts
  }
  if (!isFALSE(warn)) {
    mess <- paste0(
      "otu_table of counts is NOT available!\n",
      "Available otu_table contains non-zero values that are less than 1"
    )
    # now check ps otu_table is counts
    test_matrix <- unclass(otu_get(ps))
    if (any(test_matrix < 1 & test_matrix != 0)) {
      if(isTRUE(warn)){
        warning(mess)
      } else if (identical(warn, "error")){
        stop(mess)
      } else {
        stop("warn argument value is invalid: should be T, F or 'error'")
      }
    }
  }
  return(ps)
}

