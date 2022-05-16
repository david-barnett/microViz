#' De-duplicate phyloseq samples
#'
#' @description
#' Use one or more variables in the sample_data to identify and
#' remove duplicate samples (leaving one sample per group).
#'
#' __methods:__
#' - method = "readcount" keeps the one sample in each duplicate group
#' with the highest total number of reads (phyloseq::sample_sums)
#' - method = "first" keeps the first sample in each duplicate group
#' encountered in the row order of the sample_data
#' - method = "last" keeps the last sample in each duplicate group
#' encountered in the row order of the sample_data
#' - method = "random" keeps a random sample from each duplicate group
#' (set.seed for reproducibility)
#'
#' More than one "duplicate" sample can be kept per group by setting `n` samples > 1.
#'
#' @details
#' What happens when duplicated samples have exactly equal readcounts in method = "readcount"?
#' The first encountered maximum is kept (in sample_data row order, like method = "first")
#'
#' @param ps phyloseq object
#' @param vars
#' names of variables, whose (combined) levels identify groups
#' from which only 1 sample is desired
#' @param method
#' keep sample with max "readcount" or the "first" or "last" or "random"
#' samples encountered in given sample_data order for each duplicate group
#' @param verbose message about number of groups, and number of samples dropped?
#' @param n number of 'duplicates' to keep per group, defaults to 1
#' @param .keep_group_var keep grouping variable .GROUP. in phyloseq object?
#' @param .keep_readcount keep readcount variable .READCOUNT. in phyloseq object?
#' @param .message_IDs message sample names of dropped variables?
#' @param .label_only
#' if TRUE, the samples will NOT be filtered, just labelled with a new logical
#' variable .KEEP_SAMPLE.
#' @param .keep_all_taxa
#' keep all taxa after removing duplicates?
#' If FALSE, the default, taxa are removed if they never occur in any of
#' the retained samples
#'
#' @return phyloseq object
#' @export
#'
#' @seealso \code{\link{ps_filter}} for filtering samples by sample_data variables
#'
#' @examples
#' data("dietswap", package = "microbiome")
#'
#' dietswap
#' # let's pretend the dietswap data contains technical replicates from each subject
#' # we want to keep only one of them
#' ps_dedupe(dietswap, vars = "subject", method = "readcount", verbose = TRUE)
#'
#' # contrived example to show identifying "duplicates" via the interaction of multiple columns
#' ps1 <- ps_dedupe(
#'   ps = dietswap, method = "readcount", verbose = TRUE,
#'   vars = c("timepoint", "group", "bmi_group")
#' )
#' phyloseq::sample_data(ps1)
#'
#' ps2 <- ps_dedupe(
#'   ps = dietswap, method = "first", verbose = TRUE,
#'   vars = c("timepoint", "group", "bmi_group")
#' )
#' phyloseq::sample_data(ps2)
ps_dedupe <- function(ps,
                      vars,
                      method = "readcount",
                      verbose = TRUE,
                      n = 1,
                      .keep_group_var = FALSE,
                      .keep_readcount = FALSE,
                      .message_IDs = FALSE,
                      .label_only = FALSE,
                      .keep_all_taxa = FALSE
){
  # Check inputs
  if (!inherits(ps, "phyloseq")) stop("ps must be a phyloseq object")
  if (!rlang::is_character(vars)) stop("vars must be character")
  psCheckVariables(ps = ps, vars = vars)
  method <- rlang::arg_match(method, values = c("readcount", "first", "last", "random"))
  if (!rlang::is_scalar_integerish(n) || n < 1) stop('n must be a positive number')
  if (!rlang::is_bool(verbose)) stop("verbose must be TRUE or FALSE")
  if (!rlang::is_bool(.keep_group_var)) stop(".keep_group_var must be TRUE or FALSE")
  if (!rlang::is_bool(.keep_readcount)) stop(".keep_readcount must be TRUE or FALSE")
  if (!rlang::is_bool(.message_IDs)) stop(".message_IDs must be TRUE or FALSE")
  if (!rlang::is_bool(.label_only)) stop(".label_only must be TRUE or FALSE")
  if (!rlang::is_bool(.keep_all_taxa)) stop(".keep_all_taxa must be TRUE or FALSE")

  # End of input checks

  # get sample data and compute new (temporary?) variables
  df <- samdat_tbl(ps, sample_names_col = "._ID_.")
  df[[".GROUP."]] <- purrr::pmap_chr(
    .l = df[, vars, drop = FALSE], .f = function(...) paste(..., sep = "_")
  )
  if (.keep_group_var) phyloseq::sample_data(ps)[[".GROUP."]] <- df[[".GROUP."]]

  if (method == "readcount") {
    ps <- ps_counts(data = ps, warn = "error") # stop if data are not counts
    df[[".READCOUNT."]] <- phyloseq::sample_sums(x = ps)
    if (.keep_readcount) {
      phyloseq::sample_data(ps)[[".READCOUNT."]] <- df[[".READCOUNT."]]
    }
  }

  # deduplicate data
  df <- dplyr::group_by(.data = df, .data[[".GROUP."]])
  if (verbose) {
    nDup <- dplyr::count(df)[["n"]]
    nPerGroup <- paste(unique(range(nDup)), collapse = " to ")
    message(length(nDup), " groups: with ", nPerGroup, " samples each")
  }

  dfDedupe <- switch(
    EXPR = method,
    "readcount" = {
      dplyr::slice_max(
        .data = df, order_by = .data[[".READCOUNT."]], with_ties = FALSE, n = n
      )
    },
    "first" = dplyr::slice_head(df, n = n),
    "last" = dplyr::slice_tail(df, n = n),
    "random" = dplyr::slice_sample(df, n = n, replace = FALSE, weight_by = NULL)
  )

  # message about progress
  if (verbose) message("Dropped ", nrow(df) - nrow(dfDedupe), " samples.")
  if (.message_IDs) {
    message(paste(dplyr::setdiff(df$._ID_., dfDedupe$._ID_.), collapse = ", "))
  }

  # label samples to keep
  phyloseq::sample_data(ps)[['.KEEP_SAMPLE.']] <- df$._ID_. %in% dfDedupe$._ID_.
  if (.label_only) return(ps) # early exit without filtering

  # filter samples
  ps <- ps_filter(ps, .data$.KEEP_SAMPLE., .keep_all_taxa = .keep_all_taxa)
  phyloseq::sample_data(ps)[['.KEEP_SAMPLE.']] <- NULL

  return(ps)
}

