#' Deselect phyloseq samples with sample_data missings
#'
#' Wrapper for stats::complete.cases function.
#'
#' @param ps phyloseq with sample_data
#' @param vars vector of variable names to check for missings (or NA, which uses all variables in sample data)
#' @param verbose message about number of samples dropped if not FALSE, and number per variable in vars if "max"
#'
#' @return phyloseq
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("enterotype", package = "phyloseq")
#'
#' enterotype
#' ps_drop_incomplete(enterotype)
#' ps_drop_incomplete(enterotype, vars = "Enterotype", verbose = TRUE)
#' ps_drop_incomplete(enterotype, vars = "Sample_ID", verbose = TRUE)
#' ps_drop_incomplete(enterotype, vars = c("Enterotype", "Sample_ID"))
#' ps_drop_incomplete(enterotype, verbose = "max")
ps_drop_incomplete <- function(ps, vars = NA, verbose = FALSE) {
  df <- data.frame(phyloseq::sample_data(ps))
  if (identical(vars, NA)) vars <- phyloseq::sample_variables(ps)
  df_sub <- df[, vars, drop = FALSE]
  df_sub <- df_sub[stats::complete.cases(df_sub), , drop = FALSE]
  if (!isFALSE(verbose)) {
    message("Dropping samples with missings: ", nrow(df) - nrow(df_sub))
  }
  if (identical(verbose, "max")) {
    for (v in vars) {
      n_missings <- sum(is.na(df[[v]]))
      if (n_missings > 0) message(v, " has NAs: ", n_missings)
    }
  }
  keepers <- rownames(df_sub)
  phyloseq::prune_samples(samples = keepers, x = ps)
}
