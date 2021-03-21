#' Deselect phyloseq samples with sample_data missings
#'
#' @description
#' Check phyloseq object sample data for missing values (NAs)
#' - specify which variables to check with vars argument, or check all)
#' - drop samples with any missings
#'
#' @details
#' This is a wrapper for \code{\link{stats::complete.cases()}} function.
#'
#' @param ps phyloseq with sample_data
#' @param vars vector of variable names to check for missings (or NA, which uses all variables in sample data)
#' @param verbose message about number of samples dropped if verbose not FALSE, (and only if > 0 samples dropped)
#' and message about number of missing per variable in vars if verbose = "max" (and message even if 0 samples dropped)
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
    incomplete <- nrow(df) - nrow(df_sub)
    if (incomplete > 0 || identical(verbose, "max")) {
      message("Dropping samples with missings: ", incomplete)
    }
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
