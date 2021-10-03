#' Modify or compute new sample_data in phyloseq object
#'
#' Add or compute new phyloseq sample_data variables.
#' Uses `dplyr::mutate()` syntax.
#'
#' @param ps phyloseq object with sample data
#' @param ... passed straight to dplyr::mutate (see examples and dplyr::mutate help)
#' @param .target DEPRECATED. See tax_mutate for manipulation of tax_table
#'
#' @return phyloseq object with modified sample_data
#' @export
#'
#' @seealso \code{\link{tax_mutate}} for manipulation of tax_table variables
#' @seealso \code{\link[dplyr]{mutate}}
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' data("enterotype")
#'
#' sample_data(enterotype)[1:10, ]
#'
#' months_in_year <- 12
#' ps <- enterotype %>%
#'   ps_mutate(
#'     age_months = Age * months_in_year,
#'     IDs_match = as.character(Sample_ID) == as.character(SampleID),
#'     placeholder = "Word"
#'   )
#'
#' sample_data(ps)[1:10, ]
#'
#' # Using the dplyr::across functionality is also possible
#' ps <- ps %>%
#'   ps_mutate(
#'     dplyr::across(where(is.factor), toupper),
#'     another_var = TRUE,
#'     SeqTech = NULL # deletes SeqTech variable
#'   )
#'
#' head(sample_data(ps))
ps_mutate <- function(ps, ..., .target) {
  if (!missing(.target)){
    if (!identical(.target, "sample_data")) {
      stop("Use of .target argument is deprecated.")
    } else {
      warning("Use of .target argument is deprecated.")
    }
  }

  if (inherits(ps, "ps_extra")) {
    warning("ps argument is a ps_extra, but only a phyloseq will be returned")
    ps <- ps_get(ps)
  }


  df <- data.frame(phyloseq::sample_data(ps), check.names = FALSE)
  saved_rownames <- rownames(df)
  df <- dplyr::mutate(df, ...)
  rownames(df) <- saved_rownames
  phyloseq::sample_data(ps) <- df

  return(ps)
}
