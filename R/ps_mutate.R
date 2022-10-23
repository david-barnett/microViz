#' Modify or compute new sample_data in phyloseq object
#'
#' Add or compute new phyloseq sample_data variables.
#' Uses `dplyr::mutate()` syntax.
#'
#' @param ps phyloseq object with sample data
#' @param ... passed straight to dplyr::mutate (see examples and dplyr::mutate help)
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
ps_mutate <- function(ps, ...) {
  check_is_phyloseq(ps, argName = "ps")

  df <- samdatAsDataframe(ps)
  saved_rownames <- rownames(df)
  df <- dplyr::mutate(df, ...)
  rownames(df) <- saved_rownames
  ps@sam_data <- phyloseq::sample_data(df) # should work for psExtra and phyloseq

  return(ps)
}
