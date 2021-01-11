#' Mutate sample data in phyloseq object
#'
#' Wrapper for dplyr::mutate. Use mutate syntax with all the non-standard evaluation.
#'
#' @param ps phyloseq object with sample data
#' @param ... passed straight to dplyr::mutate (see examples and dplyr::mutate help)
#' @param .target which slot to mutate, currently only "sample_data" supported
#' @param .across placeholder for future implementation of working dplyr::across functionality

#' @return phyloseq object with modified sample_data
#' @export
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
#' # Using the dplyr::across functionality is a little different,
#' # but is possible using the dedicated .across argument.
#' # Further expressions can be passed to dots at the same time as using .across.
#'   ps <- ps %>%
#'   ps_mutate(
#'     .across = dplyr::across(where(is.factor), toupper),
#'     dots_still_work = TRUE
#'   )
#'
#' sample_data(ps)[1:10, ]
#'
ps_mutate <- function(ps, ..., .target = "sample_data", .across = NULL) {
  if (!inherits(ps, "phyloseq")) {
    stop("ps must be a phyloseq object. It is of class: ", class(ps))
  }

  if (!identical(.target, "sample_data")) {
    stop("Only .target = 'sample_data', has been implemented so far.")
  }
  # TODO: see if it is useful to facilitate mutating variables in other phyloseq slots

  df <- data.frame(phyloseq::sample_data(ps))
  saved_rownames <- rownames(df)

  across <- quote(.across)
  if (!rlang::is_null(across)){
    df <- dplyr::mutate(df, eval(across))
  }

  df <- dplyr::mutate(df, ...)
  rownames(df) <- saved_rownames
  phyloseq::sample_data(ps) <- df

  return(ps)
}
