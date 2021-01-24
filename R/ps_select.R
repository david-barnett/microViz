#' Select phyloseq sample_data using dplyr::select syntax
#'
#' Simple selection of phyloseq sample_data variables, might be useful for printing reduced sample_data, or inside other functions
#'
#' @param ps phyloseq with sample_data
#' @param ... passed straight to dplyr::select
#'
#' @return phyloseq object
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' data("enterotype", package = "phyloseq")
#'
#' head(sample_data(enterotype))
#'
#' enterotype %>%
#'   ps_select(!contains("Sample")) %>%
#'   sample_data() %>%
#'   head()
ps_select <- function(ps, ...) {
  df <- data.frame(phyloseq::sample_data(ps))
  df <- dplyr::select(.data = df, ...)
  phyloseq::sample_data(ps) <- df
  return(ps)
}
