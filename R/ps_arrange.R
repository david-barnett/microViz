#' Arrange samples in phyloseq by sample_data variables or taxon abundance
#'
#' @description
#' Uses information in the sample_data or tax_table of phyloseq object
#' to set the order of the samples
#' (sample_data or tax_table specified by .target arg)
#'
#' Give this function arguments in the same way you would use dplyr::arrange()
#'
#'
#' @param ps phyloseq object
#' @param ... dots passed directly to dplyr::arrange()
#' @param .target arrange samples by "sample_data" variables or "otu_table" taxa abundances
#'
#' @return phyloseq
#' @export
#'
#' @seealso \code{\link[dplyr]{arrange}}
#'
#' @examples
#' library(phyloseq)
#' library(microbiome)
#' data("dietswap")
#'
#' dietswap %>%
#'   ps_arrange(subject, timepoint) %>%
#'   sample_data() %>%
#'   head(8)
#'
#' ps <- dietswap %>% ps_arrange(subject, desc(timepoint))
#' sample_data(ps) %>% head(8)
#' otu_table(ps)[1:8, 1:8]
#'
#' # you can also arrange samples by the abundances of taxa in the otu tables
#' pst <- dietswap %>% ps_arrange(desc(Akkermansia), .target = "otu_table")
#' otu_table(pst)[1:8, 1:8]
#' sample_data(pst) %>% head(8)
ps_arrange <- function(ps, ..., .target = "sample_data") {
  ps <- ps_get(ps)
  sample_order <- switch(.target,
    "sample_data" = {
      df <- data.frame(phyloseq::sample_data(ps))
      df <- tibble::rownames_to_column(df, var = ".temp_sample_name_var")
      df <- dplyr::arrange(df, ...)
      df[[".temp_sample_name_var"]]
    },
    "otu_table" = {
      otu <- phyloseq::otu_table(ps)
      if (phyloseq::taxa_are_rows(ps)) otu <- phyloseq::t(otu)
      samplenames <- rownames(otu)
      otu_df <- as.data.frame.matrix(otu)
      otu_df[["samplenames"]] <- samplenames
      otu_df <- dplyr::arrange(otu_df, ...)
      otu_df[["samplenames"]]
    }
  )

  ps <- ps_reorder(ps, sample_order = sample_order)

  return(ps)
}
