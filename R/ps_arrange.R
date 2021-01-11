#' Rearrange samples in phyloseq by sample_data variables or taxon abundance
#'
#' Uses dplyr::arrange under the hood
#'
#' @param ps phyloseq object
#' @param ... dots passed directly to dplyr::arrange
#' @param .target arrange samples by "sample_data" variables or "otu_table" taxa abundances
#'
#' @return phyloseq
#' @export
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
#'
#'
ps_arrange <- function(ps, ..., .target = "sample_data") {
  otu <- switch(
    .target,
    "sample_data" = {
      df <- data.frame(phyloseq::sample_data(ps))
      df <- tibble::rownames_to_column(df, var = ".temp_sample_name_var")

      df <- dplyr::arrange(df, ...)
      df <- tibble::column_to_rownames(df, var = ".temp_sample_name_var")

      sample_order <- rownames(df)

      # ordering of samples is a phyloseq is controlled from the otu_table slot!
      otu <- phyloseq::otu_table(ps)
      if (phyloseq::taxa_are_rows(ps)) {
        otu <- otu[, sample_order]
      } else {
        otu <- otu[sample_order, ]
      }
      otu
    },
    "otu_table" = {
      otu <- phyloseq::otu_table(ps)
      if (phyloseq::taxa_are_rows(ps)) {
        samplenames <- colnames(otu)
        otu_df <- as.data.frame.matrix(t(otu))
        otu_df[["samplenames"]] <- samplenames
        otu_df <- dplyr::arrange(otu_df, ...)
        otu <- otu[, otu_df[["samplenames"]]]
      } else {
        samplenames <- rownames(otu)
        otu_df <- as.data.frame.matrix(otu)
        otu_df[["samplenames"]] <- samplenames
        otu_df <- dplyr::arrange(otu_df, ...)
        otu <- otu[, otu_df[["samplenames"]]]
      }
    }
  )

  phyloseq::otu_table(ps) <- otu

  return(ps)
}
