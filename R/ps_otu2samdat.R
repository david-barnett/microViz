#' Copy phyloseq otu_table data to sample_data
#'
#' @param ps phyloseq with sample_data
#' @param taxa list of taxa_names to copy to sample_data, or NULL (which selects all with `phyloseq::taxa_names()`)
#'
#' @return phyloseq with augmented sample_data
#' @export
#'
#' @examples
#' library(dplyr)
#' library(phyloseq)
#' library(microbiome)
#' data("dietswap")
#'
#' ps <- dietswap %>% ps_otu2samdat("Akkermansia")
#' sample_variables(ps)
#'
#' # or if you do not specify any taxa, all are copied
#' ps_all <- dietswap %>% ps_otu2samdat()
#' sample_variables(ps_all)[1:15]
#'
#' # this could be useful for colouring ordination plots, for example
#' ps %>%
#'   ps_mutate(log_akkermansia = log(Akkermansia)) %>%
#'   dist_calc("bray") %>%
#'   ord_calc("PCoA") %>%
#'   ord_plot(
#'     colour = "log_akkermansia",
#'     size = 3, shape = "nationality"
#'   )
ps_otu2samdat <- function(ps, taxa = NULL) {
  # if no taxa were specified, get all taxa
  if (identical(taxa, NULL)) {
    taxa <- phyloseq::taxa_names(ps)
  } else if (any(!taxa %in% phyloseq::taxa_names(ps))) {
    # check any specified taxa are in phyloseq
    message("You specified the following taxa that are not present as taxa_names in the phyloseq:")
    message(paste(taxa[!taxa %in% phyloseq::taxa_names(ps)], collapse = "; "))
  }

  ps_df <- data.frame(phyloseq::sample_data(ps))
  otu <- phyloseq::otu_table(ps)

  if (phyloseq::taxa_are_rows(ps)) otu <- t(otu)

  otu <- otu[, taxa]
  otu_df <- as.data.frame.matrix(otu)

  ps_df <- cbind.data.frame(ps_df, otu_df)

  phyloseq::sample_data(ps) <- ps_df

  return(ps)
}
