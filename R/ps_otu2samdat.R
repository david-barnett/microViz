#' Copy phyloseq otu_table data to sample_data
#'
#' @param ps phyloseq with sample_data
#' @param taxa list of taxa_names to copy to sample_data, or NULL (which selects all with `phyloseq::taxa_names()`)
#'
#' @return phyloseq with augmented sample_data
#' @export
#'
#' @examples
#' library(phyloseq)
#' data("dietswap", package = "microbiome")
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
#'   ord_calc(method = "PCoA") %>%
#'   ord_plot(
#'     colour = "log_akkermansia",
#'     size = 3, shape = "nationality"
#'   )
ps_otu2samdat <- function(ps, taxa = NULL) {
  # if no taxa were specified, get all taxa
  if (identical(taxa, NULL)) taxa <- phyloseq::taxa_names(ps)

  ps_df <- samdatAsDataframe(ps)
  otu <- otu_get(ps, taxa = taxa)
  otu_df <- as.data.frame.matrix(otu)

  if (any(colnames(otu_df) %in% colnames(ps_df))) {
    warning(
      "Overwriting the following sample_data variables:\n",
      paste(intersect(colnames(otu_df), colnames(ps_df)), collapse = " / ")
    )
  }

  ps_df[, colnames(otu_df)] <- otu_df
  phyloseq::sample_data(ps) <- ps_df
  return(ps)
}
