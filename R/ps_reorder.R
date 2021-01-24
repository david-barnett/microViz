#' Set order of samples in phyloseq object
#'
#' Ordering of samples is a phyloseq is controlled from the otu_table slot!
#'
#' @param ps phyloseq
#' @param sample_order names or current numerical indices of samples in desired order
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
#'   sample_data() %>%
#'   head(8)
#'
#' new_order <- rev(sample_names(dietswap))
#' dietswap %>%
#'   ps_reorder(new_order) %>%
#'   sample_data() %>%
#'   head(8)
#'
#' # random ordering with numbers
#' set.seed(1000)
#' random_order <- sample(1:nsamples(dietswap))
#' dietswap %>%
#'   ps_reorder(random_order) %>%
#'   sample_data() %>%
#'   head(8)
#'
ps_reorder <- function(ps, sample_order){

  otu <- phyloseq::otu_table(ps)
  if (phyloseq::taxa_are_rows(ps)) {
    otu <- otu[, sample_order]
  } else {
    otu <- otu[sample_order, ]
  }
  phyloseq::otu_table(ps) <- otu

  return(ps)
}


