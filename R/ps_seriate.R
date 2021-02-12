#' Arrange samples in a phyloseq by microbiome similarity
#'
#' Uses seriation methods from seriation::seriate and often dist_calc (depending on if seriation method requires a distance matrix)
#'
#' @param ps phyloseq object
#' @param method seriation method for ordering samples, from seriation::seriate
#' @param dist distance method for dist_calc (only used if required for particular seriation method!)
#' @param tax_transform transformation to apply before seriation or any distance calculation
#' @param add_variable add a variable to the sample data indicating seriation order
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
#' dietswap %>%
#'   tax_agg("Genus") %>%
#'   .$ps %>%
#'   ps_seriate(method = "OLO_ward", dist = "bray") %>%
#'   sample_data() %>%
#'   head(8)
ps_seriate <- function(ps, method = "OLO_ward", dist = "bray", tax_transform = "identity", add_variable = FALSE) {
  if (phyloseq::nsamples(ps) > 2) {
    # transform taxa for ordering (facilitated primarily for clr for PCA_angle method)
    ps_transformed <- microbiome::transform(ps, transform = tax_transform)

    if (method %in% seriation::list_seriation_methods(kind = "matrix")) {
      # directly seriate the otu matrix
      otu_mat <- t(microbiome::abundances(ps_transformed))
      ser <- seriation::seriate(x = otu_mat, method = method)
    } else if (method %in% seriation::list_seriation_methods(kind = "dist")) {
      # calculate distance between samples
      distMat <- dist_get(dist_calc(data = ps_transformed, dist = dist))
      ser <- seriation::seriate(x = distMat, method = method)
    } else {
      stop(
        method, " is not a valid method in seriation::seriate!\n",
        "Nearest match is: ", agrep(method, seriation::list_seriation_methods(), value = TRUE, ignore.case = TRUE)[[1]]
      )
    }
    s_order <- seriation::get_order(ser)
    if (isTRUE(add_variable)) {
      phyloseq::sample_data(ps)[[".seriation_order"]] <- s_order
    } else if (!isFALSE(add_variable)) {
      phyloseq::sample_data(ps)[[add_variable]] <- s_order
    }
    ps <- ps_reorder(ps, sample_order = s_order)
  }

  ps
}
