#' Arrange samples in a phyloseq by microbiome similarity
#'
#' Uses seriation methods from seriation::seriate and often dist_calc (depending on if seriation method requires a distance matrix)
#'
#' @param ps phyloseq object
#' @param method seriation method for ordering samples, from seriation::seriate
#' @param dist distance method for dist_calc (only used if required for particular seriation method!)
#' @param tax_transform transformation to apply before seriation or any distance calculation
#' @param add_variable add a variable to the sample data indicating seriation order
#' @param rank taxonomic rank to aggregate at, before seriation, NA for no aggregation
#'
#' @return phyloseq
#' @export
#' @seealso \code{\link{ps_arrange}} \code{\link{ps_reorder}}
#'
#' @examples
#' library(phyloseq)
#' data("dietswap", package = "microbiome")
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
ps_seriate <- function(ps,
                       method = "OLO_ward",
                       dist = "bray",
                       tax_transform = "identity",
                       add_variable = FALSE,
                       rank = NA) {
  ps <- ps_get(ps)
  if (phyloseq::nsamples(ps) <= 2) {
    return(ps) # return early if no ordering
  }

  # aggregate taxa for ordering (possibly, as NA is no aggregation)
  psX <- tax_agg(ps, rank = rank)
  # transform taxa for ordering (facilitated primarily for clr for PCA methods)
  ps_transformed <- tax_transform(psX, trans = tax_transform)[["ps"]]

  if (method %in% seriation::list_seriation_methods(kind = "matrix")) {
    # directly seriate the otu matrix
    ser <- seriation::seriate(x = otu_get(ps_transformed), method = method)
  } else if (method %in% seriation::list_seriation_methods(kind = "dist")) {
    # calculate distance between samples
    distMat <- dist_get(dist_calc(data = ps_transformed, dist = dist))
    ser <- seriation::seriate(x = distMat, method = method)
  } else {
    stop(
      method, " is not a valid method in seriation::seriate!\n",
      "Nearest match is: ", findNearestSeriationMethods(method)[[1]]
    )
  }
  s_order <- seriation::get_order(ser)
  if (isTRUE(add_variable)) add_variable <- ".seriation_order" # default name
  if (!isFALSE(add_variable)) phyloseq::sample_data(ps)[[add_variable]] <- s_order

  ps <- ps_reorder(ps, sample_order = s_order)
  return(ps)
}

# helper for
findNearestSeriationMethods <- function(method, ...) {
  m <- seriation::list_seriation_methods(...) # specify kind if needed
  return(agrep(pattern = method, x = m, value = TRUE, ignore.case = TRUE))
}
