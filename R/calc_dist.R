#' Calculate distances between pairs of samples in phyloseq object
#'
#' For use directly with phyloseq object or with list output of agg_tax.
#' Computes various unifrac distances, aitchison distance or any distance from vegdist.
#' Returns phyloseq and name of distance in addition to the distance matrix itself!
#' Result intended to be piped into ordin8 or permanova functions.
#'
#' Note: Instead of calculating aitchison distance with this function, users should prefer directly applying microbiome::transform(ps, 'clr') and using pca/rda method of ordin8 to allow plotting loading vectors in plot_ordin8
#'
#'
#' @param data phyloseq object or list object output from agg_tax
#' @param dist name of distance to calculate between pairs of samples
#' @param return choose which parts of list object to return
#' @param gunifrac_alpha setting alpha value only relevant if gunifrac distance used
#'
#' @return list with distance matrix, phyloseq object, and name of distance used
#' @export
#'
#' @examples
#' # bray curtis distance on genera-level features
#' data("dietswap", package = "microbiome")
#' bc <- dietswap %>%
#'   agg_tax("Genus") %>%
#'   calc_dist("aitchison")
#' class(bc)
#'
#' # gunifrac distance using phyloseq input
#' data("esophagus", package = "phyloseq")
#' gunifrac <- esophagus %>%
#'   calc_dist("gunifrac", return = "distMat")
#'
#' class(gunifrac)
#'
calc_dist <- function(
                      data,
                      dist = c("bray", "gunifrac", "unifrac", "wunifrac", "va-wunifrac", "aitchison", "euclidean")[1],
                      return = "all",
                      gunifrac_alpha = 0.5) {

  # check input data object class
  if (inherits(data, "list")) {
    ps <- data[["ps"]]
    tax_level <- data[["tax_level"]]
  } else if (inherits(data, "phyloseq")) {
    ps <- data
    tax_level <- NULL
  } else {
    stop("data argument is wrong class")
  }

  # aitchison distance
  if (dist == "aitchison") {
    distMat <- ps %>%
      microbiome::abundances(transform = "clr") %>%
      t() %>%
      stats::dist(method = "euclidean")
  } else if (grepl(pattern = "unifrac", dist)) {
    # unifrac distances using GUniFrac package
    # much faster than phyloseq version of unifrac measures and results are the same (to floating point precision)
    distMats <- GUniFrac::GUniFrac(t(microbiome::abundances(ps)),
                                   tree = phyloseq::phy_tree(ps),
                                   alpha = c(0, gunifrac_alpha, 1)
    )[["unifracs"]]

    if (dist == "unifrac") {
      gunifrac_alpha <- "UW"
    } else if (dist == "wunifrac") {
      gunifrac_alpha <- 1
    } else if (dist %in% c("va-wunifrac", "vawunifrac")) {
      gunifrac_alpha <- "VAW"
    }

    distMat <- stats::as.dist(distMats[, , paste0("d_", gunifrac_alpha)])
  } else if (dist %in% unlist(phyloseq::distanceMethodList)) {

    # calculate distance matrix if distance is supported in phyloseq
    distMat <- phyloseq::distance(ps, method = dist, type = "samples")
  } else {
    stop(glue::glue('Invalid distance measure "{dist}" named in dist argument.'))
  }

  # return object options
  out <- list(ps = ps, distMat = distMat, distName = dist, tax_level = tax_level)

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
