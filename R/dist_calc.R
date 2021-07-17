#' Calculate distances between pairs of samples in phyloseq object
#'
#' @description
#' Can compute various sample-sample distances using the microbiota composition of your samples:
#'
#'  - bray curtis ('bray') or any other ecological distance from phyloseq::distance() / vegan::vegdist()
#'  - unifrac distances (using the GUniFrac package)
#'      - generalised: 'gunifrac' (optionally set weighting alpha in gunifrac alpha)
#'      - unweighted: 'unifrac'
#'      - weighted: 'wunifrac'
#'      - variance adjusted weighted: 'va-wunifrac'
#'  - aitchison distance (euclidean distance after centered log ratio transform clr, see details)
#'  - euclidean distance
#'
#' Use dist_calc with ps_extra output of tax_transform (or tax_agg).
#' It returns a ps_extra object containing the phyloseq and the name of the distance used
#' in addition to the distance matrix itself.
#' The resulting object is intended to be piped into ord_calc or dist_permanova functions.
#' Alternatively you can directly access the distance matrix with dist_get().
#'
#' @details
#' Aitchison distance note: You should EITHER:
#' 1. skip the dist_calc function and call ord_calc(method = "PCA") directly on an object with taxa transformed with tax_transform(transformation = "clr")
#' 2. pass an object with untransformed (or 'identity' transformed) taxa to the data argument of dist_calc() and specify dist = "aitchison".
#'
#' If ordination plots with taxon loading vectors are desired, users require option 1.
#' If the distance matrix is required for permanova, users require option 2.
#'
#'
#' @param data ps_extra object output from tax_transform()
#' @param dist name of distance to calculate between pairs of samples
#' @param gunifrac_alpha setting alpha value only relevant if gunifrac distance used
#' @param ... optional distance-specific named arguments passed to phyloseq::distance()
#'
#' @return list with distance matrix, phyloseq object, and name of distance used
#' @export
#'
#' @seealso \code{\link{tax_transform}} for the function to use before dist_calc
#' @seealso \code{\link{ord_calc}}
#' @seealso \code{\link{ord_plot}}
#' @seealso \code{\link{dist_permanova}}
#' @seealso \code{phyloseq::\link[phyloseq:distance]{distance}}
#'
#' @examples
#' # bray curtis distance on genera-level features
#' data("dietswap", package = "microbiome")
#' bc <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("bray")
#' bc
#' class(bc)
#'
#' # gunifrac distance using phyloseq input
#' data("esophagus", package = "phyloseq")
#' gunifrac <- esophagus %>%
#'   dist_calc("gunifrac") %>%
#'   dist_get()
#' class(gunifrac)
dist_calc <- function(data,
                      dist = c("bray", "gunifrac", "unifrac", "wunifrac", "va-wunifrac", "aitchison", "euclidean")[1],
                      gunifrac_alpha = 0.5,
                      ...) {
  ps <- ps_get(data)

  # check input data object class
  if (inherits(data, "ps_extra")) {
    info <- info_get(data)
  } else if (methods::is(data, "phyloseq")) {
    info <- new_ps_extra_info()
    data <- new_ps_extra(ps = ps, info = info)
  } else {
    stop(
      "data is wrong class, should be ps_extra e.g. output of tax_agg or tax_transform, or a phyloseq\n",
      "data is class: ", class(data)
    )
  }

  # aitchison distance
  if (identical(dist, "aitchison")) {
    if (identical(info[["tax_transform"]], "clr")) {
      stop("aitchison distance requested on data that are already clr-transformed, see the ?dist_calc details section!")
    }
    distMat <- t(microbiome::abundances(x = ps, transform = "clr")) %>%
      stats::dist(method = "euclidean")

    # unifrac distances using GUniFrac package
  } else if (grepl(pattern = "unifrac", dist)) {
    if (identical(ps@phy_tree, NULL)) {
      warning("unifrac distances require un-aggregated taxa and a phylogenetic tree.")
    }
    if (!requireNamespace("GUniFrac", quietly = TRUE)) {
      stop("You need to install package 'GUniFrac' to use unifrac distances.")
    }

    # much faster than phyloseq version of unifrac measures and results are the same (to floating point precision)
    distMats <- GUniFrac::GUniFrac(
      otu.tab = otu_get(ps),
      tree = phyloseq::phy_tree(ps),
      alpha = c(0, gunifrac_alpha, 1)
    )[["unifracs"]]

    gunifrac_alpha <- switch(
      EXPR = dist,
      "unifrac" = "UW",
      "wunifrac" = 1,
      "va-wunifrac" = "VAW",
      "vawunifrac" = "VAW",
      "gunifrac" = gunifrac_alpha
    )
    distMat <- stats::as.dist(distMats[, , paste0("d_", gunifrac_alpha)])
    if (identical(dist, "gunifrac")) dist <- paste(dist, gunifrac_alpha, sep = "_")

    # calculate distance matrix if distance is supported in phyloseq
  } else if (dist %in% unlist(phyloseq::distanceMethodList)) {
    distMat <- phyloseq::distance(ps, method = dist, type = "samples", ...)
  } else {
    stop(paste("Invalid distance measure named in dist argument:", dist))
  }

  # return object
  info[["distMethod"]] <- dist
  data[["dist"]] <- distMat
  data[["info"]] <- info

  return(data)
}
