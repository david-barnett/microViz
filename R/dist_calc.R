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
#' 1. skip the dist_calc function and call ord_calc(method = "PCA") directly on an object with taxa transformed with tax_transform(trans = "clr")
#' 2. pass an object with untransformed (or 'identity' transformed) taxa to the data argument of dist_calc() and specify dist = "aitchison".
#'
#' If ordination plots with taxon loading vectors are desired, users require option 1.
#' If the distance matrix is required for permanova, users require option 2.
#'
#'
#' @param data ps_extra object, e.g. output from tax_transform()
#' @param dist name of distance to calculate between pairs of samples
#' @param gunifrac_alpha
#' setting alpha value only relevant if gunifrac distance used
#' @param ...
#' optional distance-specific named arguments passed to phyloseq::distance()
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

  # check input data object class
  distCalcDataValidate(data)

  # create ps_extra from phyloseq if phyloseq given
  if (methods::is(data, "phyloseq")) {
    data <- new_ps_extra(ps = data, info = new_ps_extra_info())
  }

  # get components
  ps <- ps_get(data)
  info <- info_get(data)

  # calculate distance matrix #
  if (identical(dist, "aitchison")) {
    # aitchison distance
    distMat <- distMatAitchison(ps = ps, info = info)
  } else if (grepl(pattern = "unifrac", dist)) {
    # unifrac distances using GUniFrac package
    # define unifrac result ID required by GUniFrac package
    uniID <- switch(
      EXPR = dist,
      "unifrac" = "UW",
      "wunifrac" = 1,
      "va-wunifrac" = "VAW",
      "vawunifrac" = "VAW",
      "gunifrac" = gunifrac_alpha
    )
    # update distance name
    if (identical(dist, "gunifrac")) dist <- paste0(dist, "_", uniID)

    distMat <- distMatUnifrac(
      ps = ps, gunifrac_alpha = gunifrac_alpha, uniID = uniID
    )
  } else if (dist %in% unlist(phyloseq::distanceMethodList)) {
    # calculate distance matrix if distance is supported in phyloseq
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



# calculates aitchison distance matrix from phyloseq: ps
# (ps_extra info required for transformation check)
distMatAitchison <- function(ps, info) {
  if (identical(info[["tax_transform"]], "clr")) {
    stop(
      "dist_calc 'aitchison' distance requires count data\n",
      " - your data are clr-transformed (according to the ps_extra info)",
      "\n - see the ?dist_calc details section for more info"
    )
  }
  distMat <- t(microbiome::abundances(x = ps, transform = "clr")) %>%
    stats::dist(method = "euclidean")
  return(distMat)
}



# calculates any unifrac distance matrix from phyloseq: ps
# gunifrac_alpha
distMatUnifrac <- function(ps, gunifrac_alpha, uniID) {
  if (identical(ps@phy_tree, NULL)) {
    warning(
      "unifrac distances require un-aggregated taxa and a phylogenetic tree."
    )
  }
  if (!requireNamespace("GUniFrac", quietly = TRUE)) {
    stop("You need to install package 'GUniFrac' to use unifrac distances.")
  }

  # GUniFrac is much faster than phyloseq version of unifrac measures
  # and results are the same (to floating point precision)
  distMats <- GUniFrac::GUniFrac(
    otu.tab = otu_get(ps),
    tree = phyloseq::phy_tree(ps),
    alpha = c(0, gunifrac_alpha, 1)
  )[["unifracs"]]

  distMat <- stats::as.dist(distMats[, , paste0("d_", uniID)])

  return(distMat)
}

# data class checker, also used in dist_calc_seq()
distCalcDataValidate <- function(data) {
  if (!inherits(data, "ps_extra") && !methods::is(data, "phyloseq")) {
    stop(
      "data for dist_calc must be of class 'ps_extra'\n",
      " - e.g. output of tax_agg or tax_transform\n",
      " - data is class: ", paste(class(data), collapse = " ")
    )
  }
}
