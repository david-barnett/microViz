#' Calculate distances between pairs of samples in phyloseq object
#'
#' @description
#' Can compute various sample-sample distances using the microbiota composition of your samples:
#'
#'  - Bray Curtis ('bray') or any other ecological distance from phyloseq::distance() / vegan::vegdist()
#'  - UniFrac distances (using the GUniFrac package)
#'      - generalised: 'gunifrac' (optionally set weighting alpha in gunifrac alpha)
#'      - unweighted: 'unifrac'
#'      - weighted: 'wunifrac'
#'      - variance adjusted weighted: 'va-wunifrac'
#'  - Aitchison distance (Euclidean distance after centered log ratio transform clr, see details)
#'  - Euclidean distance
#'
#' Use dist_calc with psExtra output of tax_transform (or tax_agg).
#' It returns a psExtra object containing the phyloseq and the name of the distance used
#' in addition to the distance matrix itself.
#' The resulting object is intended to be piped into ord_calc or dist_permanova functions.
#' Alternatively you can directly access the distance matrix with dist_get().
#'
#' @section Aitchison distance note:
#'
#' You should EITHER:
#'   1. skip the dist_calc function and call ord_calc(method = "PCA") directly on an object with taxa transformed with tax_transform(trans = "clr")
#'   2. pass an object with untransformed (or 'identity' transformed) taxa to the data argument of dist_calc() and specify dist = "aitchison".
#'
#' If ordination plots with taxon loading vectors are desired, users require option 1.
#' If the distance matrix is required for permanova, users require option 2.
#'
#' @section Binary Jaccard distance note:
#'
#' Jaccard distance can be computed on abundances, but often in microbiome
#' research it is the Binary Jaccard distance that is desired. So remember to
#' first perform a "binary" transformation with `tax_transform("binary")`,
#' OR pass an additional argument to `dist_calc("jaccard", binary = TRUE)`
#'
#' @param data psExtra object, e.g. output from tax_transform()
#' @param dist name of distance to calculate between pairs of samples
#' @param gunifrac_alpha
#' setting alpha value only relevant if gunifrac distance used
#' @param ...
#' optional distance-specific named arguments passed to phyloseq::distance()
#'
#' @return psExtra object including distance matrix and name of distance used
#' @export
#'
#' @seealso \code{\link{tax_transform}} for the function to use before dist_calc
#' @seealso \code{\link{ord_calc}}
#' @seealso \code{\link{ord_plot}}
#' @seealso \code{\link{dist_permanova}}
#' @seealso \code{phyloseq::\link[phyloseq:distance]{distance}}
#' @seealso \code{vegan::\link[vegan:vegdist]{vegdist}}
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
                      dist = "bray",
                      gunifrac_alpha = 0.5,
                      ...) {

  # check valid distance name was supplied
  if (!rlang::is_string(dist)) stop("dist must be name of distance/dissimilarity")
  rlang::arg_match(arg = dist, multiple = FALSE, values = union(c(
    "bray", "gunifrac", "unifrac", "wunifrac", "va-wunifrac",
    "aitchison", "robust.aitchison", "euclidean"
  ), unlist(phyloseq::distanceMethodList)))

  # check input data object class
  distCalcDataValidate(data)

  # get components
  ps <- ps_get(data)
  info <- info_get(data)

  # calculate distance matrix #
  if (identical(dist, "aitchison") || identical(dist, "robust.aitchison")) {
    # aitchison distance (or robust aitchison)
    distMat <- distMatAitchison(ps = ps, dist = dist, info = info)
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

  if (!is(data, "psExtra")) data <- psExtra(data, info = new_psExtraInfo())
  info <- modify_psExtraInfo(info, dist_method = dist)

  data@dist <- distMat
  data@info <- info
  return(data)
}



# calculates (robust.)aitchison distance matrix from phyloseq: ps
# (ps_extra `info` required for transformation check)
#
distMatAitchison <- function(ps, dist, info) {
  if (identical(info$tax_trans, "clr") || identical(info$tax_trans, "rclr")) {
    rlang::abort(call = rlang::caller_env(1), message = c(
      "dist_calc 'aitchison' distance requires count data",
      i = paste0(
        "your data are ", info$tax_trans,
        "-transformed (according to ps_extra info)"
      ),
      i = "see the ?dist_calc details section for more info"
    ))
  }
  if (dist == "aitchison") trans <- "clr"
  if (dist == "robust.aitchison") trans <- "rclr"
  transformedOTUtable <- t(microbiome::abundances(x = ps, transform = trans))
  distMat <- stats::dist(transformedOTUtable, method = "euclidean")
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
  if (!is_ps_extra(data) && !methods::is(data, "phyloseq")) {
    stop(
      "data for dist_calc must be of class 'psExtra'\n",
      " - e.g. output of tax_agg or tax_transform\n",
      " - data is class: ", paste(class(data), collapse = " ")
    )
  }
}
