#' Calculate distances between pairs of samples in phyloseq object
#'
#' For use directly with phyloseq object or with list output of tax_agg.
#' Computes various unifrac distances, aitchison distance or any distance from vegdist.
#' Returns phyloseq and name of distance in addition to the distance matrix itself!
#' Result intended to be piped into ord_calc or permanova functions.
#'
#' Aitchison distance note: You should EITHER
#' 1. skip the dist_calc function and call ord_calc(method = "RDA") directly on an object with taxa transformed with tax_transform(transformation = "clr")
#' 2. pass an object with untransformed (or 'identity' transformed) taxa to the data argument of dist_calc and specify dist = "aitchison".
#' If ordination plots with taxon loading distances are desired, users should prefer option 1.
#' If the distance matrix is required for permanova, users should prefer option 2.
#'
#'
#' @param data phyloseq object or list object output from tax_agg
#' @param dist name of distance to calculate between pairs of samples
#' @param return choose which parts of list object to return
#' @param gunifrac_alpha setting alpha value only relevant if gunifrac distance used
#' @param ... optional distance-specific named args passed to phyloseq::distance()
#'
#' @return list with distance matrix, phyloseq object, and name of distance used
#' @export
#'
#' @examples
#' # bray curtis distance on genera-level features
#' data("dietswap", package = "microbiome")
#' bc <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("bray")
#' class(bc)
#'
#' # gunifrac distance using phyloseq input
#' data("esophagus", package = "phyloseq")
#' gunifrac <- esophagus %>%
#'   dist_calc("gunifrac", return = "distMat")
#' class(gunifrac)
dist_calc <- function(
                      data,
                      dist = c("bray", "gunifrac", "unifrac", "wunifrac", "va-wunifrac", "aitchison", "euclidean")[1],
                      return = "all",
                      gunifrac_alpha = 0.5,
                      ...) {

  # check input data object class
  if (inherits(data, "list")) {
    ps <- data[["ps"]]
    info <- data[["info"]]
    if (is.null(info[["tax_transform"]])) {
      info[["tax_transform"]] <- "none specified"
    }
  } else if (inherits(data, "phyloseq")) {
    ps <- data
    info <- list(
      tax_level = "not specified",
      tax_transform = "none specified"
    )
  } else {
    stop("data argument is wrong class")
  }

  # aitchison distance
  if (dist == "aitchison") {
    if (isTRUE(info[["tax_transform"]] == "clr")) {
      stop("aitchison distance requested on data that are already clr-transformed, see the ?dist_calc details section!")
    }
    distMat <- ps %>%
      microbiome::abundances(transform = "clr") %>%
      t() %>%
      stats::dist(method = "euclidean")
  } else if (grepl(pattern = "unifrac", dist)) {
    if (rlang::is_null(ps@phy_tree)) {
      warning("unifrac distances require un-aggregated taxa and a phylogenetic tree.")
    }
    # unifrac distances using GUniFrac package
    # much faster than phyloseq version of unifrac measures and results are the same (to floating point precision)
    distMats <- GUniFrac::GUniFrac(t(microbiome::abundances(ps)),
      tree = phyloseq::phy_tree(ps),
      alpha = c(0, gunifrac_alpha, 1)
    )[["unifracs"]]

    # TODO change to a switch ?
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
    distMat <- phyloseq::distance(ps, method = dist, type = "samples", ...)
  } else {
    stop(paste("Invalid distance measure named in dist argument:", dist))
  }

  # return object
  info[["distName"]] <- dist
  if (dist == "gunifrac") {
    info[["gunifrac_alpha"]] <- gunifrac_alpha
  }
  out <- list(ps = ps, distMat = distMat, info = info)

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
