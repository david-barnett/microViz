#' Gut microbiota relative abundance data from Shao et al. 2019
#'
#' @description
#' A phyloseq dataset containing 1644 gut microbiome samples.
#' Fecal samples were collected from 596 infants. 175 mothers also provided fecal samples.
#'
#' @source
#' Stunted microbiota and opportunistic pathogen colonization in caesarean-section birth.
#' \doi{10.1038/s41586-019-1560-1}
#'
#' @section Data acquisition:
#' The processed (metaphlan3) relative abundance data were obtained using curatedMetagenomicData package version 3.4.2.
#' <https://waldronlab.io/curatedMetagenomicData/articles/available-studies.html>
#'
#' A small amount of data cleaning was performed after download:
#'
#'  1. to make sample metadata more user friendly
#'  2. to fill gaps in the taxonomy table.
#'
#' @format large phyloseq object
#'
"shao19"
