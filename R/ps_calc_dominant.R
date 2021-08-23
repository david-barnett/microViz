#' Calculate dominant taxon in each phyloseq sample
#'
#' @description
#' Which taxon is most abundant in each sample of your phyloseq object?
#' This function adds this information as a new variable in your phyloseq
#' sample_data.
#'
#' - If the most abundant taxon is below the proportional abundance threshold,
#' the dominant taxon will be "none" for that sample
#' - If there are more than n_max dominant taxa across all samples (not
#' including "none") the dominant taxon will be "other" for those samples
#'
#' @details
#' Thanks to Vitor Heidrich for the initial idea and draft implementation
#'
#'
#' @param ps phyloseq object
#' @param rank taxonomic rank to calculate
#' @param threshold
#' minimum proportion at which to consider a sample dominated by a taxon
#' @param n_max
#' maximum number of taxa that can be listed as dominant taxa
#' @param var
#' name of variable to add to phyloseq object sample data
#'
#' @return phyloseq object
#' @export
#'
#'
#' @examples
#' library(ggplot2)
#' ps <- corncob::ibd_phylo %>%
#'   tax_filter(min_prevalence = 3) %>%
#'   tax_fix() %>%
#'   phyloseq_validate()
#'
#' ps %>%
#'   ps_filter(DiseaseState == "CD") %>%
#'   ps_calc_dominant(rank = "Genus") %>%
#'   comp_barplot(tax_level = "Genus", label = "dominant_Genus", n_taxa = 12) +
#'   coord_flip()
ps_calc_dominant <- function(ps,
                             rank = "unique",
                             threshold = 0.3,
                             n_max = 6,
                             var = paste("dominant", rank, sep = "_")) {

  # get count phyloseq if ps_extra
  ps <- ps_counts(data = ps)

  # aggregate taxa at chosen rank for calculation
  psAgg <- tax_agg(ps, rank = rank)

  # get otu table with taxa as cols
  otu <- otu_get(psAgg)

  # calculate necessary features:
  # name of most abundant taxon for each sample
  topTaxonName <- getTopTaxon(data = otu)

  # check if most abundant taxon is above set dominance threshold proportion
  dominated <- isDominated(data = otu, threshold = threshold)

  # if sample not dominated by most abundant taxon, replace with "none"
  topTaxonName[!dominated] <- "none"

  # replace least frequently dominant taxa names with "other",
  # if there are more than n_max different dominant taxa (not including "none")
  topTaxonName <- aggregateExcessAsOther(topTaxonName, n_max = n_max)

  # Add new variable to original phyloseq and return
  if (var %in% phyloseq::sample_variables(ps)) {
    warning("Overwriting existing sample variable in phyloseq called: ", var)
  }
  phyloseq::sample_data(ps)[[var]] <- topTaxonName

  return(ps)
}

# helper functions -----------------------------------------------------------

# test if samples in otu_table count as "dominated" by comparing to threshold
# minimum proportion, which their most abundant taxon must reach/exceed
isDominated <- function(data, threshold) {

  # get otu table with taxa as cols
  otu <- otu_get(data = data)

  # calculate min threshold of dominance for each sample
  thresholdVec <- rowSums(otu) * threshold

  # calculate maximum value for most abundant taxon in each sample
  maxima <- apply(otu, MARGIN = 1, max, na.rm = TRUE)

  # is sample dominated?
  dominated <- maxima >= thresholdVec

  return(dominated)
}

# get name of most abundant taxon from otu_table (taxa as columns)
getTopTaxon <- function(data) {

  # get otu table with taxa as cols
  otu <- otu_get(data = data)

  # index of most abundant taxon per sample
  topTaxonIndex <- apply(otu, MARGIN = 1, which.max)

  # name of most abundant taxon per sample
  taxaNames <- colnames(otu)
  topTaxonName <- taxaNames[topTaxonIndex]

  return(topTaxonName)
}

# replace least frequently dominant taxa names with "other",
# if there are more than n_max different dominant taxa (not including "none")
aggregateExcessAsOther <- function(topTaxonName, n_max) {

  # table of dominant taxa, sorted by number of times they are dominant
  dominantTaxaTable <- sort(
    x = table(topTaxonName[topTaxonName != "none"]), decreasing = TRUE
  )

  # ensure more taxa are not requested than exist in dominant taxa list
  nMax <- min(n_max, length(dominantTaxaTable))

  # limited vector of acceptable dominant taxa categories
  validDominantTaxa <- names(dominantTaxaTable[seq_len(nMax)])

  # add "none" to list, if appropriate
  if (any(topTaxonName == "none")) {
    validDominantTaxa <- union(validDominantTaxa, "none")
  }

  # remove excess names, replacing with "other"
  topTaxonName[!topTaxonName %in% validDominantTaxa] <- "other"

  return(topTaxonName)
}
