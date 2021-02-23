# @param data phyloseq or ps_Extra
# @param taxa selection vector of taxa (names, numbers or logical)
# @param undetected the value above which taxa are classed as detected/present in a sample
#
# @return named vector of taxa prevalence values
prev_calc <- function(data, taxa, undetected = 0) {
  ps <- ps_get(data)
  otu <- otu_get(data)
  otu <- otu[, taxa, drop = FALSE]
  prevalence <- apply(otu, MARGIN = 2, function(x) sum(x > undetected, na.rm = TRUE)) / phyloseq::nsamples(ps)
  return(prevalence)
}

abund_calc <- function(data, taxa, undetected = 0) {
  ps <- ps_get(data)
  totals <- phyloseq::sample_sums(ps)
  prop_threshold <- undetected / totals
  otu <- otu_get(data)[, taxa, drop = FALSE]
  props <- apply(otu, MARGIN = 2, function(x) x / totals)
  props <- apply(props, MARGIN = 2, function(x) ifelse(test = x > prop_threshold, yes = x, no = NaN))
  return(props)
}



