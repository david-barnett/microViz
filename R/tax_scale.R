#' Mean-center and SD-scale taxa in phyloseq
#'
#' Wrapper for applying base scale function to phyloseq otu_table
#'
#' @param data phyloseq or psExtra or otu_table
#' @param center if TRUE: center each taxon by subtracting its mean
#' @param scale if TRUE, divide each centred taxon by its standard deviation (or divide by RMS if not centred!)
#' @param do alternative argument that overrides center and scale options! takes "both", "scale", "center" or "neither"
#' @param keep_counts
#' if TRUE, retain the original count data in psExtra counts slot
#'
#' @export
#'
#' @examples
#' data("dietswap", package = "microbiome")
#' ps <- dietswap
#' ps %>%
#'   otu_get() %>%
#'   .[1:6, 1:6]
#'
#' # standard use (mean center and SD scale)
#' tax_scale(ps) %>%
#'   otu_get() %>%
#'   .[1:6, 1:6] # Aerococcus is NaN as standard deviation = 0 (0 prevalence)
#'
#' # RMS scale only (directly on otu_table)
#' otu_get(ps) %>%
#'   tax_scale(center = FALSE) %>%
#'   .[1:6, 1:6] # Aerococcus is NaN as standard deviation = 0 (0 prevalence)
#'
#' # example using alternative `do` argument (to center only, no scaling)
#' tax_scale(ps, do = "center") %>%
#'   otu_get() %>%
#'   .[1:6, 1:6]
#'
#' # preserves existing info
#' tax_transform(ps, "compositional", rank = "Genus") %>% tax_scale()
#'
#' # drops other psExtra objects previously calculated with unscaled data
#' psxDist <- tax_agg(ps, "Genus") %>% dist_calc()
#' psxDist
#' psxDist %>% tax_scale()
#' tax_scale(psxDist) %>% info_get()
tax_scale <- function(data, center = TRUE, scale = TRUE, do = NA, keep_counts = TRUE) {

  # overwrite center and scale according to non-NA value of do arg, if given
  if (!identical(do, NA)) {
    stopifnot(do %in% c("both", "center", "scale", "neither"))
    center <- switch(
      EXPR = do,
      both = TRUE,
      center = TRUE,
      scale = FALSE,
      neither = FALSE
    )
    scale <- switch(
      EXPR = do,
      both = TRUE,
      center = FALSE,
      scale = TRUE,
      neither = FALSE
    )
  }

  # actually do scaling
  otu <- otu_get(data) # works on otu_table, phyloseq and psExtra!
  otu <- base::scale(otu, center = center, scale = scale)
  otu <- phyloseq::otu_table(otu, taxa_are_rows = FALSE)

  # exit early if otu_table
  if (methods::is(data, "otu_table")) {
    return(otu)
  }

  scaling <- paste(c("centered", "scaled")[c(center, scale)], collapse = "&")

  # check input data object class
  if (is(data, "psExtra")) {

    # check and update pre-existing info
    info <- info_get(data)
    if (length(info[["tax_scale"]]) > 0) {
      warning("data were already scaled: ", info[["tax_scale"]])
    }
    newInfo <- new_psExtraInfo(
      tax_agg = info$tax_agg, tax_trans = info$tax_trans, tax_scale = scaling
    )
    data@info <- newInfo

    # retain counts if requested
    if (isTRUE(keep_counts)) data@counts <- otu_get(data, counts = TRUE)
    data@otu_table <- otu
  } else if (methods::is(data, "phyloseq")) {
    data <- psExtra(data, info = new_psExtraInfo(tax_scale = scaling))
  }

  return(data)
}
