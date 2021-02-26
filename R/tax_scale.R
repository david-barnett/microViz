#' Mean-center and SD-scale taxa in phyloseq
#'
#' Wrapper for applying base scale function to phyloseq otu_table
#'
#' @param data phyloseq or ps_extra or otu_table
#' @param center if TRUE: center each taxon by subtracting its mean
#' @param scale if TRUE, divide each centred taxon by its standard deviation (or divide by RMS if not centred!)
#' @param do alternative argument that overrides center and scale options! takes "both", "scale", "center" or "neither"
#'
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(microbiome)
#' data("dietswap")
#' ps <- dietswap
#' ps %>% otu_get() %>% .[1:6, 1:6]
#' # standard use (mean center and SD scale)
#' tax_scale(ps) %>% otu_get() %>% .[1:6, 1:6]
#' # RMS scale only (directly on otu_table)
#' otu_table(ps) %>% tax_scale(center = FALSE) %>% .[1:6, 1:6] #Aerococcus is NaN as prevalence=0
#' # example using alternative `do` argument (to center only, no scaling)
#' tax_scale(ps, do = "center") %>% otu_get() %>% .[1:6, 1:6]
tax_scale <- function(data, center = TRUE, scale = TRUE, do = NA) {

  # overwrite center and scale according to non-NA value of do arg, if given
  if (!identical(do, NA)) {
    stopifnot(do %in% c("both", "center", "scale", "neither"))
    center <- switch(do, both = TRUE, center = TRUE, scale = FALSE, neither = FALSE)
    scale <- switch(do, both = TRUE, center = FALSE, scale = TRUE, neither = FALSE)
  }
  scaling_info <- paste(c("centered", "scaled")[c(center, scale)], collapse = "&")

  # otu_get works on otu_table, phyloseq and ps_extra!
  otu <- otu_get(data)

  # check input data object class
  if (inherits(data, "ps_extra")) {
    ps <- ps_get(data)
    info <- info_get(data)
    if (!is.na(info[["tax_scale"]])) warning("data were already scaled: ", info[["tax_scale"]])
    info[["tax_scale"]] <- scaling_info
  } else if (methods::is(data, "phyloseq")) {
    ps <- data
    info <- new_ps_extra_info(tax_scale = scaling_info)
  }

  # actually do scaling
  otu <- base::scale(otu, center = center, scale = scale)
  otu <- phyloseq::otu_table(otu, taxa_are_rows = FALSE)

  # return appropriate class depending on input data class
  if (methods::is(data, "otu_table")) {
    return(otu)
  } else {
    phyloseq::otu_table(ps) <- otu
    return(new_ps_extra(ps = ps, info = info))
  }
}
