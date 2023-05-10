#' Calculate richness estimate and add to phyloseq sample data
#'
#' Wrapper around microbiome::richness() function.
#' Takes and returns a phyloseq object. Calculates a richness estimate at
#' a given taxonomic rank. Returns phyloseq unaggregated, with an additional
#' variable. Variable name is by default created by pasting the index and rank.
#'
#' @details
#' Don't filter taxa before calculating richness.
#'
#' These richness indices are estimates.
#' For a discussion of the uncertainty and bias of these estimates
#' see e.g. work by Amy Willis https://doi.org/10.3389/fmicb.2019.02407
#'
#' @param ps phyloseq
#' @param rank taxonomic rank name, or "unique"
#' @param index
#' "observed" or "chao1" - name of richness estimate from microbiome::richness()
#' @param detection Detection threshold. Used for the "observed" index.
#' @param varname name of the variable to be added to phyloseq sample data
#'
#' @return phyloseq
#' @export
#'
#' @seealso \code{\link{ps_calc_diversity}}
#' @seealso \code{microbiome::\link[microbiome]{richness}}
#'
#' @examples
#' data(ibd_phylo, package = "corncob")
#' ibd_phylo %>%
#'   ps_filter(abx == "abx") %>%
#'   tax_fix() %>%
#'   ps_calc_richness("Genus", index = "observed") %>%
#'   ps_calc_richness("Family", index = "chao1") %>%
#'   tax_transform(rank = "Genus", transform = "clr") %>%
#'   ord_calc("PCA") %>%
#'   ord_plot(
#'     colour = "observed_Genus", size = "chao1_Family"
#'   ) +
#'   ggplot2::scale_colour_viridis_c()
#'
ps_calc_richness <- function(ps,
                             rank,
                             index = "observed",
                             detection = 0,
                             varname = paste0(index, "_", rank)) {
  # argument checks
  stopifnot(is.numeric(detection))
  if (!inherits(ps, "phyloseq")) stop("ps must be a phyloseq object")
  psCheckRanks(ps = ps, rank = rank, varname = "rank", or = "unique")
  index <- tolower(index)
  index <- rlang::arg_match0(arg = index, values = c("observed", "chao1"))

  # calculate richness at aggregated level
  psAgg <- tax_agg(ps = ps, rank = rank, force = TRUE) %>% ps_get()
  df <- microbiome::richness(x = psAgg, index = index, detection = detection)

  # rename richness variable
  colnames(df)[colnames(df) == index] <- varname
  # add rownames column to join by (as it is the sample names)
  df[[".rownames."]] <- rownames(df)

  # check if varname is already in the phyloseq sample data
  if (varname %in% phyloseq::sample_variables(ps)) {
    warning(
      varname, " is already a variable in phyloseq sample data -> OVERWRITING"
    )
    phyloseq::sample_data(ps)[[varname]] <- NULL
  }

  # join richness index variable to (original & unaggregated) phyloseq
  ps <- ps_join(
    x = ps, y = df, type = "left", .keep_all_taxa = TRUE,
    match_sample_names = ".rownames.", keep_sample_name_col = FALSE
  )
  return(ps)
}
