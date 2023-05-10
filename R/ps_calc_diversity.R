#' Calculate diversity index and add to phyloseq sample data
#'
#' Wrapper around microbiome::diversity() function.
#' Takes and returns a phyloseq object. Calculates an alpha diversity index at
#' a given taxonomic rank. Returns phyloseq unaggregated, with an additional
#' variable. Variable name is by default created by pasting the index and rank.
#'
#' @details
#' Don't filter taxa before calculating alpha diversity.
#'
#' See the following resources for a discussion of exponentiated diversity indices
#' http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/Diversity%20of%20a%20single%20community.htm
#' http://www.loujost.com/Statistics%20and%20Physics/Diversity%20and%20Similarity/EffectiveNumberOfSpecies.htm
#'
#' @param ps phyloseq
#' @param rank taxonomic rank name, or "unique"
#' @param index
#' name of diversity index from microbiome::diversity().
#' One of: "inverse_simpson", "gini_simpson", "shannon", "fisher", "coverage"
#' @param exp exponentiate the result? (i.e. return e^index) - see details
#' @param varname name of the variable to be added to phyloseq sample data
#'
#' @return phyloseq
#' @export
#'
#' @examples
#' data(ibd_phylo, package = "corncob")
#' ibd_phylo %>%
#'   ps_filter(abx == "abx") %>%
#'   tax_fix() %>%
#'   ps_calc_diversity("Genus", index = "shannon", exp = TRUE) %>%
#'   ps_calc_diversity("Family", index = "inverse_simpson") %>%
#'   tax_transform(rank = "Genus", transform = "clr") %>%
#'   ord_calc("PCA") %>%
#'   ord_plot(
#'     colour = "exp_shannon_Genus", size = "inverse_simpson_Family"
#'   ) +
#'   ggplot2::scale_colour_viridis_c()
#'
ps_calc_diversity <- function(ps,
                              rank,
                              index = "shannon",
                              exp = FALSE,
                              varname = paste0(ifelse(exp, "exp_", ""), paste0(index, "_", rank))) {
  # argument checks
  stopifnot(is.logical(exp))
  if (!inherits(ps, "phyloseq")) stop("ps must be a phyloseq object")
  psCheckRanks(ps = ps, rank = rank, varname = "rank", or = "unique")
  index <- tolower(index)
  index <- rlang::arg_match0(arg = index, values = c(
    "inverse_simpson", "gini_simpson", "shannon", "fisher", "coverage"
  ))

  # calculate diversity at aggregated level
  psAgg <- tax_agg(ps = ps, rank = rank, force = TRUE) %>% ps_get()
  df <- microbiome::diversity(x = psAgg, index = index)


  # compute exponential function of index values if requested
  if (isTRUE(exp)) df[, index] <- exp(df[, index])
  # rename diversity variable
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

  # join diversity index variable to (original & unaggregated) phyloseq
  ps <- ps_join(
    x = ps, y = df, type = "left", .keep_all_taxa = TRUE,
    match_sample_names = ".rownames.", keep_sample_name_col = FALSE
  )
  return(ps)
}
