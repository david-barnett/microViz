# Everything in this file is copied (and very slightly modified) from an old version of speedyseq by Michael McLaren @mikemc
# source available here: https://github.com/mikemc/speedyseq/blob/1bacc47d58549a53f662d4f1c275caab9b3d3b80/R/psmelt.R

# Attribution: The documentation for `ps_melt()` is from phyloseq,
# https://github.com/joey711/phyloseq/blob/master/R/plot-methods.R

#' Melt phyloseq data object into large dataframe (tibble)
#'
#' @description
#' The ps_melt function is a specialized melt function for melting phyloseq objects
#' (instances of the phyloseq class), usually for producing graphics
#' with \code{\link[ggplot2]{ggplot}2}.
#' The naming conventions used in downstream phyloseq graphics functions
#' have reserved the following variable names that should not be used
#' as the names of \code{\link{sample_variables}}
#' or taxonomic \code{\link{rank_names}}.
#' These reserved names are \code{c("Sample", "Abundance", "OTU")}.
#' Also, you should not have identical names for
#' sample variables and taxonomic ranks.
#' That is, the intersection of the output of the following two functions
#' \code{\link{sample_variables}}, \code{\link{rank_names}}
#' should be an empty vector
#' (e.g. \code{intersect(sample_variables(ps), rank_names(ps))}).
#' All of these potential name collisions are checked-for
#' and renamed automatically with a warning.
#' However, if you (re)name your variables accordingly ahead of time,
#' it will reduce confusion and eliminate the warnings.
#'
#' NOTE: Code and documentation copied (and very slightly modified) from an old version of speedyseq by Michael McLaren.
#' speedyseq reimplements \code{psmelt} from \code{phyloseq} to use functions from the \code{tidyr}
#' and \code{dplyr} packages. The name in microViz is changed to \code{ps_melt} for consistency with other functions.
#'
#' @details
#' Note that ``melted'' phyloseq data is stored much less efficiently, and so
#' RAM storage issues could arise with a smaller dataset (smaller number of
#' samples/OTUs/variables) than one might otherwise expect.  For common sizes
#' of graphics-ready datasets, however, this should not be a problem.  Because
#' the number of OTU entries has a large effect on the RAM requirement, methods
#' to reduce the number of separate OTU entries -- for instance by
#' agglomerating OTUs based on phylogenetic distance using
#' \code{\link{tip_glom}} -- can help alleviate RAM usage problems.  This
#' function is made user-accessible for flexibility, but is also used
#' extensively by plot functions in phyloseq.
#'
#' @param ps (Required). An \code{\link{otu_table-class}} or
#'  \code{\link{phyloseq-class}}. Function most useful for phyloseq-class.
#'
#' @return A \code{\link{tibble}} class data frame.
#'
#' @seealso \code{\link{psmelt}}
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(phyloseq)
#' data("GlobalPatterns")
#' gp_ch <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
#' mdf <- ps_melt(gp_ch)
#' mdf2 <- psmelt(gp_ch) # slower
#' # same dataframe, except with somewhat different row orders
#' dplyr::all_equal(tibble::as_tibble(mdf), mdf2, convert = TRUE) # TRUE
#' nrow(mdf2)
#' ncol(mdf)
#' colnames(mdf)
#' head(rownames(mdf))
#' p <- ggplot(mdf, aes(x = SampleType, y = Abundance, fill = Genus))
#' p <- p + geom_bar(color = "black", stat = "identity", position = "stack")
#' # This example plot doesn't make any sense
#' print(p + coord_flip())
#' # TODO replace this...
ps_melt <- function(ps) {
  # Access covariate names from object, if present
  if (!methods::is(ps, class2 = "phyloseq")) {
    rankNames <- NULL
    sampleVars <- NULL
  } else {
    # Still might be NULL, but attempt access
    rankNames <- phyloseq::rank_names(ps, FALSE)
    sampleVars <- phyloseq::sample_variables(ps, FALSE)
  }
  # Define reserved names
  reservedVarnames <- c("Sample", "Abundance", "OTU")
  # type-1a conflict: between sample_data
  # and reserved psmelt variable names
  type1aconflict <- intersect(reservedVarnames, sampleVars)
  if (length(type1aconflict) > 0) {
    wh1a <- which(sampleVars %in% type1aconflict)
    new1a <- paste0("sample_", sampleVars[wh1a])
    # First warn about the change
    warning(
      "The sample variables: \n",
      paste(sampleVars[wh1a], collapse = ", "),
      "\n have been renamed to: \n",
      paste0(new1a, collapse = ", "), "\n",
      "to avoid conflicts with special phyloseq plot attribute names."
    )
    # Rename the sample variables.
    colnames(phyloseq::sample_data(ps))[wh1a] <- new1a
  }
  # type-1b conflict: between tax_table
  # and reserved psmelt variable names
  type1bconflict <- intersect(reservedVarnames, rankNames)
  if (length(type1bconflict) > 0) {
    wh1b <- which(rankNames %in% type1bconflict)
    new1b <- paste0("taxa_", rankNames[wh1b])
    # First warn about the change
    warning(
      "The rank names: \n",
      paste(rankNames[wh1b], collapse = ", "),
      "\n have been renamed to: \n",
      paste0(new1b, collapse = ", "), "\n",
      "to avoid conflicts with special phyloseq plot attribute names."
    )
    # Rename the conflicting taxonomic ranks
    colnames(phyloseq::tax_table(ps))[wh1b] <- new1b
  }
  # type-2 conflict: internal between tax_table and sample_data
  type2conflict <- intersect(sampleVars, rankNames)
  if (length(type2conflict) > 0) {
    wh2 <- which(sampleVars %in% type2conflict)
    new2 <- paste0("sample_", sampleVars[wh2])
    # First warn about the change
    warning(
      "The sample variables: \n'",
      paste0(sampleVars[wh2], collapse = "', '"),
      "'\n have been renamed to: \n",
      paste0(new2, collapse = ", "), "\n",
      "to avoid conflicts with taxonomic rank names."
    )
    # Rename the sample variables
    colnames(phyloseq::sample_data(ps))[wh2] <- new2
  }
  # Enforce OTU table orientation.
  otutab <- unclass(phyloseq::otu_table(ps))
  if (!phyloseq::taxa_are_rows(ps)) {
    otutab <- t(otutab)
  }
  ## Speedyseq specific code starts here
  # Convert the otu_table to a tibble in tall form
  # (one sample-taxon observation per row)
  tb <- otutab %>%
    tibble::as_tibble(rownames = "OTU") %>%
    tidyr::gather("Sample", "Abundance", -OTU)
  # Add the sample data if it exists
  if (!is.null(sampleVars)) {
    sam <- phyloseq::sample_data(ps) %>%
      methods::as("data.frame", strict = TRUE) %>%
      tibble::as_tibble(rownames = "Sample") # moves rownames to Sample col
    tb <- tb %>% dplyr::left_join(sam, by = "Sample")
  }
  # Add the tax table if it exists
  if (!identical(rankNames, NULL)) {
    tax <- phyloseq::tax_table(ps) %>%
      methods::as("matrix", strict = TRUE) %>%
      tibble::as_tibble(rownames = "OTU")
    # Convert taxonomy vars to factors for phyloseq compatibility
    if (getOption("stringsAsFactors")) {
      tax <- tax %>% dplyr::mutate(
        dplyr::across(-dplyr::all_of("OTU"), .fns = as.factor)
      )
    }
    tb <- tb %>% dplyr::left_join(tax, by = "OTU")
  }
  # Arrange by Abundance and OTU names within (to approx. phyloseq behaviour)
  tb <- tb %>%
    dplyr::arrange(dplyr::across(dplyr::all_of("OTU"))) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of("Abundance"), dplyr::desc))
  return(tb)
}
