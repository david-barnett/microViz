#' Adjust p values in taxatree_stats dataframe
#'
#' @description
#' Apply a p value adjustment method from `stats::p.adjust.methods`,
#' such as false-discovery rate adjustment with "BH",
#' or more conservative family-wise error rate controlling methods such as "holm" or "bonferroni".
#'
#' @details
#' Define how to group the p values for adjustment with the `grouping` argument.
#' The default is to adjust the p values in groups at each taxonomic rank,
#' but you could also adjust per "model" / "taxon" or per "term".
#' Or even group by a combination of rank and term with c("rank", "term")
#'
#'
#' @param data ps_extra with taxatree_stats dataframe, or just the dataframe
#' @param grouping
#' defines grouping of p-values into families for adjustment, see details.
#' @param method any method from `stats::p.adjust.methods`
#' @param p name of variable containing p values for adjustment
#' @param new_var
#' name of new variable created for adjusted p values
#' (automatically inferred by default)
#'
#' @return ps_extra with dataframe of statistics, or just the data.frame
#' @export
#'
#' @seealso \code{\link{taxatree_models2stats}}
#' @seealso \code{\link{taxatree_models}}
#' @seealso \code{stats::\link[stats]{p.adjust}}
#'
#' @examples
#' # This example is an abbreviated excerpt from article on taxon modelling on
#' # the microViz documentation website
#'
#' library(corncob)
#' library(dplyr)
#' data("ibd_phylo", package = "corncob")
#'
#' # We'll keep only the Ulcerative Colitis and Healthy Control samples, to
#' # simplify the analyses for this example. We'll also remove the Species
#' # rank information, as most OTUs in this dataset are not assigned to a
#' # species. We'll also use `tax_fix` to fill any gaps where the Genus is
#' # unknown, with the family name or whatever higher rank classification is
#' # known.
#'
#' phylo <- ibd_phylo %>%
#'   ps_filter(DiseaseState %in% c("UC", "nonIBD")) %>%
#'   tax_mutate(Species = NULL) %>%
#'   tax_fix()
#'
#' # Let's make some sample data variables that are easier to use and compare
#' # in the statistical modelling ahead. We will convert dichotomous
#' # categorical variables into similar binary variables (values: 1 for true,
#' # or 0 for false). We will also scale and center the numeric variable for
#' # age.
#'
#' phylo <- phylo %>%
#'   ps_mutate(
#'     UC = ifelse(DiseaseState == "UC", yes = 1, no = 0),
#'     female = ifelse(gender == "female", yes = 1, no = 0),
#'     antibiotics = ifelse(abx == "abx", yes = 1, no = 0),
#'     steroids = ifelse(steroids == "steroids", yes = 1, no = 0),
#'     age_scaled = scale(age, center = TRUE, scale = TRUE)
#'   )
#'
#' bb_models <- phylo %>%
#'   tax_fix() %>%
#'   tax_prepend_ranks() %>%
#'   tax_filter(min_prevalence = 0.3) %>%
#'   taxatree_models(
#'     type = corncob::bbdml,
#'     ranks = c("Phylum", "Class", "Order"),
#'     variables = c("UC", "female", "antibiotics", "steroids", "age_scaled")
#'   )
#'
#' bb_stats <- bb_models %>%
#'   taxatree_models2stats(param = "mu") %>%
#'   taxatree_stats_p_adjust(method = "BH", grouping = "rank")
#'
#' bb_stats
#'
#' bb_stats$taxatree_stats
#'
#' # you can also directly modify the dataframe,
#' # and choose a different variable name
#' bb_stats$taxatree_stats %>%
#'   taxatree_stats_p_adjust(
#'     method = "holm", grouping = "taxon", new_var = "p_adj_holm"
#'   )
#'
#' # see all available adjustment methods
#' stats::p.adjust.methods
taxatree_stats_p_adjust <- function(data,
                                    method,
                                    grouping = "rank",
                                    p = "p.value",
                                    new_var = paste0(
                                      "p.adj.", method, ".", grouping
                                    )) {
  if (inherits(data, "ps_extra") && is.data.frame(data[["taxatree_stats"]])) {
    df <- data[["taxatree_stats"]]
  } else if (inherits(data, "data.frame")) {
    df <- data
  } else {
    stop(
      "data must be a ps_extra with taxatree_stats or just the stats dataframe"
    )
  }

  if (identical(grouping, "model") || identical(grouping, "taxon_name")) {
    grouping <- "taxon"
  }

  df <- dplyr::group_by(df, dplyr::across(dplyr::all_of(grouping)))
  df <- dplyr::mutate(
    .data = df, dplyr::across(
      .cols = dplyr::all_of(p),
      .fns = function(x) stats::p.adjust(x, method = method),
      .names = new_var
    )
  )

  # return ps_extra or data.frame (based on input data class)
  if (inherits(data, "ps_extra")) {
    data[["taxatree_stats"]] <- df
  } else if (inherits(data, "data.frame")) {
    data <- df
  }
  return(data)
}