#' @title Beta binomial taxon models with corncob
#'
#' @description Takes a phyloseq and returns a list of models, one for each taxon.
#' Same independent variables for all models. Same variables for modelling both the abundance and dispersion parameters.
#' P-values are from wald tests for simplicity and speed... maybe I will change this in the future.
#'
#' @details `tax_model_corncob` can use parallel processing with the `future` package.
#' This speeds up analysis if you have many taxa to model.
#' Run a line like this beforehand: `future::plan(future::multisession, workers = 3)`
#'
#' @param ps phyloseq object
#' @param tax_level name of taxonomic rank to aggregate to and model taxa at
#' @param variables vector of variable names to use in corncob beta binomial model
#' @param taxa taxa to model (named, numbered, or defaulting to 'all')
#'
#' @importFrom rlang .data
#'
#' @examples
#' # corncob stats testing
#'
#' library(dplyr)
#' library(tibble)
#' library(phyloseq)
#' library(microbiome)
#' library(corncob)
#' library(future)
#' library(future.apply)
#'
#' data(dietswap)
#' ps <- dietswap
#'
#' # create some binary variables for easy visualisation
#' sample_data(ps)$female <- if_else(sample_data(ps)$sex == "female", 1, 0, NaN)
#' sample_data(ps)$overweight <- if_else(sample_data(ps)$bmi_group == "overweight", 1, 0, NaN)
#' sample_data(ps)$obese <- if_else(sample_data(ps)$bmi_group == "obese", 1, 0, NaN)
#'
#' # This example dataset has some taxa with the same name for phylum and family...
#' # We can fix problems like this with the prepend_ranks function
#' ps <- prepend_ranks(ps)
#' # this example dataset also has no root, this is unusual and needs to be fixed
#' tax_table(ps) <- cbind(root = "root", tax_table(ps))
#'
#' # filter out rare taxa
#' ps <- ps %>% tax_filter(min_prevalence = 0.2, min_total_abundance = 10000)
#'
#' # specify variables used for modelling
#' variables <- c("female", "overweight", "obese")
#'
#' # tax_model_corncob can use parallel processing with the futures package
#' # this speeds up analysis if you have many taxa to model
#' # e.g. # future::plan(future::multisession, workers = 3)
#'
#' # prep for tree viz #
#' # model taxa at all taxonomic levels (except root)
#' tax_models_list <- lapply(
#'   X = phyloseq::rank_names(ps)[-1],
#'   function(r) {
#'     message(Sys.time(), " - modelling at level: ", r)
#'     models <- tax_model_corncob(ps, tax_level = r, variables = variables, taxa = "all")
#'     return(models)
#'   }
#' )
#' # Flatten all taxonomic level lists into one list of models
#' # (this is why taxon names must be completely identifiable across ranks)
#' flat_models_list <- purrr::flatten(tax_models_list)
#' var_stats <- corncob_models_to_var_stats(flat_models_list)
#'
#' # future::plan(future::sequential) # to turn parallel processing back off
#' @export
#' @rdname tax_model_corncob
tax_model_corncob <- function(
                              ps,
                              tax_level,
                              variables,
                              taxa = "all") {

  # aggregate phyloseq at chosen rank level
  ps <- microbiome::aggregate_taxa(ps, level = tax_level)

  # get taxon names "at this level"
  all_possible_taxa <- unclass(phyloseq::tax_table(ps))[, tax_level, drop = TRUE]
  # ensure actual taxa names match this level
  phyloseq::taxa_names(ps) <- all_possible_taxa

  # define which taxa to model
  if (identical(taxa, "all")) {
    taxons <- all_possible_taxa
  } else if (class(taxa) %in% c("numeric", "integer")) {
    taxons <- all_possible_taxa[taxa]
  } else {
    if (all(taxa %in% all_possible_taxa)){
      taxons <- taxa
    } else {
      stop("The following taxon names are not present at the lowest rank:\n",
           paste(taxa[!taxa %in% all_possible_taxa], collapse = '\n'))
    }
  }

  # define right hand side of formula (same for all taxa)
  rhs <- paste(" ~", paste(variables, collapse = " + "))

  # define formula and model each taxon individually
  taxon_models <- future.apply::future_lapply(
    X = taxons,
    FUN = function(taxon) {
      # combine lhs and rhs formula
      f <- stats::as.formula(paste0("`", taxon, "`", rhs))
      # create bbdml model
      # phi.formula is for modelling dispersion (this is probably why it does NOT take a reponse var)
      res <- corncob::bbdml(formula = f, phi.formula = stats::as.formula(rhs), data = ps)
      return(res)
    }
  )
  names(taxon_models) <- taxons
  return(taxon_models)
}

#' @title Extract stats from corncob taxon model list
#'
#' @description Use this function with the output of `tax_model_corncob`. `corncob_models_to_var_stats` splits the statistical results extracted from corncob models and groups them by the independent variable name that they refer to. One dataframe of this list can then be joined to the output of tax_tree_nodes to prepare for taxonomic heat tree graph visualisation of taxon-variable associations.
#'
#' @param taxon_models named list output of `tax_model_corncob`
#'
#' @return list of dataframes, one df per independent variable
#' @export
#' @rdname tax_model_corncob
corncob_models_to_var_stats <- function(taxon_models) {
  # get stats from models
  taxon_stats <- lapply(taxon_models, corncob::waldt)
  taxon_stats <- lapply(names(taxon_stats), function(name) {
    df <- as.data.frame(taxon_stats[[name]])
    df$taxon_name <- name
    df <- tibble::rownames_to_column(df, var = "stat")
    df <- dplyr::rename(df, p = "Pr(>|t|)", t = "t value", se = "Std. Error", b = "Estimate")
    df <- dplyr::filter(df, !grepl("(Intercept)", .data$stat))
  })
  taxon_stats_df <- purrr::reduce(taxon_stats, rbind.data.frame)
  taxon_stats_df <- tidyr::separate(taxon_stats_df,
    col = "stat", into = c("param", "model_var"),
    extra = "merge", sep = "[.]", remove = TRUE
  )
  taxon_stats_wide <- tidyr::pivot_wider(
    data = taxon_stats_df,
    names_from = .data$param, values_from = dplyr::all_of(c("b", "se", "t", "p"))
  )
  stats_per_var <- split.data.frame(taxon_stats_wide, taxon_stats_wide$model_var)
  return(stats_per_var)
}
