#' @name Taxon-modelling
#' @title Statistical modelling for individual taxa in a phyloseq
#'
#' @description
#' - `tax_model` takes a phyloseq and returns a list of models, one for each taxon
#' - Same independent variables are used for all models, as specified in `variables` or `formula` argument (latter takes precedence).
#' - For `type = "bbdml"` the same variables/formula arg is used for modelling both the abundance and dispersion parameters.
#' - `taxatree_models` runs `tax_model` at multiple taxonomic ranks. Specify ranks to run over in `tax_levels` (plural!) argument.
#'
#' @details
#' `tax_model` can use parallel processing with the `future` package.
#' This can speed up analysis if you have many taxa to model.
#' Run a line like this beforehand: `future::plan(future::multisession, workers = 3)`
#'
#' @param ps phyloseq object
#' @param tax_level name of taxonomic rank to aggregate to and model taxa at
#' @param tax_levels names of ranks to model taxa at (or their index) or defaults to all ranks except the first
#' @param type name of modelling function to use
#' @param variables vector of variable names to use in statistical model as right hand side
#' @param formula right hand side of a formula, as a formula object or character value
#' @param taxa taxa to model (named, numbered, logical selection, or defaulting to all if NULL)
#' @param verbose message about progress and any taxa name modifications
#' @param ... extra args passed directly to modelling function
#'
#' @return list of model objects or list of lists
#'
#' @examples
#' # corncob stats testing
#' library(dplyr)
#' library(tibble)
#' library(phyloseq)
#' library(microbiome)
#' library(corncob)
#'
#' data(dietswap)
#' ps <- dietswap
#'
#' # create some binary variables for easy visualisation
#' ps <- ps %>% ps_mutate(
#'   female = if_else(sex == "female", 1, 0, NaN),
#'   overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
#'   obese = if_else(bmi_group == "obese", 1, 0, NaN)
#' )
#'
#' # This example dataset has some taxa with the same name for phylum and family...
#' # We can fix problems like this with the tax_prepend_ranks function
#' ps <- tax_prepend_ranks(ps)
#'
#' # filter out rare taxa
#' ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
#'
#' # specify variables used for modelling
#' VARS <- c("female", "overweight", "obese")
#'
#' # Model first 3 genera using all VARS as predictors
#' models <- tax_model(ps, tax_level = "Genus", taxa = 1:3, variables = VARS)
#' # Alternative method using formula arg instead of variables to produce identical results
#' models2 <- tax_model(ps, tax_level = "Genus", taxa = 1:3, formula = ~ female + overweight + obese)
#' all.equal(models, models2) # should be TRUE
#' # Model only one genus, NOTE the modified name, which was returned by tax_prepend_ranks defaults
#' models3 <- ps %>%
#'   tax_model(tax_level = "Genus", taxa = "G: Bacteroides fragilis et rel.", variables = VARS)
#' # Model all taxa at multiple taxonomic ranks (ranks 1 and 2) using only female variable as predictor
#' models4 <- taxatree_models(ps, tax_levels = 1:2, formula = ~female, verbose = FALSE)
#' @rdname Taxon-modelling
#' @export
tax_model <- function(ps, tax_level, type = "bbdml", variables = NULL, formula = NULL, taxa = NULL, verbose = TRUE, ...) {

  # check phyloseq for common problems (and fix or message about this)
  ps <- phyloseq_validate(ps, verbose = TRUE)

  # aggregate phyloseq at chosen rank level
  ps <- microbiome::aggregate_taxa(ps, level = tax_level)

  # default to modelling all taxa
  if (identical(taxa, NULL)) taxa <- TRUE

  # get taxon names "at this level"
  possible_taxa <- unclass(phyloseq::tax_table(ps))[, tax_level, drop = TRUE]

  # define which taxa to model
  if (class(taxa) %in% c("numeric", "integer", "logical")) {

    # get (subset of) taxon names "at this level"
    taxons <- possible_taxa[taxa]
    tt_names <- phyloseq::taxa_names(ps)[taxa]
  } else if (any(!taxa %in% possible_taxa)) {
    stop(
      "The following taxon names are not present at the chosen aggregation level rank:\n",
      paste(taxa[!taxa %in% possible_taxa], collapse = "\n")
    )
  } else {
    taxons <- taxa
    taxa <- which(possible_taxa %in% taxa)
    tt_names <- phyloseq::taxa_names(ps)[taxa]
  }

  # ensure actual taxa names match this level
  not_matching <- tt_names != taxons
  if (any(not_matching)) {
    if (!isFALSE(verbose)) {
      message("Changing ", sum(not_matching), " taxa_names that don't match taxa found at level of ", tax_level)
      for (non_match in which(not_matching)) {
        message(tt_names[non_match], " --> ", taxons[non_match])
      }
    }
    phyloseq::taxa_names(ps)[taxa] <- taxons
  }


  # handle formula or variables
  # defines right hand side of formula (which is same for all taxa)
  if (
    (identical(formula, NULL) && identical(variables, NULL)) ||
      (!identical(formula, NULL) && !identical(variables, NULL))
  ) {
    stop("Please provide EITHER formula (rhs) OR character vector of variables!")
  } else if (!identical(formula, NULL)) {
    rhs <- stats::as.formula(formula)
  } else {
    rhs <- stats::as.formula(paste(" ~", paste(variables, collapse = " + ")))
  }

  if (identical(type, "bbdml")) {

    # define specific formulas and model each taxon individually
    taxon_models <- future.apply::future_lapply(
      future.seed = TRUE,
      X = taxons,
      FUN = function(taxon) {
        if (!isFALSE(verbose)) message("Modelling: ", taxon)
        # combine lhs and rhs formula
        f <- stats::update.formula(rhs, stats::as.formula(paste0("`", taxon, "`", " ~ .")))
        # create bbdml model
        # phi.formula is for modelling dispersion (this is probably why it does NOT take a reponse var)
        if (!exists(x = "phi.formula", inherits = FALSE)) phi.formula <- rhs
        res <- corncob::bbdml(formula = f, phi.formula = phi.formula, data = ps, ...)
        return(res)
      }
    )
  } else {
    stop("So far only beta binomial taxon models with corncob::bbdml are supported, with type = 'bbdml'")
  }
  names(taxon_models) <- taxons
  return(taxon_models)
}

#'
#' @rdname Taxon-modelling
#' @export
taxatree_models <- function(ps, tax_levels = NULL, type = "bbdml", variables = NULL, formula = NULL, verbose = TRUE, ...) {
  ranknames <- phyloseq::rank_names(ps)

  if (identical(tax_levels, NULL)) {
    tax_levels <- ranknames[-1]
  } else if (class(tax_levels) %in% c("numeric", "integer", "logical")) {
    tax_levels <- ranknames[tax_levels]
  } else if (any(!tax_levels %in% ranknames)) {
    stop("One of more of these tax_levels are not in rank_names(ps): ", paste(tax_levels, collapse = " "))
  }

  # check for entries duplicated across ranks
  tt <- phyloseq::tax_table(ps)[, tax_levels]
  uniques <- apply(tt, MARGIN = 2, unique)
  if (anyDuplicated(unlist(uniques))) {
    stop("Some elements in tax_table(ps) appear in more than one of the selected ranks.\nConsider using tax_prepend_ranks(ps) first, to fix this problem.")
  }

  tax_models_list <- lapply(
    X = tax_levels,
    function(r) {
      message(Sys.time(), " - modelling at level: ", r)
      models <- tax_model(ps = ps, tax_level = r, type = "bbdml", variables = variables, formula = formula, verbose = verbose, ...)
      return(models)
    }
  )
  names(tax_models_list) <- tax_levels
  return(tax_models_list)
}

# `models2stats_corncob`extracts stats from corncob taxon model list
#
# models2stats_corncob is used inside taxatree_plots, with the output of `tax_model(type = "bbdml")`.
# It splits the statistical results extracted from corncob models and groups them by the independent variable name that they refer to.
# One dataframe of this list can then be joined to the output of taxatree_nodes to prepare for taxonomic heat tree graph visualisation of taxon-variable associations.
#
# @param taxon_models named list output of `tax_model(type = "bbdml")`
# @return list of dataframes, one df per independent variable
models2stats_corncob <- function(taxon_models) {
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