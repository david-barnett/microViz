#
#
# models2stats is used inside taxatree_plots, with the output of `tax_model()`.
# It can extract the statistical results from various models and labels
# them by the independent variable name that they refer to.
#
# Rows for one model_var from this df can then be joined to the output of
# taxatree_nodes to prepare for taxonomic heat tree graph visualisation of
# taxon-variable associations.
#
# Use `split.data.frame(taxon_stats_df, taxon_stats_df[["model_var"]])`
# to split into variable-specific dataframes
#
# @param taxon_models named list output of `tax_model`
# @return

#' Extract dataframe of statistics from taxatree_models list
#'
#' @param data ps_extra with taxatree_models, or just the list of models
#' @param fun function to assist extraction of stats dataframe from models
#' @param ... extra arguments passed to fun
#' @param .keep_models should the models list be kept in the ps_extra output?
#'
#' @return data.frame, attached to ps_extra
#' @export
#'
#' @examples
#' # This example is an abbreviated excerpt from article on taxon modelling on
#' # the microViz documentation website
#'
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
#' lm_models <- phylo %>%
#'   tax_fix() %>%
#'   tax_prepend_ranks() %>%
#'   tax_transform("compositional", rank = "Genus") %>%
#'   tax_filter(min_prevalence = 0.1, use_counts = TRUE) %>%
#'   tax_transform("log2", zero_replace = "halfmin", chain = TRUE) %>%
#'   taxatree_models(
#'     type = lm,
#'     ranks = c("Phylum", "Class", "Genus"),
#'     variables = c("UC", "female", "antibiotics", "steroids", "age_scaled")
#'   )
#'
#'
#' lm_stats <- lm_models %>% taxatree_models2stats()
#'
#' # inspect the ps_extra returned, now with taxatree_stats dataframe added
#' lm_stats
#'
#' # inspect the dataframe itself
#' lm_stats$taxatree_stats
#'
#' # keep the models on the ps_extra object
#' lm_models %>% taxatree_models2stats(.keep_models = TRUE)
#'
#' # you can adjust the p values with taxatree_stats_p_adjust
#'
#' # you can plot the results with taxatree_plots
taxatree_models2stats <- function(data,
                                  fun = "auto",
                                  ...,
                                  .keep_models = FALSE) {
  if (inherits(data, "ps_extra") && is.list(data[["taxatree_models"]])) {
    taxon_models <- data[["taxatree_models"]]
  } else if (inherits(data, "list")) {
    taxon_models <- data
  } else {
    stop("data must be a ps_extra with taxatree_models list, or just the list")
  }

  # check `fun` argument
  if (!inherits(fun, "function") && !identical(fun, "auto")) {
    stop(
      'fun must be a function or "auto", it is class: ',
      paste(class(fun), collapse = " / ")
    )
  }

  # get stats
  stats <- lapply(X = names(taxon_models), FUN = function(rank) {
    tax_models2stats(models = taxon_models[[rank]], rank = rank, fun = fun, ...)
  })
  stats <- purrr::reduce(stats, rbind.data.frame)
  stats <- dplyr::mutate(
    .data = stats,
    dplyr::across(c(.data$term, .data$rank), ~ factor(.x, unique(.x)))
  )

  # return ps_extra or data.frame (based on input data class)
  if (inherits(data, "ps_extra")) {
    data[["taxatree_stats"]] <- stats
    if (isFALSE(.keep_models)) data[["taxatree_models"]] <- NULL
  } else if (inherits(data, "list")) {
    data <- stats
  }
  return(data)
}

# runs model2stats on a simple list, using names of list items as taxon name
tax_models2stats <- function(models, rank, fun = "auto", ...) {
  # if univariable mode was on then this layer is a list e.g. v1:tax1;tax2;...
  if (identical(class(models[[1]]), "list")) models <- purrr::flatten(models)

  # map2 to avoid indexing by names, which are duplicated in univariable mode
  out_list <- purrr::map2(
    .x = seq_along(models), .y = names(models), .f = function(x, y) {
      taxModel2stats(models[[x]], taxon = y, rank = rank, fun = fun, ...)
    }
  )
  out <- purrr::reduce(out_list, rbind.data.frame)
  return(out)
}


# fun: a function (not a string), to compute stats from model, or "auto"
#
taxModel2stats <- function(model,
                           taxon,
                           rank,
                           fun = "auto",
                           dropTerms = "(Intercept)",
                           ...) {
  # infer tidying function from model type if "auto" fun
  if (identical(fun, "auto")) {
    # for most models, assume broom::tidy has an appropriate method
    fun <- broom::tidy
    # special tidying for corncob bbdml models
    if (inherits(model, "bbdml")) fun <- tidy_bbdml
  }
  # run tidying function, with any extra arguments
  df <- fun(model, ...)
  # check result is actually a dataframe
  if (!inherits(df, "data.frame")) {
    stop(
      "`fun` function must return a data.frame or tibble.\n",
      "It returned an object of class: ", paste(class(df), collapse = " / ")
    )
  }

  # add taxon name and clean up dataframe
  df[["formula"]] <- attr(x = model, which = "formula_string", exact = TRUE)
  df[["taxon"]] <- taxon
  df[["rank"]] <- rank
  df <- dplyr::relocate(df, dplyr::all_of(c("term", "taxon", "rank", "formula")))
  df <- dplyr::filter(df, !.data[["term"]] %in% dropTerms)
  return(tibble::as_tibble(df))
}


# gets stats from corncob::bbdml model
tidy_bbdml <- function(model,
                       param = "mu",
                       method = corncob::waldt,
                       ...) {

  # get stats from model
  stats <- method(model, ...)
  df <- as.data.frame(stats, optional = TRUE)
  df <- tibble::rownames_to_column(df, var = "rownames")
  # standardise names in accordance with vibe of broom tidy guidelines
  df <- dplyr::rename(
    .data = df,
    p.value = "Pr(>|t|)", t.statistic = "t value",
    std.error = "Std. Error", estimate = "Estimate"
  )
  # bbdml models have mu (abundance) and phi (dispersion) estimates
  df <- tidyr::separate(
    data = df,
    col = "rownames", into = c("parameter", "term"),
    extra = "merge", sep = "[.]", remove = TRUE
  )
  # keep only desired statistical parameter ("mu", "phi" or both?)
  df <- dplyr::filter(df, .data$parameter %in% param)
  return(df)
}

