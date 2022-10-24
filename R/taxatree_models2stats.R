#' @title
#' Extract statistics from taxatree_models or tax_model output
#'
#' @description
#' Runs a function e.g. `broom::tidy` on a list of models, i.e. the output of
#' `taxatree_models` or `tax_model` (models list may often be attached to psExtra)
#'
#' @param data
#' psExtra with attached tax_models or taxatree_models list, or just the list of models
#' @param fun function to assist extraction of stats dataframe from models, or "auto"
#' @param ... extra arguments passed to fun
#' @param .keep_models should the models list be kept in the psExtra output?
#'
#' @return data.frame, attached to psExtra
#' @export
#' @describeIn
#' models2stats
#' Extract stats from list or psExtra output of taxatree_models
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
#' # inspect the psExtra returned, now with taxatree_stats dataframe added
#' lm_stats
#'
#' # inspect the dataframe itself
#' lm_stats %>% taxatree_stats_get()
#'
#' # keep the models on the psExtra object
#' lm_models %>% taxatree_models2stats(.keep_models = TRUE)
#'
#' # you can adjust the p values with taxatree_stats_p_adjust
#'
#' # you can plot the results with taxatree_plots
taxatree_models2stats <- function(data,
                                  fun = "auto",
                                  ...,
                                  .keep_models = FALSE) {
  if (!is.list(data)) check_is_psExtra(x = data, argName = "data")
  if (is(data, "psExtra")) models <- data@taxatree_models
  if (!is.list(models)) {
    stop("data arg psExtra must have 'taxatree_models' list attached")
  }
  if (inherits(data, "list")) {
    models <- data
  } else {
    stopifnot(all(names(models) %in% phyloseq::rank_names(ps_get(data))))
  }

  # get stats
  stats <- lapply(X = names(models), FUN = function(rank) {
    tax_models2stats(data = models[[rank]], rank = rank, fun = fun, ...)
  })
  stats <- purrr::reduce(stats, rbind.data.frame)
  stats <- dplyr::mutate(
    .data = stats,
    dplyr::across(dplyr::all_of(c("term", "rank")), ~ factor(.x, unique(.x)))
  )

  # return psExtra (based on input data class)
  if (is(data, "psExtra")) {
    data@taxatree_stats <- stats
    if (isFALSE(.keep_models)) data@taxatree_models <- NULL
    return(data)
  }
  # otherwise return data frame
  return(stats)
}

#' @param rank
#' string naming rank at which tax_model was run (needed if data is a list)
#'
#' @export
#' @describeIn
#' models2stats
#' Extract stats from list or psExtra output of tax_model
tax_models2stats <- function(data,
                             rank = NULL,
                             fun = "auto",
                             ...,
                             .keep_models = FALSE) {
  if (inherits(data, "list")) {
    models <- data
  } else {
    stopifnot(all(names(models) %in% phyloseq::rank_names(ps_get(data))))
  }

  if (!is.list(data)) check_is_psExtra(data, "data")
  if (is(data, "psExtra")) models <- data@tax_models
  if (!is.list(models)) {
    stop("data arg psExtra must have 'tax_models' list attached")
  }
  if (inherits(data, "list")) {
    if (is.null(rank)) stop("if `data` is just a list, `rank` must not be NULL")
    models <- data
  } else {
    rank <- names(models)
    if (!rlang::is_string(rank, string = phyloseq::rank_names(ps_get(data)))) {
      stop("tax_models list must be length 1 & have name of a rank in psExtra")
    }
    models <- models[[rank]] # remove one layer of nesting
  }

  # if univariable mode was on then this layer is a list e.g. v1:tax1;tax2;...
  if (identical(class(models[[1]]), "list")) models <- purrr::flatten(models)

  # map2 to avoid indexing by names, which are duplicated in univariable mode
  stat_list <- purrr::map2(
    .x = seq_along(models), .y = names(models), .f = function(x, y) {
      taxModel2stats(models[[x]], taxon = y, rank = rank, fun = fun, ...)
    }
  )
  stats <- purrr::reduce(stat_list, rbind.data.frame)

  # return psExtra or data.frame (based on input data class)
  if (is(data, "psExtra")) {
    data@tax_stats <- stats
    if (isFALSE(.keep_models)) data@tax_models <- NULL
    return(data)
  }
  return(stats)
}


# fun: a function (not a string), to compute stats from model, or "auto"
#
taxModel2stats <- function(model,
                           taxon,
                           rank,
                           fun = "auto",
                           dropTerms = "(Intercept)",
                           ...) {
  # check `fun` argument
  if (!inherits(fun, "function") && !identical(fun, "auto")) {
    stop(
      'fun must be a function or "auto", it is class: ',
      paste(class(fun), collapse = " / ")
    )
  }

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
  if (is.null(df[["term"]])) {
    # in univariable mode with cor.test etc, the term is not added by broom tidy
    # but we can retrieve it from the formula again
    df[["term"]] <- all.vars(stats::as.formula(df$formula[1]), unique = FALSE)[2]
  }
  df[["taxon"]] <- taxon
  df[["rank"]] <- rank
  df <- dplyr::relocate(df, dplyr::any_of(c("term", "taxon", "rank", "formula")))
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
