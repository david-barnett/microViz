#' Statistical modelling for individual taxa across multiple ranks
#'
#' @description
#' `taxatree_models` runs `tax_model` on every taxon at multiple taxonomic
#' ranks (you choose which ranks with the plural `ranks` argument).
#' It returns the results as a named nested list of models
#' attached to a ps_extra object.
#' One list per rank, one model per taxon at each rank.
#'
#' The result can then be used with `taxatree_models2stats` to extract a
#' dataframe of statistics for use with `taxatree_plots`.
#'
#' @param ps phyloseq object or ps_extra
#' @param ranks vector of rank names at which to aggregate taxa for modelling
#' @param type name of regression modelling function, or the function itself
#' @param variables
#' vector of variable names, to be used as model formula right hand side
#' (ignored if formula given)
#' @param formula
#' (alternative to variables arg) right hand side of a formula,
#' as a formula object or character value
#' @param verbose
#' message about progress: "rank" only notifies which rank is being processed;
#' TRUE notifies you about each taxon being processed; FALSE for no messages.
#' @param ... extra arguments are passed directly to modelling function
#'
#' @export
taxatree_models <- function(ps,
                            ranks = NULL,
                            type = "lm",
                            variables = NULL,
                            formula = NULL,
                            univariable = FALSE,
                            verbose = "rank",
                            ...) {
  data <- as_ps_extra(ps)
  ps <- ps_get(ps)
  ranks <- taxatree_modelsGetRanks(ps = ps, ranks = ranks)
  taxatree_modelsCheckDupes(ps = ps, ranks = ranks)

  tax_models_list <- lapply(
    X = ranks,
    function(r) {
      if (!isFALSE(verbose)) message(Sys.time(), " - modelling at rank: ", r)
      models <- tax_model(
        ps = ps, rank = r, type = type, variables = variables,
        formula = formula, univariable = univariable, verbose = verbose, ...
      )
      return(models)
    }
  )
  names(tax_models_list) <- ranks

  # attach models list to ps_extra
  data[["taxatree_models"]] <- tax_models_list
  return(data)
}

# get ranks as strings
taxatree_modelsGetRanks <- function(ps, ranks) {
  ranknames <- phyloseq::rank_names(ps)
  if (identical(ranks, NULL)) {
    ranks <- ranknames[-1]
  } else if (class(ranks) %in% c("numeric", "integer", "logical")) {
    ranks <- ranknames[ranks]
  } else if (any(!ranks %in% ranknames)) {
    stop(
      "One of more of these ranks are not in rank_names(ps): ",
      paste(ranks, collapse = " ")
    )
  }
  return(ranks)
}


# check for entries duplicated across ranks in tax_table
taxatree_modelsCheckDupes <- function(ps, ranks) {
  tt <- phyloseq::tax_table(ps)[, ranks, drop = FALSE]
  uniques <- apply(tt, MARGIN = 2, unique)
  if (anyDuplicated(unlist(uniques))) {
    stop(
      "Some elements in tax_table(ps) are in >1 of the selected ranks.",
      "\nConsider using tax_prepend_ranks(ps) first, to fix this problem.",
      "\nOr run `taxatree_nodes(ps)` for a more informative error."
    )
  }
}
