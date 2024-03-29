#' Statistical modelling for individual taxa across multiple ranks
#'
#' @description
#' `taxatree_models` runs `tax_model` on every taxon at multiple taxonomic
#' ranks (you choose which ranks with the plural `ranks` argument).
#' It returns the results as a named nested list of models
#' attached to a psExtra object.
#' One list per rank, one model per taxon at each rank.
#'
#' The result can then be used with `taxatree_models2stats` to extract a
#' dataframe of statistics for use with `taxatree_plots`.
#'
#' @param ps phyloseq object or psExtra
#' @param ranks vector of rank names at which to aggregate taxa for modelling
#' @param type name of regression modelling function, or the function itself
#' @param variables
#' vector of variable names, to be used as model formula right hand side.
#' If variables is a list, not a vector, a model is fit for each entry in list.
#' @param formula
#' Right hand side of a formula, as a formula object or character string. Or a list of these.
#' (alternative to variables argument, do not provide both)
#' @param use_future
#' if TRUE parallel processing with future is possible, see details of ?tax_model.
#' @param checkVars check variance of variables?
#' @param checkNA check variables for NAs?
#' @param verbose
#' message about progress: "rank" only notifies which rank is being processed;
#' TRUE notifies you about each taxon being processed; FALSE for no messages.
#' @param trans
#' name of tax_transform transformation to apply to aggregated taxa before fitting statistical models
#' @param trans_args
#' named list of any additional arguments to tax_transform e.g. list(zero_replace = "halfmin")
#' @param ... extra arguments are passed directly to modelling function
#'
#' @seealso \code{\link{tax_model}} for more details and examples
#' @seealso \code{\link{taxatree_plots}} for how to plot the output of `taxatree_models`
#'
#' @export
taxatree_models <- function(ps,
                            ranks = NULL,
                            type = "lm",
                            variables = NULL,
                            formula = NULL,
                            use_future = FALSE,
                            checkVars = TRUE,
                            checkNA = "warning",
                            verbose = "rank",
                            trans = "identity",
                            trans_args = list(),
                            ...) {
  check_is_phyloseq(ps, argName = "ps")
  data <- as(ps, "psExtra")
  ps <- ps_get(ps) # This probably is not safe.
  ranks <- taxatree_modelsGetRanks(ps = ps, ranks = ranks)
  taxatree_modelsCheckDupes(ps = ps, ranks = ranks)

  tax_models_list <- lapply(
    X = ranks, FUN = function(r) {
      if (!isFALSE(verbose)) message(Sys.time(), " - modelling at rank: ", r)

      models <- tax_model(
        ps = ps, rank = r, trans = trans, trans_args = trans_args,
        type = type, variables = variables, formula = formula,
        checkVars = checkVars, checkNA = checkNA, verbose = verbose,
        use_future = use_future, return_psx = FALSE, ...
      )
      return(models)
    }
  )
  names(tax_models_list) <- ranks

  # attach models list to psExtra
  data@taxatree_models <- tax_models_list
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
    rlang::abort(call = rlang::caller_env(), message = c(
      "One of more of these ranks are not in rank_names(ps): ",
      ">" = paste(ranks, collapse = " ")
    ))
  }
  return(ranks)
}


# check for entries duplicated across ranks in tax_table
taxatree_modelsCheckDupes <- function(ps, ranks) {
  tt <- phyloseq::tax_table(ps)[, ranks, drop = FALSE]
  uniques <- apply(tt, MARGIN = 2, unique)
  if (anyDuplicated(unlist(uniques))) {
    rlang::abort(call = rlang::caller_env(), message = c(
      "Some elements in tax_table(ps) are in >1 of the selected ranks.",
      ">" = "Consider using tax_prepend_ranks(ps) first, to fix this problem.",
      i = "Or run `taxatree_nodes(ps)` for a more informative error."
    ))
  }
}
