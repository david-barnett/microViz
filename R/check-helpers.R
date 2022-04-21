

#' Check if rank exists in phyloseq
#'
#' Internal helper to check if given ranks are in phyloseq
#' and stop with informative error
#'
#' @param ps phyloseq object
#' @param rank rank to check
#' @param varname variable name from enclosing function (providing rank)
#' @param or NULL or alternatives also allowed? e.g. "unique"
#'
#' @return nothing
#' @noRd
psCheckRanks <- function(ps, rank, varname, or = NULL) {
  stopifnot(is.null(or) || is.character(or))
  rankNames <- phyloseq::rank_names(ps)
  or <- or[!or %in% rankNames] # avoid printing duplicate if `or` already there
  if (!rlang::is_string(rank) || !rank %in% c(rankNames, or)) {
    stop(
      paste0("\n`", varname, "` must be the name of a valid rank:\n"),
      paste(rankNames, collapse = " / "),
      ifelse(length(or) == 0, "", paste0("\nor: ", paste(or, collapse = " / ")))
    )
  }
}


# check variable names found in phyloseq and give nice error if not
psCheckVariables <- function(ps, vars) {
  varNames <- phyloseq::sample_variables(ps)
  isVariable <- vars %in% varNames
  if (any(!isVariable)) {
    missingVars <- vars[!isVariable]
    stop(
      "\n",
      paste(
        paste(missingVars, "is not a variable in phyloseq sample_data"),
        collapse = "\n"
      )
    )
  }
}