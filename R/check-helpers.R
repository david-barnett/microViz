

#' Check if rank exists in phyloseq
#'
#' Internal helper to check if given ranks are in phyloseq
#' and stop with informative error
#'
#' @param ps phyloseq object
#' @param rank rank to check
#' @param varname variable name from enclosing function (providing rank)
#'
#' @return nothing
#' @noRd
psCheckRanks <- function(ps, rank, varname) {
  rankNames <- phyloseq::rank_names(ps)
  if (!is.character(rank) || length(rank) != 1 || !rank %in% rankNames) {
    stop(
      paste(varname, "must be the name of a valid rank:\n"),
      paste(rankNames, collapse = " / ")
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
