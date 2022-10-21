#' @title Statistical modelling for individual taxa in a phyloseq
#'
#' @description
#' `tax_model` provides a simple framework to statistically model the abundance
#' of individual taxa in your data.
#' You can choose which type of statistical model you want to fit, and you can
#' choose at which rank and (optionally) which specific taxa to fit statistical models for.
#' `tax_model` takes a phyloseq and returns a list of statistical models, one model for each taxon.
#' The same independent variables are used for all models,
#' as specified in `variables` or `formula` argument (latter takes precedence).
#'
#' `taxatree_models` runs `tax_model` on every taxon at multiple taxonomic ranks
#' (you choose which ranks with the plural `ranks` argument),
#' and returns the results as a named nested list designed for use with `taxatree_plots`.
#' One list per rank, one model per taxon at each rank.
#'
#' `type = "bbdml"` will run beta binomial regression model(s) using the `corncob` package.
#' For bbdml the same formula/variables is/are used for modelling both the
#' abundance and dispersion parameters.
#'
#' @details
#' `tax_model` and `taxatree_models` can use parallel processing with the `future` package.
#' This can speed up analysis if you have many taxa to model. Set use_future = TRUE and
#' run a line like this before doing your modelling: `future::plan(future::multisession, workers = 3)`
#' (This requires the future and future.apply packages to be installed.)
#'
#' @param ps phyloseq object
#' @param rank name of taxonomic rank to aggregate to and model taxa at
#' @param type name of modelling function to use, or the function itself
#' @param variables
#' vector of variable names, to be used as model formula right hand side.
#' If variables is a list, not a vector, a model is fit for each entry in list.
#' @param formula
#' Right hand side of a formula, as a formula object or character string. Or a list of these.
#' (alternative to variables argument, do not provide both)
#' @param taxa
#' taxa to model (named, numbered, logical selection, or defaulting to all if NULL)
#' @param use_future
#' if TRUE parallel processing with future is possible, see details.
#' @param return_psx if TRUE result will be returned attached to ps_extra object
#' @param checkVars should the predictor variables be checked for zero variance?
#' @param checkNA
#' One of "stop", "warning", "message", or "allow", which
#' indicates whether to check predictor variables for NAs, and how to report any NAs if present?
#' @param verbose message about progress and any taxa name modifications
#' @param ... extra args passed directly to modelling function
#'
#' @return
#' Named list of model objects or list of lists. Or, if return_psx is TRUE, a ps_extra.
#' @seealso \code{\link{taxatree_models}} for more details on the underlying approach
#' @seealso \code{\link{taxatree_plots}} for how to plot the output of `taxatree_models`
#'
#' @examples
#' library(corncob)
#' library(dplyr)
#'
#' data(dietswap, package = "microbiome")
#' ps <- dietswap
#'
#' # create some binary variables for easy visualization
#' ps <- ps %>% ps_mutate(
#'   female = if_else(sex == "female", 1, 0, NaN),
#'   overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
#'   obese = if_else(bmi_group == "obese", 1, 0, NaN)
#' )
#'
#' # This example HITChip dataset has some taxa with the same name for phylum and family...
#' # We can fix problems like this with the tax_prepend_ranks function
#' ps <- tax_prepend_ranks(ps)
#'
#' # filter out rare taxa (it is often difficult to fit multivariable models to rare taxa)
#' ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
#'
#' # specify variables used for modelling
#' VARS <- c("female", "overweight", "obese")
#'
#' # Model first 3 genera using all VARS as predictors (just for a quick test)
#' models <- tax_model(ps, type = "bbdml", rank = "Genus", taxa = 1:3, variables = VARS)
#'
#' # Alternative method using formula arg instead of variables to produce identical results
#' models2 <- tax_model(
#'   ps = ps, rank = "Genus", type = "bbdml",
#'   taxa = 1:3, formula = ~ female + overweight + obese
#' )
#' all.equal(models, models2) # should be TRUE
#'
#' # Model only one genus, NOTE the modified name,
#' # which was returned by tax_prepend_ranks defaults
#' models3 <- ps %>%
#'   tax_model(
#'     rank = "Genus", type = "bbdml",
#'     taxa = "G: Bacteroides fragilis et rel.", variables = VARS
#'   )
#'
#' # Model all taxa at multiple taxonomic ranks (ranks 1 and 2)
#' # using only female variable as predictor
#' models4 <- taxatree_models(
#'   ps = ps, type = "bbdml", ranks = 1:2, formula = ~female, verbose = FALSE
#' )
#'
#' # modelling proportions with simple linear regression is also possible via type = lm
#' # and transforming the taxa to compositional first
#' models_lm <- ps %>%
#'   tax_transform("compositional") %>%
#'   tax_model(rank = "Genus", taxa = 1:3, variables = VARS, type = "lm")
#' @export
tax_model <- function(ps,
                      rank,
                      type = "lm",
                      variables = NULL,
                      formula = NULL,
                      taxa = NULL,
                      use_future = FALSE,
                      return_psx = FALSE,
                      checkVars = TRUE,
                      checkNA = "warning",
                      verbose = TRUE,
                      ...) {
  if (!rlang::is_string(type) && !rlang::is_function(type)) {
    stop("`type` must be a string naming a modelling function, or a function")
  }
  if (!rlang::is_bool(checkVars)) stop("checkVars must be TRUE or FALSE")

  # store input data as ps_extra if user wants result returned as attachment
  if (isTRUE(return_psx)) data <- as_ps_extra(ps)

  ps <- ps_get(ps)
  # check phyloseq for common problems (and fix or message about this)
  ps <- phyloseq_validate(ps, remove_undetected = TRUE, verbose = TRUE)

  # aggregate phyloseq at chosen rank level
  ps <- tax_agg(ps, rank = rank) %>% ps_get()

  # check actual phyloseq taxa_names match this level after aggregating as
  # sometimes the names are not changed by tax_agg (when no aggregation occurs)
  ps <- taxEnsureNamesMatchRank(ps = ps, rank = rank, verbose = verbose)

  # get numeric index of taxa_names (taxa) in (aggregated) phyloseq (ps)
  taxIndex <- taxIndexGet(ps, taxa)
  if (anyNA(taxIndex)) {
    stop(
      "The following taxon names are not present at rank ", rank,
      "\n", paste(taxa[is.na(taxIndex)], collapse = "\n")
    )
  }
  if (max(taxIndex) > phyloseq::ntaxa(ps)) {
    stop(
      "There are only ", phyloseq::ntaxa(ps),
      " taxa at rank of ", rank, ", not ", max(taxIndex)
    )
  }
  taxNames <- phyloseq::taxa_names(ps)[taxIndex]

  # make a string representing a formula
  # can be a vector result if univariable = TRUE
  fstring_rhs <- formulaMakerRHSstring(formula = formula, variables = variables)

  # check variables
  if (checkVars || checkNA != "allow") {
    df <- samdatAsDataframe(ps)
    vars <- lapply(fstring_rhs, function(f) all.vars(stats::as.formula(f)))
    vars <- unique(simplify2array(vars))
    if (checkNA != "allow") {
      lapply(vars, function(v) checkNAs(df[[v]], name = v, fun = checkNA))
    }
    if (checkVars) {
      lapply(vars, function(v) checkVariance(df[[v]], name = v))
    }
  }

  # set lapply function to future.apply version if requested
  if (isTRUE(use_future)) {
    checkPackageAvailable(
      packages = c("future", "future.apply"),
      message = " package is required when use_future is set to TRUE"
    )
    LAPPLY <- future.apply::future_lapply
  } else {
    LAPPLY <- lapply
  }

  # define specific formulas and model each taxon individually
  taxon_models <- LAPPLY(X = taxNames, FUN = function(taxon) {
    if (isTRUE(verbose)) message("Modelling: ", taxon)
    # loop over fstring_rhs in case this is a vector, i.e. in univariable mode
    mods <- lapply(fstring_rhs, function(f) {
      taxonModel(ps = ps, type = type, taxon = taxon, fstring_rhs = f, ...)
    })
    names(mods) <- gsub(x = fstring_rhs, pattern = "^ ?~ ?", replacement = "")
    if (length(mods) == 1) mods <- mods[[1]] # collapse in case of only 1 model
    return(mods)
  })
  names(taxon_models) <- taxNames
  # taxon_models will be nested list of lists if multiple formula strings given
  # transposing is required to make the ultimate model object names be taxa
  if (length(fstring_rhs) > 1) taxon_models <- purrr::transpose(taxon_models)

  if (isTRUE(return_psx)) {
    # attach models list to ps_extra
    data[["tax_models"]] <- list(x = taxon_models)
    names(data[["tax_models"]]) <- rank
    return(data)
  } else {
    return(taxon_models)
  }

}

# helper to actually model taxa
# takes:
# - ps: phyloseq at correct aggregation level
# - type: (name of) modelling function
# - taxon is name of taxon
# - fstring_rhs is model formula right hand side as string
# - verbose logical
# - any other args, passed from tax_model
taxonModel <- function(ps, type, taxon, fstring_rhs, ...) {
  stopifnot(rlang::is_string(fstring_rhs))

  # combine lhs and rhs formula
  fstring <- paste0("`", taxon, "`", fstring_rhs)

  # start argument list to be passed to modelling function
  args <- list(formula = stats::as.formula(fstring), ...)

  # set appropriate data arg for modelling type
  if (isType_bbdml(type)) {
    args[["data"]] <- ps
  } else {
    args[["data"]] <- samdatAsDataframe(ps_otu2samdat(ps = ps, taxa = taxon))
  }

  # additional setup for corncob
  if (isType_bbdml(type)) {
    type <- corncob::bbdml
    # phi.formula is for modelling dispersion (doesn't take a response var)
    if (identical(args[["phi.formula"]], NULL)) {
      args[["phi.formula"]] <- stats::as.formula(fstring_rhs)
    }
  }

  # actually do the modelling
  res <- do.call(type, args = args)
  # attach formula information as attribute to model object
  attr(res, which = "formula_string") <- fstring
  res[["call"]] <- "See formula_string attribute of model."
  return(res)
}

# simple helper to generate text for a message about number of NAs in a vector
checkNAs <- function(vec, name, fun = "warning") {
  if (fun != "allow" && anyNA(vec)) {
    txt <- paste(sum(is.na(vec)), "/", length(vec), "values are NA in", name)
    do.call(fun, list(txt))
  }
}

# check the variance or variation of a vector
checkVariance <- function(vec, name) {
  if (is.numeric(vec)) {
    if (stats::var(vec, na.rm = TRUE) %in% c(0, NA)) {
      stop("Variance of variable ", name, " is: ", stats::var(vec, na.rm = TRUE))
    }
  } else {
    uniq <- unique(vec[!is.na(vec)])
    if (length(uniq) == 1L) {
      stop(name, " has only one non-NA value: ", uniq)
    }
  }
}


# helper to safely check if corncob bbdml model is request
# first checking if corncob installed, before checking function equality
isType_bbdml <- function(type) {
  hasCorncob <- requireNamespace("corncob", quietly = TRUE)
  if (hasCorncob && identical(type, corncob::bbdml)) return(TRUE)
  if (identical(type, "bbdml")) {
    if (hasCorncob) return(TRUE)
    stop("you must install corncob package to use 'bbdml' models")
  }
  return(FALSE)
}

# helper to check if ps_cor_test requested in tax_model
isType_ps_cor_test <- function(type) {
  identical(type, "ps_cor_test") | identical(type, ps_cor_test)
}

# Internal helper function for tax_model:
# check actual phyloseq taxa_names match this level after aggregating as
# sometimes the names are not changed by tax_agg
# (i.e. when no aggregation occurs when rank = "unique")
taxEnsureNamesMatchRank <- function(ps, rank, verbose){
  taxaAtRank <- unname(unclass(phyloseq::tax_table(ps))[, rank, drop = TRUE])
  if (!isTRUE(all.equal(phyloseq::taxa_names(ps), taxaAtRank))) {
    not_matching <- phyloseq::taxa_names(ps) != taxaAtRank
    if (!isFALSE(verbose)) {
      message(
        "After aggregation at rank of ", rank, ", ", sum(not_matching),
        " taxa_names do not match their tax_table entries at ", rank, " level.",
        "\nThe tax_table entries will be used in the modelling results."
      )
      for (i in which(not_matching)) {
        message(
          "Renaming: ",  phyloseq::taxa_names(ps)[i], " -> ", taxaAtRank[i]
        )
      }
    }
    phyloseq::taxa_names(ps) <- taxaAtRank
  }
  return(ps)
}

# internal helper to tax_model
# get numeric index of taxa_names (taxa arg) in (aggregated) phyloseq (ps arg)
taxIndexGet <- function(ps, taxa) {
  if (any(is.na(taxa))) stop("taxa argument contains one or more NAs")

  # default behaviour is to use all taxa
  if (is.null(taxa)) return(seq_len(phyloseq::ntaxa(ps)))

  # convert logical or character selections to numeric indices
  if (is.logical(taxa)) return(which(taxa))
  if (is.character(taxa)) return(match(unique(taxa), phyloseq::taxa_names(ps)))

  # check bad numeric options
  if (identical(taxa, 0) || identical(taxa, 0L)) stop("taxa cannot be 0")
  if (!rlang::is_integerish(taxa)) stop("taxa cannot be non-integer numbers")
  if (is.numeric(taxa)) return(unique(taxa))
}


# make string representing rhs of formula from a formula or vector of variables
formulaMakerRHSstring <- function(formula = NULL,
                                  variables = NULL#,
                                  # univariable = FALSE
                                  ) {
  # if (isTRUE(univariable) && is.null(variables)) {
  #   stop("To fit one or more univariable models, use the variables argument")
  # }
  if (!xor(rlang::is_empty(formula), rlang::is_empty(variables))) {
    stop("Provide EITHER formula (rhs) OR character vector of variables!")
  }

  if (rlang::is_empty(formula)) {
    # if formula arg not supplied, use variables arg to create formula(e)
    if (!is.list(variables) && !is.character(variables)) {
      stop("variables must be NULL, character, or list")
    }
    if (is.list(variables)) {
      formula <- sapply(X = variables, USE.NAMES = FALSE, FUN = function(v) {
        if (!is.character(v)) stop("variables list contents must be character")
        if (anyNA(v)) stop("variables argument must not contain NAs")
        return(paste("~", paste(v, collapse = " + ")))
      })
    } else {
      if (anyNA(variables)) stop("variables argument must not contain NAs")
      formula <- paste("~", paste(variables, collapse = " + "))
    }
  }

  # convert formula(e) to formula, check and convert (back) to string(s)
  if (!is.list(formula) && !rlang::is_formula(formula) && !is.character(formula)) {
    stop("formula must be NULL or of class formula, character, or list")
  }
  if (is.list(formula) || is.character(formula)) {
    fstring <- sapply(X = formula, USE.NAMES = FALSE, FUN = function(f) {
      if (!inherits(f, "formula") && !is.character(f)) {
        stop("formula arg list must contain only formulae or formulae strings")
      }
      f <- stats::as.formula(f)
      checkFormulaOneSided(f)
      return(formula2string(f))
    })
  } else {
    checkFormulaOneSided(formula)
    fstring <- formula2string(formula)
  }

  return(fstring)
}

checkFormulaOneSided <- function(f) {
  # test idea from stats::asOneSidedFormula
  if (length(f) != 2L) {
    stop("formula must include only right-hand side, e.g '~ a + b'")
  }
}

formula2string <- function(f) {
  # ref: https://stackoverflow.com/a/14671300/9005116
  paste(deparse(f, width.cutoff = 500), collapse = "")
}

# check if package(s) is/are available, and stop if not, explaining why needed
checkPackageAvailable <- function(
    packages = c("package", "names"),
    message = " package is required to use this functionality."
) {
  for (p in packages) {
    if (!requireNamespace(p, quietly = TRUE)) stop(p, message)
  }
}
