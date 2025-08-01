#' Calculate PERMANOVA after dist_calc()
#'
#' @description
#' `dist_permanova` runs a Permutational Multivariate ANOVA (aka Non-parametric MANOVA).
#' This is a way to test for the statistical significance of (independent)
#' associations between variables in your phyloseq::sample_data(),
#' and a microbiota distance matrix you have already calculated with dist_calc().
#'
#' This function is a wrapper around vegan's `adonis2()` function. See `?vegan::adonis2()` for more insight.
#'
#' You can also read this excellent book chapter on PERMANOVA by Marti Anderson:
#' \doi{10.1002/9781118445112.stat07841}
#'
#' Or this NPMANOVA page on GUSTA ME:
#' \url{https://sites.google.com/site/mb3gustame/hypothesis-tests/manova/npmanova}
#'
#' @details
#' The variables argument will be collapsed into one string (if length > 1) by pasting together, separated by "+".
#' Any interaction terms described in the interactions argument will be pasted onto the end of the pasted variables argument.
#' Alternatively, you can supply the complete right hand side of the formula yourself e.g variables = "varA + varB + varC\*varD"
#'
#' Watch out, if any of your variable names contain characters that would normally separate variables in a formula then
#' you should rename the offending variable (e.g. avoid any of "+" "\*" "|" or ":" ) otherwise permanova will split that variable into pieces.
#'
#' @param data psExtra output from dist_calc()
#' @param variables character vector of variables to include in model or character representation of the right-hand side of a formula, e.g "varA + varB + varA:varB"
#' @param interactions optional argument to define any interactions between variables, written in the style of e.g. "var_a * var_b"
#' @param n_processes how many parallel processes to use? (on windows this uses parallel::makePSOCKcluster())
#' @param n_perms how many permutations? e.g. 9999. Less is faster but more is better!
#' @param seed set a random number generator seed to ensure you get the same results each run
#' @param complete_cases if TRUE, drops observations if they contain missing values (otherwise stops if missings are detected)
#' @param by passed to vegan::adonis2() `by` argument: what type of sums of squares to calculate? "margin" or "terms"
#' @param verbose sends messages about progress if TRUE
#' @param ... additional arguments are passed directly to vegan::adonis2() (e.g. strata, add, sqrt.dist etc.)
#'
#' @return psExtra list containing permanova results and (filtered) input objects
#' @export
#'
#' @seealso \code{\link{dist_calc}} for calculating the required distance matrix input
#' @seealso \code{\link{ord_plot}} with constraints as a way to visualise the microbial associations of significant predictors
#' @seealso \code{vegan::\link[vegan]{adonis2}}
#'
#' @examples
#' data("dietswap", package = "microbiome")
#'
#' # add some missings to demonstrate automated removal
#' phyloseq::sample_data(dietswap)$sex[3:6] <- NA
#'
#' # compute distance
#' testDist <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("identity") %>%
#'   dist_calc("bray")
#'
#' PERM <- testDist %>%
#'   dist_permanova(
#'     seed = 1,
#'     variables = c("sex", "bmi_group"),
#'     n_processes = 1,
#'     n_perms = 99 # only 99 perms used in examples for speed (use 9999+!)
#'   )
#' PERM
#' str(PERM, max.level = 1)
#'
#' # try permanova with interaction terms
#' PERM2 <- testDist %>%
#'   dist_permanova(
#'     seed = 1,
#'     variables = "nationality + sex * bmi_group",
#'     n_processes = 1, n_perms = 99
#'   )
#' perm_get(PERM2)
#'
#' # specify the same model in alternative way
#' PERM3 <- testDist %>%
#'   dist_permanova(
#'     seed = 1,
#'     variables = c("nationality", "sex", "bmi_group"),
#'     interactions = "sex * bmi_group",
#'     n_processes = 1, n_perms = 99
#'   )
#' perm_get(PERM3)
#'
#' identical(PERM3, PERM2) # TRUE
#'
#' # take same distance matrix used for the permanova and plot an ordination
#' PERM2 %>%
#'   ord_calc(method = "PCoA") %>%
#'   ord_plot(color = "bmi_group")
#' # this trick ensures any samples dropped from the permanova
#' # for having missing values in the covariates are NOT included
#' # in the corresponding ordination plot
dist_permanova <- function(data,
                           variables = NULL,
                           interactions = NULL,
                           complete_cases = TRUE,
                           n_processes = 1,
                           n_perms = 999,
                           seed = NULL,
                           by = "margin",
                           verbose = TRUE,
                           ...) {
  # check input data object class
  check_is_psExtra(data, argName = "data")
  ps <- ps_get(data)
  distMat <- dist_get(data)

  # Build the formula if supplied as a string
  formula <- paste0("~ ", paste(variables, collapse = " + "))
  if (!identical(interactions, NULL)) {
    formula <- paste0(formula, " + ", paste(interactions, collapse = " + "))
  }
  formula <- paste("distMat", formula)
  # split variables in case they were provided in any part as formula
  split_vars <- strsplit(x = variables, split = "[+*|:]", perl = TRUE)
  split_vars <- unlist(split_vars)
  # remove trailing and leading whitespaces
  split_vars <- unique(trimws(split_vars))

  if (any(!split_vars %in% phyloseq::sample_variables(ps))) {
    stop(
      "Some variables are not found in phyloseq sample data:\n",
      paste(
        split_vars[!split_vars %in% phyloseq::sample_variables(ps)],
        collapse = "\n"
      )
    )
  }
  # drop observations with missings
  if (isFALSE(complete_cases)) {
    if (anyNA(phyloseq::sample_data(ps)[, split_vars])) {
      stop(
        "phyloseq has missings in at least one of the specified variables",
        "\n\tTry complete_cases = TRUE or manually call `ps_drop_incomplete()`"
      )
    }
  }
  ps <- ps_drop_incomplete(ps, vars = split_vars, verbose = verbose)

  # drop samples from pre-existing distMat
  # if no longer in ps after dropping incomplete
  keepers <- phyloseq::sample_names(ps)
  distMat <- stats::as.dist(as.matrix(distMat)[keepers, keepers])
  # drop samples from any existing count matrix
  if (!is.null(data@counts)) data@counts <- data@counts[keepers, , drop = FALSE]

  # extract sample metadata from phyloseq object
  metadata <- samdatAsDataframe(ps)[, split_vars, drop = FALSE]

  # set seed for reproducibility
  if (!identical(seed, NULL)) set.seed(seed)

  if (!isFALSE(verbose)) {
    message(
      Sys.time(), " - Starting PERMANOVA with ", n_perms,
      " perms with ", n_processes, " processes"
    )
  }
  # on windows: set up perform PERMANOVA in parallel with socket cluster
  if (n_processes > 1 && grepl("Windows", utils::osVersion)) {
    parall <- parallel::makePSOCKcluster(n_processes)
    if (!isFALSE(verbose)) {
      message(
        Sys.time(), " - Started PSOCK cluster with with ",
        n_processes, " workers"
      )
      on.exit(parallel::stopCluster(parall))
    }
  } else {
    parall <- n_processes
  }
  # perform permanova
  formula <- stats::as.formula(formula)
  results <- vegan::adonis2(
    formula = formula, data = metadata, permutations = n_perms,
    parallel = parall, by = by, ...
  )
  if (!isFALSE(verbose)) message(Sys.time(), " - Finished PERMANOVA")

  data <- psExtra(
    ps = ps, info = info_get(data), counts = data@counts,
    dist = distMat, # might have been filtered
    permanova = results
  )
  return(data)
}
