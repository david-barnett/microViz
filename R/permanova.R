#' Calculate PERMANOVA after calc_dist
#'
#' Wrapper for vegan adonis/adonis2, takes the output of calc_dist (a list containing a phyloseq object and calculated distance matrix)
#'
#' Drops observations with missing values if complete_cases is TRUE, otherwises throws an error.
#'
#' @param data list output from calc_dist
#' @param variables character vector of variables to include in model
#' @param interactions interactions between variables, written in the style of e.g. "var_a * var_b"
#' @param n_processes how many parallel processes to use? (uses parallel::makePSOCKcluster)
#' @param n_perms how many permutations?
#' @param seed set a random number generator seed to ensure you get the same results each run
#' @param adonis2 use adonis2 (with by = "margin") instead of adonis (better if you have interactions)
#' @param complete_cases if TRUE, drops observations if they contain missing values
#' @param verbose sends messages about progress if true
#' @param return what parts of return list to return, defaults to all parts
#'
#' @return list containing permanova results and input objects
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(vegan)
#' data("dietswap", package = "microbiome")
#'
#' # add some missings to demonstrate automated removal
#' sample_data(dietswap)$sex[3:6] <- NA
#'
#' # compute distance
#' testDist <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   calc_dist("bray")
#'
#' PERM <- testDist %>%
#'   permanova(
#'     seed = 1,
#'     variables = c("sex", "bmi_group"),
#'     n_processes = 1, n_perms = 99
#'   )
#' str(PERM, max.level = 1)
#'
#' PERM2 <- testDist %>%
#'   permanova(
#'     seed = 1,
#'     variables = c("sex", "bmi_group"),
#'     interactions = "sex * bmi_group",
#'     n_processes = 1, n_perms = 99,
#'     adonis2 = TRUE
#'   )
#' PERM2$permanova
#'
#' # take the same distance matrix used for the permanova and plot an ordination
#' PERM2 %>%
#'   ordin8("PCoA") %>%
#'   plot_ordin8(color = "bmi_group")
#' # this ensures any samples dropped from the permanova for having missing values
#' # in the covariates are NOT included in the corresponding ordination plot
permanova <- function(data,
                      variables,
                      interactions = NULL,
                      complete_cases = TRUE,
                      n_processes = 1,
                      n_perms = 999,
                      seed = NULL,
                      adonis2 = FALSE,
                      verbose = TRUE,
                      return = "all") {

  # check input data object class
  if (inherits(data, "list")) {
    ps <- data[["ps"]]
    distMat <- data[["distMat"]]
    info <- data[["info"]]
  } else {
    stop("data argument must be an output object from calc_dist")
  }

  # set seed for reproducibility
  if (isFALSE(is.null(seed))){
    set.seed(seed)
  }

  # Build the formula
  f <- paste0("distMat ~ ", paste(variables, collapse = " + "))
  if (!rlang::is_null(interactions)) {
    f <- paste0(f, " + ", paste(interactions, collapse = " + "))
  }
  FORMULA <- stats::as.formula(f)

  # drop observations with missings
  for (v in variables) {
    vec <- phyloseq::sample_data(ps)[[v]]
    NAs <- is.na(vec)
    s <- sum(NAs)
    if (s > 0) {
      if (complete_cases) {
        if (verbose) {
          message('WARNING: Dropping samples with NAs for "', v, '". At least ', s)
        }
        ps <- phyloseq::prune_samples(samples = !NAs, x = ps)
        if (exists("distMat") && !rlang::is_null(distMat)) {
          distMat <- stats::as.dist(as.matrix(distMat)[!NAs, !NAs])
        }
      } else {
        stop(v, " contains missings, at least: ", s, "\n\tTry `drop_incomplete()`")
      }
    }
  }

  # extract sample metadata from phyloseq object
  metadata <- microbiome::meta(ps)[, variables, drop = FALSE]

  if (verbose) {
    message(Sys.time(), " - Starting PERMANOVA with ", n_perms, " perms with ", n_processes, " processes")
  }
  # perform PERMANOVA, in parallel (socket cluster)
  if (n_processes > 1) {
    cl <- parallel::makePSOCKcluster(n_processes)
    on.exit(parallel::stopCluster(cl))
  } else {
    cl <- 1
  }

  if (isTRUE(adonis2)) {
    results <- vegan::adonis2(formula = FORMULA, data = metadata, permutations = n_perms, parallel = cl, by = "margin")
  } else {
    results <- vegan::adonis(formula = FORMULA, data = metadata, permutations = n_perms, parallel = cl)
  }
  if (verbose) {
    message(Sys.time(), " - Finished PERMANOVA")
  }

  # return object (results and processing info)
  out <- list(
    info = info, permanova = results, distMat = distMat, ps = ps
  )

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
