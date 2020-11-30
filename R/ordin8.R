#' Ordinate phyloseq object or distance matrix computed by calc_dist
#'
#' Extends functionality of phyloseq::ordinate(). Results can be used directly in plot_ordin8.
#'
#' @param data list object output from calc_dist or tax_agg (if no distance calculation required e.g. for RDA)
#' @param method which ordination method to use? currently one of 'PCoA', 'PCA' or 'CCA'
#' @param constraints (a vector of) valid variable name(s) to constrain PCoA or RDA analyses, or leave as 1 for unconstrained ordination
#' @param conditions (a vector of) valid variable name(s) to partial these out of PCoA or RDA analyses with Condition(), or leave as NULL
#' @param return choose which parts of list object to return
#' @param ... optional arguments passed on to phyloseq::ordinate()
#'
#' @return list object (or named parts)
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(vegan)
#' data("dietswap", package = "microbiome")
#'
#' # create a numerical variable to use as constraint
#' sample_data(dietswap)$female <-
#'   dplyr::if_else(sample_data(dietswap)$sex == "female", true = 1, false = 0)
#'
#' # add a couple of missing values to demo automated dropping of observations with missings
#' sample_data(dietswap)$female[c(3, 4)] <- NA
#'
#' # create another numeric variable
#' sample_data(dietswap)$weight <-
#'   dplyr::recode(sample_data(dietswap)$bmi_group, obese = 3, overweight = 2, lean = 1)
#'
#' # compute ordination
#' test <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   calc_dist("bray") %>%
#'   ordin8(constraints = c("weight", "female"))
#' # familiarise yourself with the structure of the returned list object
#' str(test, max.level = 1)
#'
#' # compute RDA ("aitchison distance") directly from phyloseq (and demo return argument)
#' test2 <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ordin8(method = "RDA", constraints = c("weight", "female"), return = "ordination")
#' # plot with oldschool vegan graphics to show it returns a standard interoperable ordination object
#' ordiplot(test2)
ordin8 <- function(data,
                   method = c("PCoA", "PCA", "CCA", "RDA", "CAP")[1],
                   constraints = 1,
                   conditions = NULL,
                   return = "all",
                   ...) {

  # check input data object class
  if (inherits(data, "list")) {
    ps <- data[["ps"]]
    distMat <- data[["distMat"]]
    info <- data[["info"]]
  } else if (inherits(data, "phyloseq")) {
    ps <- data
    distMat <- NULL
    info <- list(tax_level = "not specified", distName = "not specified")
  } else {
    stop("data is wrong class, should be list output of calc_dist or tax_agg, or a phyloseq")
  }

  # constraint and condition handling in phyloseq object and distance matrix
  # drop missings and scale explanatory variables (constraints) if given (for RDA or dbRDA/constrained PCoA)
  if (!identical(constraints, 1) || !rlang::is_null(conditions)) {
    message("\nCentering (mean) and scaling (sd) the constraint and conditioning vars: ")
    # remove '1' in case conditions is set but constraints is still 1 (default)
    VARS <- setdiff(c(constraints, conditions), 1)
    for (v in VARS) {
      message("\t", v)
      vec <- phyloseq::sample_data(ps)[[v]]
      if (!class(vec) %in% c("logical", "numeric", "integer")) {
        stop(paste0("Constraints and conditions must be numeric, logical, or integer: ", v, " is ", class(vec)))
      }
      NAs <- is.na(vec)
      s <- sum(NAs)
      if (s > 0) {
        message('\tWARNING: Dropping samples with NAs for "', v, '". N = ', s)
        ps <- phyloseq::prune_samples(samples = !NAs, x = ps)
        if (!rlang::is_null(distMat)) {
          distMat <- stats::as.dist(as.matrix(distMat)[!NAs, !NAs])
        }
      }
      phyloseq::sample_data(ps)[, v] <- scale(vec[!is.na(vec)], center = TRUE, scale = TRUE)
    }
  }

  # deal with synonyms / equivalent methods (set to name that calls preferred underlying function)
  if (method == "PCA") {
    method <- "RDA" # synonymous for this purpose (PCA is unconstrained RDA)
  }
  if (method %in% c("PCoA", "MDS")) {
    # CAP causes phyloseq::ordinate to call vegan::capscale which, with formula DIST ~ 1, is PCoA (and gives scores!)
    method <- "CAP"
  }

  # PCoA/CAPscale or RDA/PCA or CCA
  if (method %in% c("RDA", "CAP", "CCA")) {
    if (method != "RDA" && rlang::is_null(distMat)) {
      stop("Use calc_dist before using ordin8 with this method: ", method)
    }

    # set formula to include any given constraints on RHS (by default the RHS = 1)
    f <- paste0("distMat ~ ", paste(constraints, collapse = " + "))

    # add any conditions specified in conditions arg
    if (!rlang::is_null(conditions)) {
      condVars <- paste(conditions, collapse = " + ")
      f <- paste0(f, " + Condition(", condVars, ")")
    }

    Formula <- stats::as.formula(f)

    # note RDA does not use distance arg (and distMat is not computed)
    ORD <- phyloseq::ordinate(physeq = ps, method = method, distance = distMat, formula = Formula, ...)
  } else if (method %in% c("DCA", "DPCoA", "NMDS") && identical(constraints, 1)) {
    # other valid unconstrained phyloseq methods
    ORD <- phyloseq::ordinate(physeq = ps, method = method, distance = distMat, ...)
  }

  # return list output
  info[["method"]] <- method
  info[["constraints"]] <- constraints
  info[["conditions"]] <- conditions

  out <- list(
    info = info, ordination = ORD,
    distMat = distMat, ps = ps
  )

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
