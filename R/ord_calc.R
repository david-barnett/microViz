#' Ordinate phyloseq object or distance matrix computed by dist_calc
#'
#' Extends functionality of phyloseq::ordinate(). Results can be used directly in ord_plot.
#'
#' @param data list object output from dist_calc or tax_agg (if no distance calculation required e.g. for RDA)
#' @param method which ordination method to use? currently one of 'PCoA', 'PCA' or 'CCA'
#' @param constraints (a vector of) valid variable name(s) to constrain PCoA or RDA analyses, or leave as 1 for unconstrained ordination
#' @param conditions (a vector of) valid variable name(s) to partial these out of PCoA or RDA analyses with Condition(), or leave as NULL
#' @param return choose which parts of list object to return
#' @param ... optional arguments passed on to phyloseq::ordinate()
#' @param verbose if TRUE or "max", message about scaling and missings etc.
#'
#' @return list object (or named parts)
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(vegan)
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use as constraints
#' dietswap <- ps_mutate(
#'   dietswap,
#'   female = dplyr::if_else(sex == "female", true = 1, false = 0),
#'   weight = dplyr::recode(bmi_group, obese = 3, overweight = 2, lean = 1)
#' )
#'
#' # add a couple of missing values to demo automated dropping of observations with missings
#' sample_data(dietswap)$female[c(3, 4)] <- NA
#'
#' # compute ordination
#' test <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("bray") %>%
#'   ord_calc(constraints = c("weight", "female"))
#' # familiarise yourself with the structure of the returned list object
#' str(test, max.level = 1)
#'
#' # compute RDA ("aitchison distance") directly from phyloseq (and demo return argument)
#' test2 <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(method = "RDA", constraints = c("weight", "female"), return = "ordination")
#' # plot with oldschool vegan graphics to show it returns a standard interoperable ordination object
#' ordiplot(test2)
ord_calc <- function(data,
                     method = c("PCoA", "PCA", "CCA", "RDA", "CAP")[1],
                     constraints = 1,
                     conditions = NULL,
                     return = "all",
                     verbose = TRUE,
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
    stop("data is wrong class, should be list output of dist_calc or tax_agg, or a phyloseq")
  }

  # constraint and condition handling in phyloseq object and distance matrix
  # handle missings and scale explanatory variables (constraints) if given (for RDA or dbRDA/constrained PCoA)
  if (!identical(constraints, 1) || !identical(NULL, conditions)) {
    # remove '1' in case conditions is set but constraints is still 1 (default)
    VARS <- setdiff(c(constraints, conditions), 1)
    # drop samples with missings
    ps <- ps_drop_incomplete(ps, vars = VARS, verbose = verbose)
    # drop samples from any pre-existing distMat if no longer in ps
    if (exists("distMat") && !identical(distMat, NULL)) {
      keepers <- phyloseq::sample_names(ps)
      distMat <- stats::as.dist(as.matrix(distMat)[keepers, keepers])
    }
    if (!isFALSE(verbose)) message("\nCentering (mean) and scaling (sd) the constraint and conditioning vars: ")
    for (v in VARS) {
      if (!isFALSE(verbose)) message("\t", v)
      vec <- phyloseq::sample_data(ps)[[v]]
      if (!class(vec) %in% c("logical", "numeric", "integer")) {
        stop(paste0("Constraints and conditions must be numeric, logical, or integer: ", v, " is ", class(vec)))
      }
      phyloseq::sample_data(ps)[, v] <- scale(vec[!is.na(vec)], center = TRUE, scale = TRUE)
    }
  }

  # deal with synonyms / equivalent methods (set to name that calls preferred underlying function)
  # synonymous for this purpose (PCA is unconstrained RDA)
  if (identical(method, "PCA")) method <- "RDA"
  # CAP causes phyloseq::ordinate to call vegan::capscale which, with formula DIST ~ 1, is PCoA (and gives scores!)
  if (method %in% c("PCoA", "MDS")) method <- "CAP"

  # PCoA/CAPscale or RDA/PCA or CCA
  if (method %in% c("RDA", "CAP", "CCA")) {
    if (identical(method, "CAP") && identical(NULL, distMat)) {
      stop("Use dist_calc before using ord_calc with this method: ", method)
    }

    # set formula to include any given constraints on RHS (by default the RHS = 1)
    f <- paste0("distMat ~ ", paste(constraints, collapse = " + "))

    # add any conditions specified in conditions arg
    if (!identical(conditions, NULL)) {
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

  if (identical(return, "all")) {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
}
