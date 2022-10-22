#' Ordinate samples (arrange by similarity in multiple dimensions)
#'
#' @description
#' Used before plotting with ord_plot() or explorating interactively with ord_explore().
#' Use method = "auto" to automatically pick an appropriate method from:
#'  - "PCA" (Principal Components Analysis) combines taxa abundances into new dimensions. The first axes display the greatest variation in your microbial data.
#'  - "RDA" (Redundancy Analysis) is constrained PCA, roughly speaking. It finds variation in your data that can be explained both by the constraints variables, and the microbial data.
#'  - "PCoA" (Principal Coordinates Analysis) finds a coordinate system that best preserves the original distances between samples.
#'  - "CAP" (Constrained Analysis of Principal Coordinates) is also known as distance-based Redundancy Analysis.
#'
#' Alternatively to leaving method = "auto", you can explicitly specify any of the above methods, or choose one of the following:
#'  - "CCA" (Canonical Correspondence Analysis) - NOT canonical correlation analysis!
#'  - "NMDS" (Non-metric Multidimensional Scaling)
#'
#' You are strongly recommended to check out this useful website for introductory explanations of these methods
#' the "GUide to STatistical Analysis in Microbial Ecology": \url{https://sites.google.com/site/mb3gustame/}
#'
#' @details
#' Extends functionality of phyloseq::ordinate().
#' Results can be used directly in ord_plot().
#' You can extract the ordination object for other analyses with ord_get()
#'
#'
#' @seealso \code{\link{dist_calc}} for distance matrix calculation
#' @seealso \code{\link{ord_plot}} and \code{\link{ord_explore}}
#' @seealso \code{phyloseq \link[phyloseq]{ordinate}}
#'
#'
#' @param data ps_extra list object: output from dist_calc(), or tax_transform() if no distance calculation required for method e.g. for RDA
#' @param method which ordination method to use? "auto" means automatically determined from ps_extra and other args.
#' If you really know what you want: manually set one of 'PCoA', 'PCA', 'CCA', 'CAP' or 'RDA'
#' @param constraints (a vector of) valid sample_data name(s) to constrain analyses, or leave as NULL for unconstrained ordination.
#' Non-NULL values are compatible with method = "auto"/"RDA"/"CAP"
#' @param conditions (a vector of) valid sample_data name(s) to partial these out of analyses with Condition(), or leave as NULL
#' @param scale_cc
#' If TRUE (default) ensures any constraints and conditions variables are scaled before use, to ensure their effects are comparable.
#' If set to FALSE you must ensure you have already set the variables on a similar scale yourself!
#' If there are no constraints or conditions, scale_cc does nothing.
#' @param verbose
#' If TRUE or "max", show any warnings and messages about constraint and conditions scaling and missings etc.
#' FALSE suppresses warnings!
#' @param ... optional arguments passed on to phyloseq::ordinate()
#'
#' @return ps_extra list object
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
#' # familiarise yourself with the structure of the returned ps_extra list object
#' test
#' str(test, max.level = 1)
#'
#' # compute RDA with centre-log-ratio transformed taxa
#' test2 <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(constraints = c("weight", "female"))
#' # plot with vegan package graphics to show it returns a standard ordination object
#' ord_get(test2) %>% vegan::ordiplot()
#' ord_plot(test2, plot_taxa = 8:1)
#' # This is equivalent to CAP with "aitchison" distance
#' # but the latter (below) doesn't allow plotting taxa loadings with ord_plot
#' dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("aitchison") %>%
#'   ord_calc(constraints = c("weight", "female")) %>%
#'   ord_plot()
ord_calc <- function(data,
                     method = "auto",
                     constraints = NULL,
                     conditions = NULL,
                     scale_cc = TRUE,
                     verbose = TRUE,
                     ...) {
  if (!rlang::is_scalar_logical(scale_cc)) stop("scale_cc must be logical")
  # identify if requested ordination is constrained or conditioned
  if (identical(constraints, NULL)) constraints <- 1
  isConstrained <- ordCheckConstraints(constraints)
  isConditioned <- ordCheckConditions(conditions)

  # check class of data is ps_extra and if phyloseq, convert to psExtra
  if (!is_ps_extra(data)) check_is_phyloseq(data, argName = "data")
  if (!is(data, "psExtra")) {
    warning(
      "* data provided to ord_calc is a phyloseq object, not a psExtra.\n",
      "* Consider using tax_agg and/or tax_transform before ordinating."
    )
    data <- psExtra(ps = data, info = new_psExtraInfo())
  }

  # get data elements
  ps <- ps_get(data)
  distMat <- dist_get(data)
  info <- info_get(data)

  if (!identical(ord_get(data), NULL) && !isFALSE(verbose)) {
    warning(
      "You already calculated an ordination (with ",
      info$ord_info$method, ") --> overwriting that ordination"
    )
  }

  # check method is valid option, and guess appropriate method if "auto"
  method <- ordCheckMethod(
    method = method, con = isConstrained, distMat = distMat, verbose = verbose
  )
  # check ordination method compatible with distance availability
  distMat <- ordCheckDist(distMat, method = method, verbose = verbose)

  # constraint and condition handling in phyloseq object and distance matrix
  if (isConstrained || isConditioned) {

    # handle missings and scale conditions and constraints (for RDA etc.)
    ps <- ps_conScale(
      ps = ps, constraints = constraints, conditions = conditions,
      verbose = verbose, scale_cc = scale_cc
    )

    # drop samples from distMat if no longer in ps
    if (!identical(distMat, NULL)) {
      keepers <- phyloseq::sample_names(ps)
      distMat <- stats::as.dist(as.matrix(distMat)[keepers, keepers])
    }
  }

  # set ordMethod (dealing with synonyms / equivalent unconstrained methods)
  # `method` is used to annotate plot,
  # `ordMethod` is used to specify how to actually do the ordination
  ordMethod <- method

  # PCA is unconstrained RDA (invalid PCA+constraints combo checked earlier)
  if (identical(method, "PCA")) ordMethod <- "RDA"

  # CAP causes ordinate to call capscale which, with formula DIST ~ 1, is PCoA
  if (method %in% c("PCoA", "MDS")) ordMethod <- "CAP"

  # standard and possibly constrained methods
  if (ordMethod %in% c("RDA", "CAP", "CCA")) {

    # set formula to include any given constraints on RHS
    # (if unconstrained, the constraints = 1, and so RHS is = 1)
    f <- paste0("~ ", paste(constraints, collapse = " + "))
    # phyloseq::ordinate only requires RHS of formula, as it constructs the
    # LHS from physeq or distance arg (whichever relevant to given method)

    # add any conditions specified in conditions arg
    if (isConditioned) {
      condVars <- paste(conditions, collapse = " + ")
      f <- paste0(f, " + Condition(", condVars, ")")
    }

    FORM <- stats::as.formula(f)

    ORD <- phyloseq::ordinate(
      physeq = ps, method = ordMethod, distance = distMat, formula = FORM, ...
    )
  }

  # other valid unconstrained phyloseq methods
  if (method %in% c("DCA", "DPCoA", "NMDS") && !isConstrained) {
    ORD <- phyloseq::ordinate(
      physeq = ps, method = ordMethod, distance = distMat, ...
    )
  }

  # build return object

  ordI <- new_psExtraOrdInfo(method = method)
  if (isConstrained) ordI[["constraints"]] <- paste(constraints, collapse = "+")
  if (isConditioned) ordI[["conditions"]] <- paste(conditions, collapse = "+")

  info[["ord_info"]] <- ordI
  data <- psExtra(
    ps = ps, info = info, counts = data@counts, dist = distMat, ord = ORD
  )
  return(data)
}

#' Scale and center constraint and condition vars in phyloseq sample data
#'
#' Internal helper function for ord_calc
#'
#' @param ps phyloseq object with sample data
#' @param constraints vector of constraint variables
#' @param conditions vector of conditions variables
#' @param verbose logical, send messages?
#' @param scale_cc logical, scale and center constraints/conditions?
#'
#' @return phyloseq object
#' @noRd
ps_conScale <- function(ps, constraints, conditions, verbose, scale_cc) {
  # remove '1' in case conditions is set but constraints is still 1 (default)
  VARS <- setdiff(c(constraints, conditions), 1)
  # drop samples with missings
  ps <- ps_drop_incomplete(ps, vars = VARS, verbose = verbose)

  if (!isFALSE(verbose) && isTRUE(scale_cc)) {
    message(
      "\nCentering (mean) and scaling (sd) the constraints and/or conditions: "
    )
  }
  for (v in VARS) {
    if (!isFALSE(verbose) && isTRUE(scale_cc)) message("\t", v)
    vec <- phyloseq::sample_data(ps)[[v]]
    if (!class(vec) %in% c("logical", "numeric", "integer")) {
      rlang::abort(call = rlang::caller_env(1), message = c(
        "Constraints and conditions must be numeric, logical, or integer",
        i = paste0(v, " is ", paste(class(vec), collapse = " "))
      ))
    }
    if (isTRUE(scale_cc)) {
      phyloseq::sample_data(ps)[, v] <- as.numeric(
        scale(vec[!is.na(vec)], center = TRUE, scale = TRUE)
      )
    }
  }
  return(ps)
}

# check if constraints given are valid (or 1, meaning not constrained)
# and return logical indicating if constrained
ordCheckConstraints <- function(constraints) {
  isConstrained <- !identical(constraints, 1)
  if (isConstrained && !inherits(constraints, "character")) {
    stop("constraints must be a character vector, or NULL")
  }
  return(isConstrained)
}

# check if conditions given are valid (or NULL, meaning not conditioned)
# and return logical indicating if conditioned
ordCheckConditions <- function(conditions) {
  isConditioned <- !identical(conditions, NULL)
  if (isConditioned && !inherits(conditions, "character")) {
    stop("conditions must be a character vector, or NULL")
  }
  return(isConditioned)
}

#' Check compatibility of distance matrix and ordination method
#'
#' Internal helper for ord_calc.
#' Returns NULL if distance matrix not needed for chosen method.
#'
#' @param distMat distance matrix or NULL if none
#' @param method ordination method name
#' @param verbose logical, send messages?
#'
#' @return distance matrix as provided, or NULL
#' @noRd
ordCheckDist <- function(distMat, method, verbose) {
  if (!identical(distMat, NULL) && method %in% c("PCA", "RDA", "CCA")) {
    distMat <- NULL # so output psExtra will get NULL distance matrix
    if (!isFALSE(verbose)) {
      rlang::warn(message = c(
        "!" = paste("Distance matrix is not used for", method),
        "!" = "Ignoring distance matrix and removing it from ps_extra output",
        i = "Did you mean to use PCoA or CAP? (or try method = 'auto')"
      ))
    }
  }
  if (identical(distMat, NULL) && method %in% c("CAP", "PCoA", "MDS", "NMDS")) {
    rlang::abort(call = rlang::caller_env(1), message = c(
      "Distance matrix missing!",
      i = paste("Use dist_calc() before using ord_calc() with method", method)
    ))
  }
  return(distMat)
}

#' method check/guess helper for ord_calc
#'
#' check method is valid and guess method from constraints/distance if "auto"
#'
#' @param method method name
#' @param con logical, is constrained?
#' @param distMat distance matrix or NULL if none
#' @param verbose logical, send messages?
#'
#' @return method string
#' @noRd
ordCheckMethod <- function(method, con, distMat, verbose) {
  rlang::arg_match(arg = method, error_call = rlang::caller_env(1), values = c(
    "auto", "PCoA", "PCA", "CCA", "RDA", "CAP", "NMDS", "DCA", "DPCoA"
  ))

  # set method automatically if auto given
  if (identical(method, "auto")) {
    if (identical(distMat, NULL)) {
      method <- if (!con) "PCA" else "RDA"
    } else {
      method <- if (!con) "PCoA" else "CAP"
    }
  }

  # check if untested method
  if (!isFALSE(verbose) && method %in% c("DCA", "DPCoA")) {
    w <- "Neither DCA nor DPCoA have been tested in microViz, so may not work as expected..."
    rlang::warn(w)
  }
  # fail if unconstrained method provided with constraints
  if (con && !method %in% c("RDA", "CAP", "CCA")) {
    rlang::abort(call = rlang::caller_env(1), message = c(
      paste(method, "cannot use constraints"),
      i = "Did you mean RDA, CAP or CCA?",
      i = "Alternatively use method = 'auto' (the default)"
    ))
  }
  return(method)
}
