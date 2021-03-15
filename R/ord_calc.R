#' Ordinate samples (arrange by similarity in multiple dimensions)
#'
#' @description
#' Used before plotting with ord_plot() or explorating interactively with ord_explore().
#' Use method = "auto" to automatically pick an appropriate method from:
#'  - "PCA" (Principle Components Analysis) combines taxa abundances into new dimensions. The first axes display the greatest variation in your microbial data.
#'  - "RDA" (Redundancy Analysis) is constrained PCA, roughly speaking. It finds variation in your data that can be explained both by the constraints variables, and the microbial data.
#'  - "PCoA" (Principle Coordinates Analysis) finds a coordinate system that best preserves the original distances between samples.
#'  - "CAP" (Constrained Analysis of Principle Coordinates) is also known as distance-based Redundancy Analysis.
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
                     method = c(
                       "auto", "PCoA", "PCA", "CCA", "RDA", "CAP", "NMDS"
                     )[1],
                     constraints = NULL,
                     conditions = NULL,
                     scale_cc = TRUE,
                     verbose = TRUE,
                     ...) {
  stopifnot(method %in% c(
    "auto", "PCoA", "PCA", "CCA", "RDA", "CAP", "NMDS", "MDS", "DCA", "DPCoA"
  ))
  stopifnot(inherits(scale_cc, "logical"))
  if (!isFALSE(verbose) && method %in% c("DCA", "DPCoA")) {
    warning(
      "Neither DCA nor DPCoA have been tested, so may not work as expected..."
    )
  }

  # identify if constrained or conditioned
  if (identical(constraints, NULL) || identical(constraints, 1)) {
    constraints <- 1
    constrained <- FALSE
  } else {
    if (!inherits(constraints, "character")) {
      stop("constraints must be NULL or a character vector")
    }
    constrained <- TRUE
  }
  conditioned <- !identical(conditions, NULL)
  if (conditioned && !inherits(conditions, "character")) {
    stop("conditions must be NULL or a character vector")
  }

  # if input is a phyloseq, convert to ps_extra
  if (inherits(data, "phyloseq")) data <- new_ps_extra(ps = data)
  # check class of data is (or at least now is) ps_extra
  if (!inherits(data, "ps_extra")) {
    stop(
      "data should be ps_extra list output of dist_calc or tax_transform, ",
      "or a phyloseq\n", "data is class: ", class(data)
    )
  }

  # get data elements
  ps <- ps_get(data)
  distMat <- dist_get(data)
  info <- info_get(data)

  # set method automatically if auto given
  if (identical(method, "auto")) {
    if (identical(distMat, NULL)) {
      method <- if (!constrained) "PCA" else "RDA"
    } else {
      method <- if (!constrained) "PCoA" else "CAP"
    }
  }
  # check compatibility of input
  if (!identical(ord_get(data), NULL) && !isFALSE(verbose)) {
    warning(
      "You already calculated an ordination (with ",
      info[["ordMethod"]], ") --> overwriting"
    )
  }
  if (!identical(distMat, NULL) &&
    method %in% c("PCA", "RDA", "CCA") &&
    !isFALSE(verbose)) {
    warning(
      "Distance matrix is not used for ", method,
      "! Removing distance matrix. ",
      "Did you mean to use PCoA or CAP? (or use method = auto)"
    )
    distMat <- NULL # so output ps_extra will get NULL distance matrix
  }
  if (identical(distMat, NULL) && method %in% c("CAP", "PCoA", "MDS", "NMDS")) {
    stop(
      "Distance matrix missing! Use dist_calc() ",
      "before using ord_calc() with this method: ", method
    )
  }
  if (constrained && method %in% c("PCA", "PCoA", "NMDS", "MDS")) {
    stop(
      method, " cannot use constraints, did you mean RDA, CAP or CCA. ",
      "Alternatively use method = auto (the default)"
    )
  }

  # constraint and condition handling in phyloseq object and distance matrix
  # handle missings and scale conditions and constraints if given
  # (for RDA or dbRDA/constrained PCoA)
  if (constrained || conditioned) {
    # remove '1' in case conditions is set but constraints is still 1 (default)
    VARS <- setdiff(c(constraints, conditions), 1)
    # drop samples with missings
    ps <- ps_drop_incomplete(ps, vars = VARS, verbose = verbose)
    # drop samples from distMat if no longer in ps
    if (!identical(distMat, NULL)) {
      keepers <- phyloseq::sample_names(ps)
      distMat <- stats::as.dist(as.matrix(distMat)[keepers, keepers])
    }
    if (!isFALSE(verbose) && isTRUE(scale_cc)) {
      message(
        "\nCentering (mean) and scaling (sd) the constraints and conditions: "
      )
    }
    for (v in VARS) {
      if (!isFALSE(verbose)) message("\t", v)
      vec <- phyloseq::sample_data(ps)[[v]]
      if (!class(vec) %in% c("logical", "numeric", "integer")) {
        stop(
          "Constraints and conditions must be numeric, logical, or integer: ",
          v, " is ", paste(class(vec), collapse = " ")
        )
      }
      if (isTRUE(scale_cc)) {
        phyloseq::sample_data(ps)[, v] <-
          scale(vec[!is.na(vec)], center = TRUE, scale = TRUE)
      }
    }
  }

  # set ordMethod
  # method is used to annotate plot,
  # ordMethod is used to specify how to actually do the ordination
  # deal with synonyms / equivalent unconstrained methods
  if (identical(method, "PCA")) {
    # synonymous for this purpose (PCA is unconstrained RDA)
    ordMethod <- "RDA"
  } else if (method %in% c("PCoA", "MDS")) {
    # CAP causes phyloseq::ordinate to call vegan::capscale which,
    # with formula DIST ~ 1, is PCoA (and gives scores!)
    ordMethod <- "CAP"
  } else {
    ordMethod <- method
  }

  if (ordMethod %in% c("RDA", "CAP", "CCA")) {

    # set formula to include any given constraints on RHS
    # (by default the RHS = 1)
    f <- paste0("distMat ~ ", paste(constraints, collapse = " + "))

    # add any conditions specified in conditions arg
    if (conditioned) {
      condVars <- paste(conditions, collapse = " + ")
      f <- paste0(f, " + Condition(", condVars, ")")
    }

    FORM <- stats::as.formula(f)

    # note RDA does not use distance arg
    ORD <- phyloseq::ordinate(
      physeq = ps, method = ordMethod, distance = distMat, formula = FORM, ...
    )
  } else if (method %in% c("DCA", "DPCoA", "NMDS") && !constrained) {
    # other valid unconstrained phyloseq methods
    ORD <- phyloseq::ordinate(
      physeq = ps, method = ordMethod, distance = distMat, ...
    )
  }

  # build return object
  info[["ordMethod"]] <- method
  if (constrained) info[["constraints"]] <- paste(constraints, collapse = "+")
  if (conditioned) info[["conditions"]] <- paste(conditions, collapse = "+")

  data[["ps"]] <- ps
  data[["info"]] <- info
  data[["dist"]] <- distMat
  data[["ord"]] <- ORD

  return(data)
}
