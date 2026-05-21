#' Wrapper for vegan::betadisper()
#'
#' Runs `vegan::betadisper()` on a distance matrix stored in a `psExtra`
#' object, usually produced by `dist_calc()`. This can also be used on the
#' result of `dist_permanova()` to ensure that dispersion and PERMANOVA results
#' correspond to the same distance matrix and sample set.
#'
#' `dist_bdisp()` fits one betadisper model per grouping variable. For each
#' valid grouping variable, it also stores the corresponding `anova()` and
#' `TukeyHSD()` results.
#'
#' @param data A `psExtra` object containing a distance matrix, as returned by
#'   `dist_calc()` or by downstream functions such as `dist_permanova()`.
#' @param variables Character vector of sample-data variable names to use as
#'   grouping variables. Variables must be categorical or coercible groupings;
#'   unsupported variable classes are skipped with a warning.
#' @param method Either `"centroid"` or `"median"`. Passed to
#'   `vegan::betadisper(type = ...)`.
#' @param complete_cases Logical. If `TRUE`, samples with missing values in any
#'   of the specified `variables` are removed before running `betadisper()`.
#'   If `FALSE`, the function errors if missing values are present.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @details
#' `vegan::betadisper()` tests whether groups differ in their multivariate
#' dispersion, i.e. their average distance to a group centroid or spatial median.
#' This is often used as a companion check when interpreting PERMANOVA results.
#'
#' microViz currently defaults to `method = "centroid"`, whereas recent versions
#' of `vegan::betadisper()` default to `type = "median"`.
#'
#' When `complete_cases = TRUE`, samples with missing values in any requested
#' grouping variable are removed once, before all betadisper models are fitted.
#' This means all returned models use the same filtered distance matrix.
#'
#' @return A `psExtra` object containing betadisper results. Results are stored
#'   by variable name; each entry contains the fitted `betadisper` model, its
#'   `anova()` table, and its `TukeyHSD()` result.
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(vegan)
#' data("dietswap", package = "microbiome")
#'
#' # add some missings to demonstrate automated removal
#' sample_data(dietswap)$sex[3:6] <- NA
#' # create a numeric variable to show it will be skipped with a warning
#' dietswap <- ps_mutate(dietswap, timepoint = as.numeric(timepoint))
#'
#' # straight to the betadisp
#' bd1 <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("aitchison") %>%
#'   dist_bdisp(variables = c("sex", "bmi_group", "timepoint")) %>%
#'   bdisp_get()
#' bd1$sex
#' # quick vegan plotting methods
#' plot(bd1$sex$model, label.cex = 0.5)
#' boxplot(bd1$sex$model)
#'
#' # compute distance and use for both permanova and dist_bdisp
#' testDist <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("bray")
#'
#' PERM <- testDist %>%
#'   dist_permanova(
#'     variables = c("sex", "bmi_group"),
#'     n_processes = 1, n_perms = 99
#'   )
#' str(PERM, max.level = 1)
#'
#' bd <- PERM %>% dist_bdisp(variables = c("sex", "bmi_group"))
#' bd
dist_bdisp <- function(data,
                       variables,
                       method = "centroid",
                       complete_cases = TRUE,
                       verbose = TRUE) {
  method <- match.arg(arg = method, choices = c("centroid", "median"))
  # check input data object class
  check_is_psExtra(data, argName = "data")
  if (identical(dist_get(data), NULL)) {
    stop("data argument must be a psExtra object from dist_calc")
  }

  ps <- ps_get(data)
  distMat <- dist_get(data)

  if (isFALSE(complete_cases)) {
    if (anyNA(phyloseq::sample_data(ps)[, variables])) {
      stop(
        "phyloseq contains missings within at least one of the specified variables",
        "\n\tTry complete_cases = TRUE or manually call `ps_drop_incomplete()`"
      )
    }
  }
  # drop observations with missings
  ps <- ps_drop_incomplete(ps, vars = variables, verbose = verbose)
  # drop samples from any pre-existing distMat if no longer in ps after dropping incomplete
  if (exists("distMat") && !identical(distMat, NULL)) {
    keepers <- phyloseq::sample_names(ps)
    distMat <- stats::as.dist(as.matrix(distMat)[keepers, keepers])
    # drop samples from any existing count matrix
    if (!is.null(data@counts)) data@counts <- data@counts[keepers, ]
  }

  # extract sample metadata from phyloseq object
  meta <- samdatAsDataframe(ps)[, variables, drop = FALSE]

  # calculate bdisp and anova and tukeyHSD confidence/significance for all variables
  bdisp <- lapply(variables, function(V) {
    if (!class(meta[[V]]) %in% c("logical", "character", "factor", "integer")) {
      warning(
        "Variable '", V,
        "' is skipped as it cannot be used for grouping (class = '",
        class(meta[[V]]), "')"
      )
      return(NULL)
    } else {
      model <- vegan::betadisper(d = distMat, group = meta[[V]], type = method)
      Anova <- stats::anova(object = model)
      tukeyHSD <- stats::TukeyHSD(model)
      return(list(model = model, anova = Anova, tukeyHSD = tukeyHSD))
    }
  })

  names(bdisp) <- variables
  data@bdisp <- bdisp
  data@dist <- distMat # might be filtered
  data@counts <- data@counts # might be filtered
  return(data)
}
