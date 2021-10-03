#' Wrapper for vegan::betadisper()
#'
#' Takes the output of dist_calc function. Or use with the result of the permanova function to ensure the results correspond to exactly the same input data.
#' Runs betadisper for all categorical variables in variables argument.
#' See help('betadisper', package = 'vegan').
#'
#' @param data ps_extra output from dist_calc
#' @param variables list of variables to use as group
#' @param method centroid or median
#' @param complete_cases drop samples with NAs in any of the variables listed
#' @param verbose sends messages about progress if true
#'
#' @return ps_extra list containing betadisper results
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
                       method = c("centroid", "median")[[1]],
                       complete_cases = TRUE,
                       verbose = TRUE) {

  # check input data object class
  if (inherits(data, "ps_extra") && !identical(dist_get(data), NULL)) {
    ps <- data[["ps"]]
    distMat <- data[["dist"]]
  } else {
    stop("data argument must be a ps_extra object from dist_calc")
  }

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
  data[["dist"]] <- distMat
  data[["bdisp"]] <- bdisp
  return(data)
}
