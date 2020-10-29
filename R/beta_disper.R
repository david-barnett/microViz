#' Wrapper for vegan::betadisper
#'
#' Takes the output of calc_dist function. Or use with the result of the permanova function to ensure the results correspond to exactly the same input data.
#' Runs betadisper for all categorical variables in variables argument.
#' See help for vegan::betadisper.
#'
#' @param data list output from calc_dist
#' @param variables list of variables to use as group
#' @param method centroid or median
#' @param complete_cases drop samples with NAs in any of the variables listed
#' @param verbose sends messages about progress if true
#' @param return what parts of return list to return, defaults to all parts
#'
#' @return list containing betadisper results list and input objects
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
#' sample_data(dietswap)$timepoint <- as.numeric(sample_data(dietswap)$timepoint)
#'
#' # straight to the betadisp
#' bd1 <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   calc_dist("aitchison") %>%
#'   beta_disper(variables = c("sex", "bmi_group", "timepoint"))
#' # quick vegan plotting methods
#' plot(bd1$beta_disper$sex$model, label.cex = 0.5)
#' boxplot(bd1$beta_disper$sex$model)
#'
#' # compute distance and use for both permanova and beta_disper
#' testDist <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   calc_dist("bray")
#'
#' PERM <- testDist %>%
#'   permanova(
#'     variables = c("sex", "bmi_group"),
#'     n_processes = 1, n_perms = 99
#'   )
#' str(PERM, max.level = 1)
#'
#' bd <- PERM %>% beta_disper(variables = c("sex", "bmi_group"))
#' bd$beta_disper$bmi_group
beta_disper <- function(data,
                        variables,
                        method = c("centroid", "median")[[1]],
                        complete_cases = TRUE,
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

  # calculate bdisp and anova and tukeyHSD confidence/significance for all variables
  bdisp <- lapply(variables, function(V) {
    if (!class(metadata[[V]]) %in% c("logical", "character", "factor", "integer")) {
      warning("Variable ", V, " is skipped as it cannot be used for grouping (class = ", class(metadata[[V]]), ")")
      return(NULL)
    } else {
      model <- vegan::betadisper(d = distMat, group = metadata[[V]], type = method)
      Anova <- stats::anova(object = model)
      tukeyHSD <- stats::TukeyHSD(model)
      return(list(model = model, anova = Anova, tukeyHSD = tukeyHSD))
    }
  })

  names(bdisp) <- variables

  # return object (results and processing info)
  out <- list(
    info = info, beta_disper = bdisp, distMat = distMat, ps = ps
  )

  if (return == "all") {
    return(out)
  } else if (length(return) == 1) {
    return(out[[return]])
  } else {
    return(out[return])
  }
  return(bdisp)
}
