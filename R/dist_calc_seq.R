
#' Calculate distances between sequential samples in ps_extra/phyloseq object
#'
#' @param data ps_extra object, e.g. output from tax_transform()
#' @param dist
#' name of distance to calculate between pairs of sequential samples
#' @param group
#' name of variable in phyloseq sample_data used to define groups of samples
#' @param seq
#' name of variable in phyloseq sample_data used to define order of samples
#' within groups
#' @param unequal "error" or "warn" or "ignore" if groups of samples, defined
#' by group argument, are of unequal size
#' @param start_value value returned for the first sample in each group, which
#' has no preceding sample in the group's sequence, and so has no obvious value
#' @param return
#' format of return object: "data" returns ps_extra with sorted samples and
#' additional variable. "vector" returns only named vector of sequential
#' distances.
#' @param var_name name of variable created in ps_extra if return arg = "data"
#'
#' @return
#' ps_extra object sorted and with new sequential distance variable
#' or a named vector of that variable
#' @export
#'
#' @seealso \code{\link{dist_calc}}
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#'
#' pseq <- dietswap %>%
#'   tax_transform("identity", rank = "Genus") %>%
#'   dist_calc_seq(
#'     dist = "aitchison", group = "subject", seq = "timepoint",
#'     # group sizes are unequal because some subjects are missing a timepoint
#'     unequal = "ignore"
#'   )
#'
#' pseq %>%
#'   samdat_tbl() %>%
#'   dplyr::select(1, subject, timepoint, dplyr::last_col())
#'
#' # ggplot heatmap - unsorted
#' pseq %>%
#'   samdat_tbl() %>%
#'   filter(timepoint != 1) %>%
#'   ggplot(aes(x = timepoint, y = subject)) +
#'   geom_tile(aes(fill = aitchison_DistFromLast)) +
#'   scale_fill_viridis_c(na.value = NA, name = "dist") +
#'   theme_minimal(base_line_size = NA) +
#'   scale_y_discrete(limits = rev(levels(samdat_tbl(pseq)$subject)))
#'
#' # ComplexHeatmap plotting with clustering #
#' library(tidyr)
#' library(ComplexHeatmap)
#'
#' # make data matrix
#' heatmat <- pseq %>%
#'   samdat_tbl() %>%
#'   filter(timepoint != 1) %>%
#'   pivot_wider(
#'     id_cols = subject,
#'     names_from = timepoint, names_prefix = "t",
#'     values_from = aitchison_DistFromLast
#'   ) %>%
#'   tibble::column_to_rownames("subject")
#'
#' heatmat <- as.matrix.data.frame(heatmat)
#'
#' heatmap <- Heatmap(
#'   name = "dist",
#'   matrix = heatmat, col = viridisLite::viridis(12), na_col = "white",
#'   cluster_columns = FALSE,
#'   cluster_rows = hclust(dist(heatmat), method = "ward.D"),
#'   width = unit(1.5, "in"), rect_gp = gpar(col = "black"),
#'   row_names_side = "left", row_names_gp = gpar(fontsize = 8)
#' )
#' heatmap
#'
#' # comparison with subject tracking on PCA
#' pca <- pseq %>%
#'   # already sorted data
#'   dist_calc("aitchison") %>%
#'   ord_calc("PCoA") %>%
#'   ord_plot(alpha = 0.1, shape = "nationality", size = 2) %>%
#'   add_paths(
#'     mapping = aes(colour = subject, alpha = timepoint, size = timepoint),
#'     id_var = "subject", id_values = c(
#'       "eve", "hsf", # low variation
#'       "vem", # medium
#'       "ufm", # high variation
#'       "pku" # starts high
#'     )
#'   ) +
#'   scale_alpha_continuous(range = c(0.3, 1), breaks = c(2, 4, 6)) +
#'   scale_size_continuous(range = c(1, 2), breaks = c(2, 4, 6))
#'
#' heatmap
#' pca
dist_calc_seq <- function(data,
                          dist,
                          group,
                          seq,
                          unequal = "warn",
                          start_value = NaN,
                          return = "data",
                          var_name = paste0(dist, "_DistFromLast")) {

  # check input data object class
  distCalcDataValidate(data)

  # get phyloseq and info from data
  ps <- ps_get(data)
  info <- info_get(data)

  # check group and seq are variables in phyloseq
  psCheckVariables(ps = ps, vars = c(group, seq))

  # arrange by group then arrange by seq
  ps <- ps_arrange(ps = ps, .data[[group]], .data[[seq]])

  # split ps by grouping variable
  psSubgroups <- ps_split(ps, group = group, unequal = unequal)

  # re-attach ps_extra info to all subgroup phyloseqs
  psSubgroups <- lapply(psSubgroups, function(p) new_ps_extra(p, info = info))

  # dist_calc on each of the subgroup phyloseqs
  psSubgroups <- lapply(psSubgroups, dist_calc, dist = dist)

  # get distance matrix from each subgroup phyloseq
  psSubgroupDists <- lapply(psSubgroups, distGetSeq, start_value = start_value)

  # flatten distance vector
  sequentialDists <- unlist(psSubgroupDists)

  # maybe return distance vector
  if (identical(return, "vector")) {
    return(sequentialDists)
  }

  # attach distances as variable
  phyloseq::sample_data(ps)[[var_name]] <- sequentialDists

  # return data
  if (!is(data, "psExtra") && is(data, "phyloseq")) return(ps)
  if (is(data, "psExtra")) data <- update_psExtra(data, ps = ps)
  if (is_ps_extra(data)) data[["ps"]] <- ps
  return(data)
}

# helpers -----------------------------------------------------------------

# split phyloseq by levels of grouping variable group (in phyloseq sample_data)
# optionally check for unequal group sizes and error or warn
ps_split <- function(ps, group, unequal = "ignore") {
  groupLevels <- unique(phyloseq::sample_data(ps)[[group]])

  psSubgroups <- lapply(
    X = groupLevels,
    FUN = function(level) ps_filter(ps, .data[[group]] == level)
  )

  # check each subgroup phyloseq has same number of samples (warn/error)
  groupSizes <- sapply(X = psSubgroups, FUN = phyloseq::nsamples)

  if (length(unique(groupSizes)) > 1 && !identical(unequal, "ignore")) {
    tab <- sort(table(groupSizes), decreasing = TRUE)
    txt <- paste("\n- there are", tab, "groups with", names(tab), "samples")
    if (identical(unequal, "error")) {
      stop("groups must all have the same number of samples:", txt)
    } else if (identical(unequal, "warn") || identical(unequal, "warning")) {
      warning("groups do not all have the same number of samples:", txt)
    } else {
      stop("argument `unequal` must be one of 'ignore'/'warn'/'error'")
    }
  }
  names(psSubgroups) <- groupLevels
  return(psSubgroups)
}

# takes ps_extra object with distance matrix already calculated
# gets named vector of distances representing distance of a sample from the
# preceding sample in the (presumably ordered) phyloseq
distGetSeq <- function(ps_extra, start_value = NaN) {
  d <- dist_get(ps_extra = ps_extra)
  m <- as.matrix(d)
  # get subset of matrix representing distances between sequential samples
  # NaN, [2, 1], [3, 2], [4, 3] ... = c(NaN, mat[row(mat) - col(mat) == 1]])
  sequentialDistances <- c(start_value, m[row(m) - col(m) == 1])
  names(sequentialDistances) <- colnames(m)
  return(sequentialDistances)
}
