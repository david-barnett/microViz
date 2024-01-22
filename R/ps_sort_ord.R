#' Sort phyloseq samples by ordination axes scores
#'
#' @description
#' `ps_sort_ord` reorders samples in a phyloseq object based on their relative
#' position on 1 or 2 ordination axes.
#'
#' `ord_order_samples` gets the sample_names in order from the ordination
#' contained in a psExtra. This is used internally by `ps_sort_ord`
#'
#' If 2 axes given, the samples are sorted by anticlockwise rotation around the
#' selected ordination axes, starting on first axis given, upper right quadrant.
#' (This is used by ord_plot_iris.)
#'
#' If 1 axis is given, samples are sorted by increasing value order along this axis.
#' This could be used to arrange samples on a rectangular barplot in order of
#' appearance along a parallel axis of a paired ordination plot.
#'
#' @param ps phyloseq object to be sorted
#' @param ord psExtra with ordination object
#' @param axes which axes to use for sorting? numerical vector of length 1 or 2
#' @inheritParams ord_plot
#' @rdname  ordination-sorting-samples
#' @export
#' @return
#' `ps_sort_ord` returns a phyloseq
#'
#' `ord_order_samples` returns a character vector
#'
#' @seealso
#' - These functions were created to support ordering of samples on `ord_plot_iris`
#' - `tax_sort_ord` for ordering taxa in phyloseq by ordination
#'
#' @examples
#' # attach other necessary packages
#' library(ggplot2)
#'
#' # example data
#' ibd <- microViz::ibd %>%
#'   tax_filter(min_prevalence = 2) %>%
#'   tax_fix() %>%
#'   phyloseq_validate()
#'
#' # create numeric variables for constrained ordination
#' ibd <- ibd %>%
#'   ps_mutate(
#'     ibd = as.numeric(ibd == "ibd"),
#'     steroids = as.numeric(steroids == "steroids"),
#'     abx = as.numeric(abx == "abx"),
#'     female = as.numeric(gender == "female"),
#'     # and make a shorter ID variable
#'     id = stringr::str_remove_all(sample, "^[0]{1,2}|-[A-Z]+")
#'   )
#'
#' # create an ordination
#' ordi <- ibd %>%
#'   tax_transform("clr", rank = "Genus") %>%
#'   ord_calc()
#'
#' ord_order_samples(ordi, axes = 1) %>% head(8)
#' ps_sort_ord(ibd, ordi, axes = 1) %>%
#'   phyloseq::sample_names() %>%
#'   head(8)
#'
#' p1 <- ord_plot(ordi, colour = "grey90", plot_taxa = 1:8, tax_vec_length = 1) +
#'   geom_text(aes(label = id), size = 2.5, colour = "red")
#'
#' b1 <- ibd %>%
#'   ps_sort_ord(ord = ordi, axes = 1) %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 12, label = "id",
#'     order_taxa = ord_order_taxa(ordi, axes = 1),
#'     sample_order = "asis"
#'   ) +
#'   theme(axis.text.x = element_text(angle = 90, hjust = 1))
#'
#' patchwork::wrap_plots(p1, b1, ncol = 1)
#'
#' # constrained ordination example (and match vertical axis) #
#' cordi <- ibd %>%
#'   tax_transform("clr", rank = "Genus") %>%
#'   ord_calc(
#'     constraints = c("steroids", "abx", "ibd"), conditions = "female",
#'     scale_cc = FALSE
#'   )
#'
#' cordi %>% ord_plot(plot_taxa = 1:6, axes = 2:1)
ps_sort_ord <- function(ps, ord, axes = 1:2, scaling = 2) {
  # get names of samples in order
  sortedSamples <- ord_order_samples(ord = ord, axes = axes, scaling = scaling)

  # reorder samples in phyloseq
  ps <- ps_reorder(ps = ps, sample_order = sortedSamples)
  return(ps)
}

#' @export
#' @rdname ordination-sorting-samples
ord_order_samples <- function(ord, axes = 1:2, scaling = 2) {
  ord <- ord_get(ord)

  # get axes
  scoreMatrix <- vegan::scores(
    x = ord, choices = axes, display = "sites", scaling = scaling
  )

  if (length(axes) == 1) {
    # convert 1-column matrix to named vector
    scoreVector <- scoreMatrix[, 1, drop = TRUE]
    sortedSamples <- names(sort(scoreVector, decreasing = FALSE))
  } else if (length(axes) == 2) {
    df <- as.data.frame.matrix(scoreMatrix, stringsAsFactors = FALSE)
    df[["ID"]] <- rownames(df)

    # find anticlockwise angle from right x-axis
    df[["angle"]] <- dplyr::if_else(
      condition = df[[2]] > 0,
      true = atan2(x = df[[1]], y = df[[2]]),
      false = (2 * pi) + (atan2(x = df[[1]], y = df[[2]]))
    )

    df <- dplyr::arrange(df, .data$angle)
    sortedSamples <- rownames(df)
  } else {
    stop("Length of `axes` argument must be 1 or 2")
  }
  return(sortedSamples)
}
