data("dietswap", package = "microbiome")

clr_pca <- dietswap %>%
  ps_filter(timepoint %in% c(1, 2)) %>%
  tax_agg("Genus") %>%
  tax_transform("clr", keep_counts = FALSE) %>%
  ord_calc(method = "PCA")

test_that("ord_plot_iris fails as it can't aggregate CLR transformed values", {
  expect_error(
    object = suppressWarnings(ord_plot_iris(data = clr_pca, tax_level = "Genus")),
    regexp = "otu_table values must not be negative when aggregating"
  )
})
