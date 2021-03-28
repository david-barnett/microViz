data("dietswap", package = "microbiome")

clr_pca <- dietswap %>%
  ps_filter(timepoint %in% c(1, 2)) %>%
  tax_agg("Genus") %>%
  tax_transform("clr", keep_counts = FALSE) %>%
  ord_calc(method = "PCA")

test_that("ord_plot_iris warns counts aren't available", {
  expect_warning(
    object = ord_plot_iris(data = clr_pca, tax_level = "Genus"),
    regexp = "otu_table of counts is NOT available"
  )
})
