data("dietswap", package = "microbiome")

clr_pca <- dietswap %>%
  ps_filter(timepoint %in% c(1, 2)) %>%
  tax_agg("Genus") %>%
  tax_transform("clr") %>%
  ord_calc("PCA")

test_that("ord_plot_iris stops and requests ps arg if data is noted as transformed", {
  expect_error(
    ord_plot_iris(ord = clr_pca, tax_level = "Genus")
  )
})
