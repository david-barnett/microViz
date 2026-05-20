data("dietswap", package = "microbiome")

test_that("dist_bdisp matches vegan betadisper when type is aligned", {
  dist_data <- dietswap %>%
    tax_agg("Genus") %>%
    dist_calc("bray")

  group <- samdat_tbl(ps_get(dist_data), sample_names_col = NA)[["sex"]]
  dist_mat <- dist_get(dist_data)

  vegan_median <- vegan::betadisper(d = dist_mat, group = group)
  vegan_centroid <- vegan::betadisper(d = dist_mat, group = group, type = "centroid")

  viz_default <- dist_data %>%
    dist_bdisp(variables = "sex", verbose = FALSE) %>%
    bdisp_get()

  viz_median <- dist_data %>%
    dist_bdisp(variables = "sex", method = "median", verbose = FALSE) %>%
    bdisp_get()

  expect_equal(
    object = viz_default$sex$model$distances,
    expected = vegan_centroid$distances
  )

  expect_equal(
    object = viz_median$sex$model$distances,
    expected = vegan_median$distances
  )
})
