local_edition(3)
data("dietswap", package = "microbiome")

test_that("tax_transform and agg 1 and 2 step options equivalent", {
  expect_equal(
    object = tax_transform(dietswap, trans = "clr", rank = "Phylum"),
    expected = tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
  )
})

test_that("tax_transform doesn't change", {
  ps <- dietswap
  comp <- tax_transform(ps, trans = "compositional", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_prop", object = round(otu_get(comp), 4))

  clr <- tax_transform(ps, trans = "clr", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_clr", object = round(otu_get(clr), 4))

  bin10 <- tax_transform(ps, rank = "Family", trans = "binary", undetected = 10)
  expect_snapshot_csv(name = "diet_fam_bin10", object = otu_get(bin10))

  l10p <- tax_transform(ps, trans = "log10p", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_l10p", object = round(otu_get(l10p), 4))

  l2p <- tax_transform(ps, rank = "Family", trans = "log2", zero_replace = 1)
  expect_snapshot_csv(name = "diet_fam_l2p", object = round(otu_get(l2p), 4))

  # error on not replacing zeros before log2
  expect_error(object = {
    tax_transform(ps, rank = "Family", trans = "log2", zero_replace = 0)
  }, regexp = "711 zeros detected in otu_table")

})
