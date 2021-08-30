local_edition(3)
data("dietswap", package = "microbiome")

test_that("tax_transform and agg 1 and 2 step options equivalent", {
  expect_equal(
    object = tax_transform(dietswap, transformation = "clr", rank = "Phylum"),
    expected = tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
  )
})

test_that("tax_transform doesn't change", {
  ps <- dietswap
  comp <- tax_transform(ps, transformation = "compositional", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_prop", object = round(otu_get(comp), 4))

  clr <- tax_transform(ps, transformation = "clr", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_clr", object = round(otu_get(clr), 4))

  l10p <- tax_transform(ps, transformation = "log10p", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_l10p", object = round(otu_get(l10p), 4))

  bin10 <- tax_transform(
    data = ps, rank = "Family", transformation = "binary", undetected = 10
  )
  expect_snapshot_csv(name = "diet_fam_bin10", object = otu_get(bin10))
})
