library(phyloseq)
# get example data
data("enterotype", package = "phyloseq")
ps <- enterotype

test_that("tax_name doesn't change", {
  local_edition(3)

  ps1 <- tax_name(ps)
  expect_snapshot_csv("ps1-names", object = taxa_names(ps1))

  ps2 <- tax_name(ps, rank = "Genus")
  expect_snapshot_csv("ps2-names", object = taxa_names(ps2))
})
