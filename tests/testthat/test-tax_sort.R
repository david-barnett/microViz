library(phyloseq)
library(microbiome)
data("dietswap")

test_that("tax_sort sum ordering works", {
  sums <- names(sort(taxa_sums(dietswap), decreasing = TRUE))
  expect_equal(
    object = taxa_names(tax_sort(dietswap, by = sum)),
    expected = sums
  )
})
