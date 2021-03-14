library(microbiome)
data("dietswap", package = "microbiome")

test_that("tax_transform and agg 1 and 2 step options equivalent", {
  expect_equal(
    object = tax_transform(dietswap, transformation = "clr", rank = "Phylum"),
    expected = tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
  )
})
