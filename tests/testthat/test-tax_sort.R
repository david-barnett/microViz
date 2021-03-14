library(phyloseq)
library(microbiome)
data("dietswap")

sums <- phyloseq::taxa_sums(dietswap)

test_that("tax_sort sum ordering works", {
  expect_equal(
    object = taxa_names(tax_sort(dietswap, by = sum)),
    expected = names(sort(sums, decreasing = TRUE))
  )
})

# manually sort by descending prevalence, breaking ties with descending sums
prevs <- microbiome::prevalence(x = dietswap, detection = 0)
stopifnot(identical(names(sums), names(prevs)))
df <- data.frame(sums = sums, prevs = prevs, names = names(sums))
df <- dplyr::arrange(df, dplyr::desc(prevs), dplyr::desc(sums))


test_that("tax_sort prevalence ordering works", {
  expect_equal(
    object = tax_sort(dietswap, by = sum) %>% tax_sort("prev") %>% taxa_names(),
    expected = df[["names"]]
  )
})

test_that("expected names order for comparison hasn't changed", {
  local_edition(3L)
  expect_snapshot(cat(df[["names"]]), cran = FALSE)
})
