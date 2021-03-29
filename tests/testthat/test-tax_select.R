
data("dietswap", package = "microbiome")
pS <- dietswap

test_that("tax_select works on dietswap data", {
  a <- tax_select(pS, tax_list = "Bif", n_typos = 0, ranks_searched = "Genus")
  b <- tax_select(pS, tax_list = "Bifidobacterium", n_typos = 0)
  c <- tax_select(pS, tax_list = "Bif", n_typos = 1)
  expect_equal(a, b) # TRUE
  expect_true(phyloseq::ntaxa(a) < phyloseq::ntaxa(c)) # FALSE
})

test_that("tax_select works with more typos", {
  one <- tax_select(pS, tax_list = "Akkarmensia", n_typos = 2)
  two <- tax_select(pS, tax_list = "Akkermansia", n_typos = 0)
  expect_equal(one, two) # TRUE
})

test_that("tax_select throws error on no matches", {
  expect_error(
    tax_select(pS, tax_list = "Bif", strict_matches = TRUE),
    regexp = "No taxa matched"
  )
})
