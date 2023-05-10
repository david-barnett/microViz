data("dietswap", package = "microbiome")
pS <- dietswap

# Test basic functionality ----
test_that("tax_select works on dietswap data", {
  a <- tax_select(pS, tax_list = "Bif", n_typos = 0, ranks_searched = "Genus")
  b <- tax_select(pS, tax_list = "Bifidobacterium", n_typos = 0)
  c <- tax_select(pS, tax_list = "Bif", n_typos = 1)
  expect_equal(a, b)
  expect_true(phyloseq::ntaxa(a) < phyloseq::ntaxa(c)) # FALSE
})

test_that("tax_select works with more typos", {
  one <- tax_select(pS, tax_list = "Akkarmensia", n_typos = 2)
  two <- tax_select(pS, tax_list = "Akkermansia", n_typos = 0)
  expect_equal(one, two)
})

test_that("tax_select throws error on no matches", {
  expect_error(
    tax_select(pS, tax_list = "Bif", strict_matches = TRUE),
    regexp = "No taxa matched"
  )
})

# Test input format checks ----
test_that("tax_select works with a list of character strings", {
  d <- tax_select(pS, tax_list = list("Bif", "Akkermansia"), n_typos = 0)
  e <- tax_select(pS, tax_list = c("Bif", "Akkermansia"), n_typos = 0)
  expect_equal(d, e)
})

test_that("tax_select throws error on non-character entries in tax_list", {
  expect_error(
    object = tax_select(pS, tax_list = 123, n_typos = 0),
    regexp = "tax_list must be a character vector or a list of strings"
  )
  expect_error(
    object = tax_select(pS, tax_list = list("Bif", 123), n_typos = 0),
    regexp = "tax_list list must contain only character strings"
  )
})

test_that("tax_select throws error on non-logical strict_matches or deselect inputs", {
  expect_error(
    object = tax_select(pS, tax_list = "Bif", strict_matches = "not_logical"),
    regexp = "strict_matches and deselect must be logical values"
  )
  expect_error(
    object = tax_select(pS, tax_list = "Bif", deselect = "not_logical"),
    regexp = "strict_matches and deselect must be logical values"
  )
})
