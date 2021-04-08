
data(dietswap, package = "microbiome")

test_that("phyloseq_validate warns about removing all zero taxa", {
  local_edition(3)
  expect_snapshot(
    phyloseq_validate(
      ps = dietswap, remove_undetected = TRUE, verbose = TRUE
    )
  )
})

test_that("phyloseq_validate fixes missing sam_data with message", {
  local_edition(3)
  dietswap@sam_data <- NULL
  expect_snapshot(
    phyloseq_validate(
      ps = dietswap, verbose = TRUE
    )
  )
})

test_that("phyloseq_validate fixes missing tax_table with message", {
  local_edition(3)
  data("soilrep", package = "phyloseq")
  expect_snapshot(
    phyloseq_validate(soilrep, verbose = TRUE)
  )
})
