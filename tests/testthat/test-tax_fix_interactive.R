library(shinytest)

test_that("tax_fix_interactive() works", {
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_on_ci()

  expect_pass(testApp(appDir = "apps/tax_fix_interactive/", compareImages = FALSE))
})

