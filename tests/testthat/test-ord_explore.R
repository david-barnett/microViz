library(shinytest)

test_that("ord_explore() works", {
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_on_ci()

  expect_pass(testApp(appDir = "apps/ord_explore/", compareImages = FALSE))
})
