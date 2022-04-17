library(shinytest)

test_that("tax_fix_interactive() works", {
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_on_os("windows")
  # in future, see what server function testing can do with shiny::testServer()
  # https://shiny.rstudio.com/articles/server-function-testing.html

  expect_pass(testApp(appDir = "apps/tax_fix_interactive/", compareImages = FALSE))
})
