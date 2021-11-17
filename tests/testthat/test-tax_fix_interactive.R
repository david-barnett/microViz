library(shinytest)

test_that("tax_fix_interactive() works", {
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_on_os("windows")
  # DT package now has timestamps in json under view_tate
  # making shinytest snapshot testing impossible? --> skip for now
  # https://github.com/rstudio/DT/issues/944
  # in future, see what server function testing can do with shiny::testServer()
  # https://shiny.rstudio.com/articles/server-function-testing.html
  testthat::skip("Skipping since DT saves timestamp now")
  testthat::skip_on_ci()

  expect_pass(testApp(appDir = "apps/tax_fix_interactive/", compareImages = FALSE))
})
