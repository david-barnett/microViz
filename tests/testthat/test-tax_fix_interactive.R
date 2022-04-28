library(shinytest)

test_that("tax_fix_interactive() works", {
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  testthat::skip_on_os("windows")
  # in future, see what server function testing can do with shiny::testServer()
  # https://shiny.rstudio.com/articles/server-function-testing.html

  # devel R has Shiny errors fixed in #3625
  # due to Sys.setenv(`_R_CHECK_LENGTH_1_LOGIC2_`="true")
  testthat::skip_if(getRversion() > "4.2" && packageVersion("shiny") <= "1.7.1")

  expect_pass(testApp(appDir = "apps/tax_fix_interactive/", compareImages = FALSE))
})
