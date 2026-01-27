library(shinytest2)

test_that("tax_fix_interactive() works", {
  local_edition(3)
  testthat::skip_on_cran()
  testthat::skip_on_bioc()
  skip_on_os(os = c("windows", "linux"))
  skip_on_os(os = "mac", arch = "x86_64")
  DTversion <- utils::packageVersion("DT")
  skip_if(DTversion < "0.26")

  # in future, see what server function testing can do with shiny::testServer()
  # https://shiny.rstudio.com/articles/server-function-testing.html

  # devel R has Shiny errors fixed in #3625
  # due to Sys.setenv(`_R_CHECK_LENGTH_1_LOGIC2_`="true")
  skip_if(getRversion() > "4.2" && packageVersion("shiny") <= "1.7.1")

  data("dietswap", package = "microbiome")
  expect_snapshot(dietswap)

  expect_message(
    shinyApp <- tax_fix_interactive(dietswap),
    "tax_fix_interactive looks best fullscreen"
  )
  # record_test(shinyApp)

  app <- AppDriver$new(
    app_dir = shinyApp, load_timeout = 1e+05, seed = 1,
    variant = shinytest2::platform_variant()
  )

  # app$view()
  app$set_inputs(min_char = 1)
  app$wait_for_value(input = "view_rows_all")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(selected = "Anaerostipes caccae et rel.")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(highlight = "Actinomycetaceae")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(tab = "fixed")
  app$wait_for_value(input = "in_tt_rows_all")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(tab = "tips")
  app$wait_for_idle()
  app$expect_values()
})
