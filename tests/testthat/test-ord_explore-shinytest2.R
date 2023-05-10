library(shinytest2)

test_that("ord_explore app works: unconstrained ords", {
  # Skip tests in most conditions
  skip_on_cran()
  skip_if(Sys.info()[["machine"]] == "arm64")
  skip_on_os(os = c("windows", "linux"))

  # Set local edition and options
  local_edition(3)
  options(width = 80)

  # Prepare the data
  data("shao19")
  ps <- shao19 %>% ps_filter(infant_age == 10, sex == "female") # only 5 samples

  # Expect snapshots
  expect_snapshot(ps)
  expect_snapshot(phyloseq::sample_variables(ps))
  expect_snapshot(phyloseq::sample_names(ps))
  expect_snapshot_csv(phyloseq::taxa_names(ps), name = "taxa-names")

  # Initialize the shiny app
  expect_message(
    shinyapp <- ord_explore(
      data = ps, notification_durations = NULL, modal_fade = FALSE
    ),
    "To stop the app"
  )

  # Prepare the AppDriver
  app <- suppressWarnings(
    AppDriver$new(
      app_dir = shinyapp, timeout = 15000, seed = 123,
      name = "general-ord-explore-test", width = 1600, height = 960,
      variant = shinytest2::platform_variant()
    )
  )
  # use record_test(app) to record a new test
  app$wait_for_idle()

  # keep this commented out unless interactive testing
  # app$view()

  # Set initial inputs and click build
  app$expect_values() # test 1
  app$set_inputs(rank = "genus")
  app$set_inputs(trans = "clr")
  app$wait_for_idle()
  app$expect_values() # 2

  # Skip test based on package versions
  skip_if_not(packageVersion("ggiraph") > "0.8.3" || packageVersion("ggplot2") < "3.4.0")
  # test locally with dev version in the meantime until visual change in barplot legend (thick borders) is fixed

  # check state after first build
  app$click("build")
  app$wait_for_idle()
  app$expect_values() # 3

  # check sample selection
  app$set_inputs(
    ord_plot_selected = c("SID513122_ba_10", "SID515126_ba_10", "SID521130_ba_10"),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values() # 4

  # check barchart modifications work
  app$set_inputs(mergeOther = FALSE)
  app$set_inputs(tax_level_comp = "species")
  app$set_inputs(tax_order = "median")
  app$set_inputs(facet_by = "sex")
  app$set_inputs(ntaxa = 19)
  app$set_inputs(comp_label = "family_id")
  app$wait_for_idle()
  app$expect_values() # 5

  # check for error message for gunifrac on aggregated taxa
  app$click("settings")
  app$set_inputs(rank = "family")
  app$set_inputs(trans = "identity")
  app$set_inputs(dist = "gunifrac")
  app$set_inputs(method = "PCoA")
  app$wait_for_idle()
  app$expect_values() # 6
  app$click("build")
  app$wait_for_idle()
  app$expect_values() # 7

  # check error avoided
  app$set_inputs(rank = "unique")
  app$click("build")
  app$wait_for_idle()
  app$expect_values() # 8

  # select a sample
  app$set_inputs(
    ord_plot_selected = "SID606141_ba_10",
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values() # 9

  # change to variable size
  app$set_inputs(sizeFixed = FALSE)
  app$set_inputs(ord_size_var = "number_reads")
  app$wait_for_idle()
  app$expect_values() # 10

  # change axis displayed
  app$set_inputs(y1 = 3)
  app$set_inputs(ord_colour = "sex")
  app$set_inputs(id_var = "sex")
  app$set_inputs(
    ord_plot_selected = "female",
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values() # 11

  # test convex hulls can be added
  app$set_inputs(add = "chulls")
  app$wait_for_idle()
  app$expect_values() # 12
})
