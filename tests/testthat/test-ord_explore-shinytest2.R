library(shinytest2)
options(width = 80)


test_that("ord_explore app works: unconstrained ords", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  skip_on_os("windows")
  local_edition(3)

  data("shao19")
  ps <- shao19 %>% ps_filter(infant_age == 10) # only 8 samples
  expect_snapshot(ps)
  expect_snapshot(phyloseq::sample_variables(ps))
  expect_snapshot(phyloseq::sample_names(ps))
  expect_snapshot_csv(phyloseq::taxa_names(ps), name = "taxa-names")

  expect_message(
    shinyapp <- ord_explore(
      data = ps, notification_durations = NULL, modal_fade = FALSE
    ),
    "To stop the app"
  )
  # record_test(shinyapp)

  app <- suppressWarnings(
    AppDriver$new(
      shinyapp,
      timeout = 10000,
      variant = platform_variant(os_name = TRUE, r_version = FALSE),
      name = "general-ord-explore-test", width = 1619, height = 976
    )
  )
  # app$view()
  # Update output value
  app$expect_values()
  app$wait_for_idle()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$set_inputs(rank = "genus")
  app$set_inputs(trans = "clr")
  app$expect_values()
  app$click("build")
  app$wait_for_idle()
  # Update output value
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$set_inputs(
    ord_plot_selected = c("SID513122_ba_10", "SID604136_ba_10", "BBS0008_ba_10"),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  # Update output value
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$set_inputs(mergeOther = FALSE)
  # Update output value
  app$set_inputs(interactive = FALSE)
  # Update output value
  app$set_inputs(tax_level_comp = "species")
  # Update output value
  app$set_inputs(tax_order = "median")
  # Update output value
  app$set_inputs(facet_by = "sex")
  # Update output value
  app$set_inputs(ntaxa = 19)
  # Update output value
  app$set_inputs(comp_label = "family_id")
  # Update output value
  app$wait_for_idle()
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$click("settings")
  app$set_inputs(rank = "family")
  app$set_inputs(trans = "identity")
  app$set_inputs(dist = "gunifrac")
  app$set_inputs(method = "PCoA")
  app$expect_values()
  app$click("build")
  # Update output value
  app$expect_values()
  app$set_inputs(rank = "unique")
  app$click("build")
  # Update output value
  app$wait_for_idle(duration = 2000)
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$set_inputs(
    ord_plot_selected = "BBS0008_ba_10", allow_no_input_binding_ = TRUE
  )
  # Update output value
  app$wait_for_idle()
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$set_inputs(sizeFixed = FALSE)
  # Update output value
  app$set_inputs(ord_size_var = "number_reads")
  # Update output value
  app$expect_values()
  app$set_inputs(y1 = 3)
  # Update output value
  app$set_inputs(ord_colour = "sex")
  # Update output value
  app$set_inputs(id_var = "sex")
  # Update output value
  app$set_inputs(ord_plot_selected = "female", allow_no_input_binding_ = TRUE)
  # Update output value
  app$expect_values()
  app$set_inputs(add = "chulls")
  # Update output value
  app$wait_for_idle()
  app$expect_values()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
  app$click("code")
  # Update output value
  app$wait_for_idle()
  app$expect_screenshot(threshold = 20, quiet = FALSE)
})

