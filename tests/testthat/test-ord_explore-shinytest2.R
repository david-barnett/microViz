library(shinytest2)

test_that("ord_explore app works: unconstrained ords", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  local_edition(3)
  options(width = 80)

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
      app_dir = shinyapp, timeout = 10000,
      name = "general-ord-explore-test", width = 1619, height = 976
    )
  )
  # app$view()
  app$expect_values()
  app$wait_for_idle()
  app$set_inputs(rank = "genus")
  app$set_inputs(trans = "clr")
  app$expect_values()
  app$click("build")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(
    ord_plot_selected = c("SID513122_ba_10", "SID604136_ba_10", "BBS0008_ba_10"),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(mergeOther = FALSE)
  app$set_inputs(tax_level_comp = "species")
  app$set_inputs(tax_order = "median")
  app$set_inputs(facet_by = "sex")
  app$set_inputs(ntaxa = 19)
  app$set_inputs(comp_label = "family_id")
  app$wait_for_idle()
  app$expect_values()
  app$click("settings")
  app$set_inputs(rank = "family")
  app$set_inputs(trans = "identity")
  app$set_inputs(dist = "gunifrac")
  app$set_inputs(method = "PCoA")
  app$expect_values()
  app$click("build")
  app$expect_values()
  app$set_inputs(rank = "unique")
  app$click("build")
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(
    ord_plot_selected = "BBS0008_ba_10", allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle()
  app$expect_values()
  app$set_inputs(sizeFixed = FALSE)
  app$set_inputs(ord_size_var = "number_reads")
  app$expect_values()
  app$set_inputs(y1 = 3)
  app$set_inputs(ord_colour = "sex")
  app$set_inputs(id_var = "sex")
  app$set_inputs(ord_plot_selected = "female", allow_no_input_binding_ = TRUE)
  app$expect_values()
  app$set_inputs(add = "chulls")
  app$wait_for_idle()
  app$expect_values()
})

