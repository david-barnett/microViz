app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("test-barplot")

priorPlotValue <- app$waitForValue("comps_girafe", iotype = "output", ignore = list(NULL))
app$setInputs(
  ord_plot_selected = c("Sample-29", "Sample-19"), allowInputNoBinding_ = TRUE
)
priorPlotValue <- app$waitForValue("comps_girafe", iotype = "output", ignore = list(priorPlotValue))
app$snapshot()
app$setInputs(ntaxa = 19)
app$waitForValue("comps_girafe", iotype = "output", ignore = list(priorPlotValue))
app$snapshot()
