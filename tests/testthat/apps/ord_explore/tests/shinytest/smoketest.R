app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("smoketest")

app$setInputs(interactive = FALSE)
app$snapshot(list(output = "comps_gg"))
