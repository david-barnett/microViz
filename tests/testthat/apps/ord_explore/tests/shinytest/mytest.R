app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot(list(output = "comps_gg"))
