app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("mytest")

app$setInputs(min_char = 0)
tmp <- app$waitForValue("view_rows_all", iotype = "input", ignore = list(NULL))
app$snapshot()
app$setInputs(selected = "Anaerostipes caccae et rel.")
app$snapshot()
app$setInputs(highlight = "Actinomycetaceae")
app$snapshot()
app$setInputs(tab = "fixed")
tmp <- app$waitForValue("in_tt_rows_all", iotype = "input", ignore = list(NULL))
app$snapshot()
app$setInputs(tab = "tips")
app$snapshot()
rm(tmp)

# After making changes to the test script, run it with:
# testApp("tests/testthat/apps/tax_fix_interactive", "mytest.R")
