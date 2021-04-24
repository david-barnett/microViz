app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("mytest")

app$setInputs(min_char = 0)
app$snapshot()
app$snapshot()
app$setInputs(selected = "Firmicutes")
app$snapshot()
app$setInputs(highlight = "Aerococcus")
app$snapshot()
app$snapshot()
app$setInputs(suffix = "current")
app$setInputs(sep = " ...")
app$setInputs(selected = character(0))
app$setInputs(selected = "Anaerofustis")
app$snapshot()

# https://github.com/rfaelens/exampleShinyTest/blob/master/tests/shinyTest/tests/test.R
# wait for the process to close gracefully
# this allows covr to write out the coverage results
p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
