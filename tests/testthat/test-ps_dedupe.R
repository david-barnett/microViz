library(phyloseq)
data("dietswap", package = "microbiome")

ps1 <- ps_dedupe(
  dietswap,
  method = "first", verbose = FALSE,
  vars = c("timepoint", "group", "bmi_group")
)

ps2 <- ps_dedupe(
  dietswap,
  method = "readcount", verbose = FALSE,
  vars = c("timepoint", "group", "bmi_group")
)


test_that("ps_dedupe method = readcount gives same nsamples as method = first", {
  expect_equal(nsamples(ps1), nsamples(ps2))
})


test_that("ps_dedupe method = first example works", {
  expect_equal(nsamples(ps1), 18)
})

test_that("ps_dedupe dietswap readcount example works", {
  expect_equal(nsamples(ps2), 18)
})


