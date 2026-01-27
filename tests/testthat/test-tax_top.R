test_that("tax_top works", {
  local_edition(3)
  data("enterotype", package = "phyloseq")
  expect_error(
    object = tax_top(enterotype, use_counts = TRUE),
    regexp = "otu_table of counts is NOT available"
  )
  expect_message(tax_top(enterotype), "NAs detected")
  expect_silent(TOPa <- tax_top(enterotype, verbose = FALSE))
  expect_equal(object = TOPa, expected = c(
    "-1", "Bacteroides", "Blautia", "Faecalibacterium", "Prevotella",
    "Roseburia", "Alistipes", "Subdoligranulum", "Lachnospiraceae", "Dorea"
  ))
  data("esophagus", package = "phyloseq")
  eso <- microViz:::psCheckTaxTable(esophagus, verbose = FALSE)
  eso <- microViz:::psCheckSamdat(eso, verbose = FALSE)
  eso <- tax_transform(eso, "comp_clr") # now a legacy method
  expect_silent(TOPb_transf <- tax_top(eso, n = NA))
  expect_silent(TOPb_counts <- tax_top(eso, n = NA, use_counts = TRUE))
  expect_snapshot_csv(TOPb_transf, name = "tax_top")
  expect_setequal(TOPb_counts, TOPb_transf)
  expect_failure(expect_equal(TOPb_counts, TOPb_transf))
})
