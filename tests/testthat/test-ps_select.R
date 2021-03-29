data("enterotype", package = "phyloseq")

test_that("dplyr selection helper selection works", {
  expect_equal(
    phyloseq::sample_variables(ps_select(enterotype, dplyr::starts_with("S"))),
    expected = c("Sample_ID", "SeqTech", "SampleID")
  )
})
