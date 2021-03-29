data("GlobalPatterns", package = "phyloseq")

test_that("ps_melt equivalent to psmelt", {
  gp_ch <- phyloseq::subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
  gp_ch <- ps_mutate(gp_ch, OTU = "testing")
  expect_warning(
    mdf <- ps_melt(gp_ch),
    regexp = "to avoid conflicts with special phyloseq plot attribute names"
  )
  expect_warning(
    mdf2 <- phyloseq::psmelt(gp_ch), # slower
    regexp = "to avoid conflicts with special phyloseq plot attribute names"
  )
  # same dataframe, except with somewhat different row orders
  expect_true(dplyr::all_equal(tibble::as_tibble(mdf), mdf2, convert = TRUE))
})
