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
  expect_true(all.equal(
    dplyr::arrange(mdf, OTU, Sample),
    dplyr::arrange(mdf2, OTU, Sample),
    check.attributes = FALSE
  ))
})


test_that("ps_melt doesn't change", {
  local_edition(3)
  gp_ch <- phyloseq::subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
  mdf <- ps_melt(gp_ch)
  expect_snapshot_csv(name = "melted_ps", object = mdf)
})
