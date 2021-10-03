data("dietswap", package = "microbiome")


test_that("tax_mutate errors on creation of non-character ranks", {
  expect_error(
    object = tax_mutate(dietswap, var1 = 1, var2 = factor("a")),
    regexp = "tax_mutate created a non-character rank in taxonomy"
  )
})

test_that("tax_mutate returns phyloseq with new ranks", {
  ps <- tax_mutate(
    data = dietswap,
    genius = paste(Phylum, Genus, "!"),
    Family = NULL
  )
  expect_s4_class(ps, "phyloseq")
  expect_equal(phyloseq::rank_names(ps), c("Phylum", "Genus", "genius"))

  # no other unexpected changes
  expect_equal(phyloseq::sample_names(dietswap), phyloseq::sample_names(ps))
  expect_equal(phyloseq::taxa_names(dietswap), phyloseq::taxa_names(ps))
})


test_that("tax_mutate warns when data is ps_extra", {
  ps2 <- dietswap %>% tax_agg("Genus")
  expect_warning(
    object = tax_mutate(ps2, new = "abc"),
    regexp = "data is ps_extra but only a phyloseq will be returned"
  )
  expect_s4_class(suppressWarnings(tax_mutate(ps2, new = "abc")), "phyloseq")
})
