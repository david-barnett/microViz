
library(phyloseq)
data("GlobalPatterns")
gp_ch <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
mdf <- ps_melt(gp_ch)
mdf2 <- psmelt(gp_ch) # slower

test_that("ps_melt equivalent to psmelt", {
  # same dataframe, except with somewhat different row orders
  expect_true(dplyr::all_equal(tibble::as_tibble(mdf), mdf2, convert = TRUE))
})
