library(phyloseq)
library(microbiome)
data("dietswap")

local_edition(3)

test_that("microbiome's dietswap data hasn't changed", {
  expect_snapshot(dietswap)
  tt_df <- data.frame(tax_table(dietswap), check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)
  otu_df <- data.frame(otu_table(dietswap), check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)
  expect_snapshot_csv(name = "tt", object = tt_df)
  expect_snapshot_csv(name = "otu", object = otu_df)
})


agg_level_test <- c("Phylum", "Family", "Genus")
for (level in agg_level_test) {
  test_that(
    desc = paste("tax_agg produces same phyloseq as microbiome::aggregate_taxa at", level),
    code = {
      biome <- microbiome::aggregate_taxa(x = dietswap, level = level)
      viz <- ps_get(tax_agg(ps = dietswap, agg_level = level))
      expect_equal(object = viz, expected = biome)
    }
  )

  test_that(
    desc = paste("microbiome::aggregate_taxa output hasn't changed:", level),
    code = {
      expect_snapshot(microbiome::aggregate_taxa(x = dietswap, level = level))
    }
  )

  test_that(
    desc = paste("microViz::tax_agg output hasn't changed:", level),
    code = {
      expect_snapshot(ps_get(tax_agg(ps = dietswap, agg_level = level)))
    }
  )
}
