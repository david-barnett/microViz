library(phyloseq)
library(microbiome)
data("dietswap")

options(width = 80)
local_edition(3)

test_that("microbiome's dietswap data hasn't changed", {
  expect_snapshot(dietswap)
  tt_df <- data.frame(tax_table(dietswap), check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)
  otu_df <- data.frame(otu_table(dietswap), check.names = FALSE, check.rows = FALSE, stringsAsFactors = FALSE)
  expect_snapshot_csv(name = "tt", object = tt_df)
  expect_snapshot_csv(name = "otu", object = otu_df)
})


agg_level_test <- c("Phylum", "Family")
# tax_agg not expected to be the same at Genus for dietswap
# as microbiome::aggregate_taxa doesn't add "unique" rank if aggregated by
# last rank
for (level in agg_level_test) {
  test_that(
    desc = paste("tax_agg produces same phyloseq as microbiome::aggregate_taxa at", level),
    code = {
      # biome output must be sorted by name (using tax_sort)
      # it is not clear how to recreate the order produced by aggregate taxa itself
      biome <- tax_sort(microbiome::aggregate_taxa(x = dietswap, level = level), by = "name")
      viz <- tax_sort(ps_get(tax_agg(ps = dietswap, level, add_unique = TRUE)), by = "name")
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
      expect_snapshot(ps_get(tax_agg(ps = dietswap, level, sort_by = "name", add_unique = TRUE)))
    }
  )
}

test_that(
  desc = paste("tax_agg with top_N and no agg eqivalent to microbiome::top_taxa"),
  code = {
    biome_top <- microbiome::top_taxa(x = dietswap, n = 40)
    viz_tt <- tt_get(tax_agg(dietswap, rank = "unique", top_N = 40))
    viz_top <- unname(unclass(viz_tt)[, "top"])[1:40]
    expect_equal(object = viz_top, expected = biome_top)
  }
)

test_that(
  desc = paste("tax_top gives same results as tax_agg with top_N and no agg"),
  code = {
    tax_top_out <- tax_top(dietswap, n = 40, by = sum)
    viz_tt <- tt_get(tax_agg(dietswap, rank = "unique", top_N = 40, sort_by = sum))
    viz_top <- unname(unclass(viz_tt)[, "top"])[1:40]
    expect_equal(object = tax_top_out, expected = viz_top)
  }
)

test_that(
  desc = paste("tax_agg with top_N and Family agg eqivalent to aggregate_taxa and top_taxa"),
  code = {
    biome_top <- dietswap %>%
      microbiome::aggregate_taxa("Family") %>%
      microbiome::top_taxa(n = 10)
    viz_tt <- tt_get(tax_agg(dietswap, rank = "Family", top_N = 10))
    viz_top <- unname(unclass(viz_tt)[, "top"])[1:10]
    expect_equal(object = viz_top, expected = biome_top)
  }
)

test_that("tax_fix error prompt looks right", {
  expect_snapshot(cat(taxFixPrompt()))
  expect_snapshot(cat(taxFixPrompt(unknowns = c("anUnknown", "another"))))
})

test_that("tax_agg errors on NAs or convergent values", {
  phyloseq::tax_table(dietswap)[3, "Genus"] <- NA
  expect_snapshot(tax_agg(dietswap, rank = "Genus"), error = TRUE)
  phyloseq::tax_table(dietswap)[3:10, "Genus"] <- "g__"
  expect_snapshot(tax_agg(dietswap, rank = "Genus"), error = TRUE)
})
