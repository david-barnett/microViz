library(phyloseq)
library(microbiome)
data("dietswap")

fixed_order <- c(
  "Fusobacteria", "Cyanobacteria", "Verrucomicrobia", "Spirochaetes",
  "Actinobacteria", "Firmicutes", "Proteobacteria", "Bacteroidetes"
)

test_that("fixing order of comp_barplot works", {
  p <- dietswap %>%
    ps_filter(timepoint == 1) %>%
    comp_barplot(
      tax_level = "Phylum", n_taxa = 8,
      sample_order = "bray",
      tax_order = fixed_order
    ) + coord_flip()

  expect_equal(
    object = levels(p$data$unique),
    expected = rev(fixed_order)
  )
})

test_that("tt_add_topN_var helper works as expected", {
  local_edition(3)
  tt <- microViz:::tt_add_topN_var(
    phyloseq::tax_table(dietswap),
    N = 4, other = "things", varname = "test"
  )
  expect_snapshot(
    print(head(x = tt, 6), width = 80)
    )
})

