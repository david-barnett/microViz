local_edition(3)

test_that("ibd_phylo names are as expected, and are replaced consistently", {
  data("ibd_phylo", package = "corncob")

  ps <- ibd_phylo %>%
    tax_filter(min_prevalence = 15) %>%
    tax_fix()

  expect_snapshot_csv(
    name = "ibd_phylo_names", object = phyloseq::taxa_names(ps)
  )
  expect_snapshot_csv(
    name = "ibd_renamed",
    object = phyloseq::taxa_names(tax_rename(ps, rank = "Genus"))
  )
  expect_snapshot_csv(
    name = "ibd_renamed_max",
    object = phyloseq::taxa_names(
      tax_rename(ps, rank = "Genus", pad_digits = "max")
    )
  )
  expect_error(
    object = tax_rename(ps, rank = "Gebus"),
    regexp = "`rank` must be the name of a valid rank"
  )
  expect_error(
    object = tax_rename(ps, rank = "Genus", pad_digits = TRUE),
    regexp = "pad_digits must be 'auto', 'max', or a number from 0 to 10"
  )
  expect_error(
    object = tax_rename(ps, rank = "Genus", sep = 1:5)
  )
})
