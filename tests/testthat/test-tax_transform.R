local_edition(3)
data("dietswap", package = "microbiome")

test_that("tax_transform and agg 1 and 2 step options equivalent", {
  expect_equal(
    object = tax_transform(dietswap, trans = "clr", rank = "Phylum"),
    expected = tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
  )
})


test_that("tax_transform add and zero_replace work", {
  data("shao19")
  ps <- ps_filter(shao19, family_id %in% 1:3)

  expect_equal(
    otu_get(ps) + 0.1,
    otu_get(tax_transform(ps, trans = "identity", add = 0.1))
  )

  expect_s4_class(
    addHm <- tax_transform(ps, trans = "identity", add = "halfmin"),
    class = "psExtra"
  )
  expect_s4_class(
    replaceHm <- tax_transform(ps, trans = "identity", zero_replace = "halfmin"),
    class = "psExtra"
  )
  expect_equal(min(otu_get(addHm)), min(otu_get(replaceHm)))
  expect_failure(expect_equal(otu_get(addHm), otu_get(replaceHm), ignore_attr = TRUE))
})


test_that("tax_transform doesn't change", {
  ps <- dietswap
  comp <- tax_transform(ps, trans = "compositional", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_prop", object = round(otu_get(comp), 4))

  # this is now a legacy routine: - replicating microbiome <1.32.0 behaviour
  clr <- tax_transform(ps, trans = "comp_clr", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_clr", object = round(otu_get(clr), 4))

  rclr <- tax_transform(ps, trans = "rclr", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_rclr", object = round(otu_get(rclr), 4))

  bin10 <- tax_transform(ps, rank = "Family", trans = "binary", undetected = 10)
  expect_snapshot_csv(name = "diet_fam_bin10", object = otu_get(bin10))

  l10p <- tax_transform(ps, trans = "log10p", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_l10p", object = round(otu_get(l10p), 4))

  l2p <- tax_transform(ps, rank = "Family", trans = "log2", zero_replace = 1)
  expect_snapshot_csv(name = "diet_fam_l2p", object = round(otu_get(l2p), 4))

  # error on not replacing zeros before log2
  expect_error(object = {
    tax_transform(ps, rank = "Family", trans = "log2", zero_replace = 0)
  }, regexp = "711 zeros detected in otu_table")
})

test_that("tax_transform clr output doesn't change", {
  skip_if(
    packageVersion("microbiome") < "1.32.0",
    message = "requires microbiome >= 1.32.0 for current clr behaviour"
  )
  ps <- dietswap
  clr2 <- tax_transform(ps, trans = "clr", rank = "Family")
  expect_snapshot_csv(name = "diet_fam_clr2", object = round(otu_get(clr2), 4))
})

test_that("clr and rclr equivalent with no zeros", {
  testr <- dietswap %>%
    tax_transform("rclr", rank = "Family", zero_replace = 1) %>%
    otu_get()
  testc <- dietswap %>%
    tax_transform("clr", rank = "Family", zero_replace = 1) %>%
    otu_get()
  expect_equal(testr, testc, tolerance = 0.0000000001)
})

test_that("tax_transform 'maaslin2-default' chaining works", {
  ps <- dietswap
  trans1 <- tax_transform(ps, trans = "compositional", rank = "Genus")
  # error as no zero replacement
  expect_error(
    object = tax_transform(trans1, "log2", chain = TRUE),
    regexp = "5935 zeros detected in otu_table"
  )
  # error as chain = FALSE
  expect_error(
    object = tax_transform(trans1, "log2", chain = FALSE),
    regexp = "data were already transformed by: compositional"
  )
  ord <- trans1 %>%
    tax_transform("log2", zero_replace = "halfmin", chain = TRUE) %>%
    ord_calc("PCA")
  expect_snapshot(ord)
  p <- ord_plot(ord) + ggplot2::theme_test()
  vdiffr::expect_doppelganger("trans-chaining", fig = p)

  # ensure preserved counts aren't changing
  counts1 <- microViz:::ps_counts(trans1)
  counts2 <- microViz:::ps_counts(ord)
  expect_equal(counts1, counts2)
})
