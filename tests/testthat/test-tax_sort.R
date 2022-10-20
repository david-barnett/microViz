library(phyloseq)
data("dietswap", package = "microbiome")

test_that("tax_reorder works", {
  # sanity check of original order
  originalOrder <- c(
    "Actinobacteria", "Firmicutes", "Proteobacteria", "Verrucomicrobia",
    "Bacteroidetes", "Spirochaetes", "Fusobacteria", "Cyanobacteria"
  )
  expect_equal(
    object = tax_agg(dietswap, rank = "Phylum") %>% ps_get() %>%
      phyloseq::taxa_names(),
    expected = originalOrder
  )
  # set new order (all phyla named)
  new_order <- c(
    "Fusobacteria", "Cyanobacteria", "Verrucomicrobia", "Spirochaetes",
    "Actinobacteria", "Firmicutes", "Proteobacteria", "Bacteroidetes"
  )
  expect_equal(
    object = tax_agg(dietswap, rank = "Phylum") %>% ps_get() %>%
      tax_reorder(tax_order = new_order) %>%
      phyloseq::taxa_names(),
    expected = new_order
  )

  # partial reordering
  new_order2 <- union(c("Fusobacteria", "Cyanobacteria"), originalOrder)

  expect_equal(
    object = tax_agg(dietswap, rank = "Phylum") %>% ps_get() %>%
      tax_reorder(tax_order = new_order2[1:2]) %>%
      phyloseq::taxa_names(),
    expected = new_order2
  )

  # partial reordering and some non-matching taxa should throw a warning
  reordered <- expect_warning(
    object = tax_agg(dietswap, rank = "Phylum") %>% ps_get() %>%
      tax_reorder(tax_order = c(new_order2[1:2], "extra", "taxa")) %>%
      phyloseq::taxa_names(),
    regexp = "2 taxa specified in tax_order are not in phyloseq"
  )
  expect_equal(object = reordered, expected = new_order2)

  suppressWarnings(
    expect_error(
      object = tax_agg(dietswap, rank = "Phylum") %>% ps_get() %>%
        tax_reorder(tax_order = c("wrong", "taxa")),
      regexp = "tax_order did not match any taxa in ps"
    )
  )
})

sums <- phyloseq::taxa_sums(dietswap)

test_that("tax_sort sum ordering works", {
  expect_equal(
    object = taxa_names(tax_sort(dietswap, by = sum)),
    expected = names(sort(sums, decreasing = TRUE))
  )
})

# manually sort by descending prevalence, breaking ties with descending sums
prevs <- microbiome::prevalence(x = dietswap, detection = 0)
stopifnot(identical(names(sums), names(prevs)))
df <- data.frame(sums = sums, prevs = prevs, names = names(sums))
df <- dplyr::arrange(df, dplyr::desc(prevs), dplyr::desc(sums))


test_that("tax_sort prevalence ordering works", {
  expect_equal(
    object = tax_sort(dietswap, by = sum) %>%
      tax_sort("prev") %>%
      taxa_names(),
    expected = df[["names"]]
  )
})

test_that("expected names order for comparison hasn't changed", {
  local_edition(3L)
  expect_snapshot(cat(df[["names"]]), cran = FALSE)
})

test_that("reversing taxa order works", {
  expect_equal(
    object = phyloseq::taxa_names(tax_sort(dietswap, by = "rev")),
    expected = rev(phyloseq::taxa_names(dietswap))
  )
})
test_that("reversing taxa order ignores `at` arg", {
  expect_equal(
    object = phyloseq::taxa_names(tax_sort(dietswap, by = "rev", at = "bla")),
    expected = rev(phyloseq::taxa_names(dietswap))
  )
})

test_that("sorting by Phylum total abundance works", {
  res <- tax_sort(dietswap, by = sum, at = "Phylum")
  expect_equal(
    object = unique(unclass(phyloseq::tax_table(res))[, "Phylum"]),
    expected = c(
      "Bacteroidetes", "Firmicutes", "Proteobacteria", "Verrucomicrobia",
      "Actinobacteria", "Fusobacteria", "Spirochaetes", "Cyanobacteria"
    )
  )
})

test_that("otu_table returned untransformed at transform for sorting", {
  res <- tax_sort(dietswap, by = sum, trans = "compositional")
  expect_setequal(object = otu_get(res)[1, ], expected = otu_get(dietswap)[1, ])
})
