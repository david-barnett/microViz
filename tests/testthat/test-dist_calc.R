data(esophagus, package = "phyloseq")

test_that("unifrac distances work", {
  local_edition(3)
  for (d in c("gunifrac", "unifrac", "wunifrac", "va-wunifrac")) {
    expect_snapshot(dist_get(dist_calc(esophagus, dist = d)))
  }
})

test_that("gunifrac alpha = 1 is wunifrac", {
  expect_equal(
    dist_get(dist_calc(esophagus, dist = "wunifrac")),
    dist_get(dist_calc(esophagus, dist = "gunifrac", gunifrac_alpha = 1))
  )
})


test_that("dist_calc rclr and euclid same as robust aitchison", {
  local_edition(3)
  robustAitchVeg <- corncob::soil_phylum_small %>%
    otu_get() %>%
    vegan::vegdist(method = "robust.aitchison")

  robustAitchViz <- corncob::soil_phylum_small %>%
    dist_calc(dist = "robust.aitchison") %>%
    dist_get()

  expect_equal(
    object = robustAitchVeg, expected = robustAitchViz,
    tolerance = 0.0000001, ignore_attr = c("call", "method")
  )

  rclrEuclid <- corncob::soil_phylum_small %>%
    tax_transform("rclr") %>%
    dist_calc(dist = "euclidean") %>%
    dist_get()

  expect_equal(
    object = robustAitchVeg, expected = rclrEuclid,
    tolerance = 0.0000001, ignore_attr = c("call", "method")
  )
})

test_that("dist_calc throws errors", {
  expect_error(
    object = dist_calc(
      data = tax_transform(corncob::soil_phylum_small, trans = "clr"),
      dist = "aitchison"
    ),
    regexp = "dist_calc 'aitchison' distance requires count data"
  )

  expect_error(
    object = dist_calc(
      data = tax_transform(corncob::soil_phylum_small, trans = "rclr"),
      dist = "aitchison"
    ),
    regexp = "dist_calc 'aitchison' distance requires count data"
  )

  expect_error(dist_calc(data = 2), regexp = "data is class: numeric")
})


