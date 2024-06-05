data(esophagus, package = "phyloseq")

test_that("unifrac distances work", {
  local_edition(3)
  # GUniFrac 1.5 did not have verbose argument (relevant for R 3.6 checks)
  skip_if(packageVersion("GUniFrac") < "1.6")
  expect_snapshot(dist_get(dist_calc(esophagus, dist = 'gunifrac')))
  expect_snapshot(dist_get(dist_calc(esophagus, dist = 'unifrac')))
  expect_snapshot(dist_get(dist_calc(esophagus, dist = 'wunifrac')))
  # since GUniFrac 1.8, variance-adjusted unifrac appears to have been dropped
})

test_that("gunifrac alpha = 1 is wunifrac", {
  skip_if(packageVersion("GUniFrac") < "1.6")
  expect_equal(
    dist_get(dist_calc(esophagus, dist = "wunifrac")),
    dist_get(dist_calc(esophagus, dist = "gunifrac", gunifrac_alpha = 1))
  )
})

test_that("dist_calc rclr and euclid same as robust aitchison", {
  local_edition(3)
  robustAitchVeg <- microViz::ibd %>%
    otu_get() %>%
    vegan::vegdist(method = "robust.aitchison")

  robustAitchViz <- microViz::ibd %>%
    dist_calc(dist = "robust.aitchison") %>%
    dist_get()

  expect_equal(
    object = robustAitchVeg, expected = robustAitchViz,
    tolerance = 0.0000001, ignore_attr = c("call", "method", "maxdist")
  )

  rclrEuclid <- microViz::ibd %>%
    tax_transform("rclr") %>%
    dist_calc(dist = "euclidean") %>%
    dist_get()

  expect_equal(
    object = robustAitchVeg, expected = rclrEuclid,
    tolerance = 0.0000001, ignore_attr = c("call", "method", "maxdist")
  )
})

test_that("dist_calc throws errors", {
  expect_error(
    object = dist_calc(
      data = tax_transform(microViz::ibd, trans = "clr"),
      dist = "aitchison"
    ),
    regexp = "dist_calc 'aitchison' distance requires count data"
  )

  expect_error(
    object = dist_calc(
      data = tax_transform(microViz::ibd, trans = "rclr"),
      dist = "aitchison"
    ),
    regexp = "dist_calc 'aitchison' distance requires count data"
  )

  expect_error(dist_calc(data = 2), regexp = "data is class: numeric")
})
