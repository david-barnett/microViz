local_edition(3)

library(ggplot2)
data("dietswap", package = "microbiome")

# create a couple of numerical variables to use as constraints or conditions
ps <- dietswap %>%
  ps_mutate(
    weight = dplyr::recode(bmi_group, obese = 3, overweight = 2, lean = 1),
    female = dplyr::if_else(sex == "female", true = 1, false = 0)
  ) %>%
  tax_filter(min_prevalence = 1)

# RDA ------------------------

p <- suppressMessages(
  # messages about scaling
  ps %>%
    tax_transform("clr", rank = "Genus") %>%
    ord_calc(constraints = c("weight", "female")) %>%
    ord_plot(colour = "bmi_group", plot_taxa = 1:3) +
    lims(x = c(-5, 6), y = c(-5, 5))
)


test_that("constrained rda plot gives correct positions", {
  expect_snapshot(cat(p$data[1:50, 1, drop = TRUE]))
  expect_snapshot(cat(p$data[1:50, 2, drop = TRUE]))
  expect_snapshot(cat(p$layers[[2]]$data[, 1, drop = TRUE]))
  expect_snapshot(cat(p$layers[[3]]$data[, 1, drop = TRUE]))
  expect_snapshot(cat(p$layers[[3]]$data[, 2, drop = TRUE]))
})

# PCoA ------------------------

p2 <- suppressMessages(
  # messages about scaling
  ps %>%
    tax_transform("identity", rank = "Genus") %>%
    dist_calc("bray") %>%
    ord_calc(conditions = "weight", constraints = "female") %>%
    ord_plot(colour = "bmi_group", plot_taxa = TRUE)
)

test_that("partialed bray CAP plot gives correct positions", {
  expect_snapshot(cat(p2$data[1:50, 1, drop = TRUE]))
  expect_snapshot(cat(p2$data[1:50, 2, drop = TRUE]))
  expect_snapshot(cat(p2$layers[[2]]$data[, 1, drop = TRUE]))
})

# aitchison and clr PCA equivalence ----------------------------------------

p3 <- ps %>%
  tax_transform(transformation = "identity", rank = "Genus") %>%
  dist_calc("aitchison") %>%
  ord_calc(method = "PCoA") %>%
  ord_plot(colour = "bmi_group")

p4 <- ps %>%
  tax_transform(transformation = "clr", rank = "Genus") %>%
  ord_calc(method = "PCA") %>%
  ord_plot(colour = "bmi_group")

test_that("clr PCA equivalent to aitchison PCoA", {
  expect_equal(
    object = abs(round(unname(p3$data[, 1:2]), digits = 10)),
    expected = abs(round(unname(p4$data[, 1:2]), digits = 10))
  )
})

test_that("aitchison plot hasn't changed", {
  expect_snapshot(cat(abs(p3$data[1:50, 1, drop = TRUE])))
  expect_snapshot(cat(abs(p3$data[1:50, 2, drop = TRUE])))
  expect_snapshot(p3$layers)
})

test_that("clr PCA plot hasn't changed", {
  expect_snapshot(cat(p4$data[1:50, 1, drop = TRUE]))
  expect_snapshot(cat(p4$data[1:50, 2, drop = TRUE]))
  expect_snapshot(p4$layers)
})
