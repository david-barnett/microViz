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
  expect_snapshot_csv(name = "samples_RDA", object = p$data[, 1:4])
  expect_snapshot_csv(name = "taxa_vecs_RDA", object = p$layers[[2]]$data)
  expect_snapshot_csv(name = "constraints_RDA", object = p$layers[[3]]$data)
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
  expect_snapshot_csv(name = "samples_CAP", object = p2$data[, 1:4])
  expect_snapshot_csv(name = "constraint_CAP", object = p2$layers[[2]]$data)
})

