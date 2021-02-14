# corncob stats testing
library(dplyr)
library(phyloseq)
library(microbiome)
library(corncob)

data(dietswap)
ps <- dietswap
# create some binary variables for easy visualisation
ps <- ps %>% ps_mutate(
  female = if_else(sex == "female", 1, 0, NaN),
  overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
  obese = if_else(bmi_group == "obese", 1, 0, NaN)
)

# filter out rare taxa
ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)

models <- tax_model(ps, tax_level = "Genus", taxa = 1:2, formula = ~female, verbose = FALSE)

test_that("can't plot tax_model output", {
  expect_error(
    taxatree_plots(ps, models_list = models),
    regexp = "following names in the models_list are not in rank_names"
  )
})
