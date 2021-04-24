library(dplyr)
library(tibble)
library(phyloseq)
library(microbiome)
library(corncob)
library(future)
library(future.apply)

data(dietswap)
ps <- dietswap
# create some binary variables for easy visualisation
ps <- ps %>% ps_mutate(
  female = if_else(sex == "female", 1, 0, NaN),
  overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
  obese = if_else(bmi_group == "obese", 1, 0, NaN)
)

# This example dataset has some taxa with the same name for phylum and family...
# We can fix problems like this with the tax_prepend_ranks function
ps <- tax_prepend_ranks(ps)
# this example dataset also has no root, this is unusual and needs to be fixed
tax_table(ps) <- cbind(root = "root", tax_table(ps))
# filter out rare taxa
ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
# specify variables used for modelling
VARS <- c("female", "overweight", "obese")

models <- tax_model(
  ps,
  type = "bbdml", tax_level = "Genus", taxa = 1:5, variables = VARS
)
# Alternative method using formula arg instead of variables to produce identical results
models2 <- tax_model(
  ps,
  type = "bbdml", tax_level = "Genus", taxa = 1:5,
  formula = ~ female + overweight + obese
)
all.equal(models, models2) # should be TRUE

test_that("formula and variable arg alternatives can be equivalent", {
  expect_equal(models, models2)
})
