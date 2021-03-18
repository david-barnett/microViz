library(dplyr)
data("dietswap", package = "microbiome")

# create a couple of numerical variables to use
psq <- dietswap %>%
  ps_mutate(
    weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
    female = if_else(sex == "female", true = 1, false = 0),
    african = if_else(nationality == "AFR", true = 1, false = 0)
  )
psq <- tax_agg(psq, "Genus")

# randomly select 30 taxa from the 50 most abundant taxa
set.seed(123)
taxa <- sample(microbiome::top_taxa(ps_get(psq))[1:50], size = 30)

column_tax_anno <- tax_anno(undetected = 50, which = "column")

test_that("default taxa_side ('right') is not compatible with taxa column annotations", {
  expect_error(
    object = cor_heatmap(psq, taxa, anno_tax = column_tax_anno),
    regexp = "This is not compatible with the taxa_side argument you specified"
  )
})
