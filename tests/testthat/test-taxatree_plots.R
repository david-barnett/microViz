# corncob stats testing
library(dplyr)
library(corncob)

data("dietswap", package = "microbiome")
ps <- dietswap
# create some binary variables for easy visualisation
ps <- ps %>% ps_mutate(
  female = if_else(sex == "female", 1, 0, NaN),
  overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
  obese = if_else(bmi_group == "obese", 1, 0, NaN),
  african = if_else(nationality == "AFR", 1, 0, NaN)
)

# filter out rare taxa
ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)

testing_taxa <- c("Allistipes et rel.", "Akkermansia", "Bifidobacterium")

set.seed(123) # corncob model fitting may vary stochastically
# Model first 3 genera using all VARS as predictors (just for a quick test)
bb_models <- tax_model(
  ps = ps, tax_level = "Genus", type = "bbdml", verbose = FALSE,
  taxa = testing_taxa, variables = c("female", "overweight", "obese")
)
# Alternative method using formula argument
# instead of variables argument to produce identical results
bb_models2 <- tax_model(
  ps = ps, tax_level = "Genus", type = "bbdml", verbose = FALSE,
  taxa = testing_taxa, formula = ~ female + overweight + obese
)

# test bbdml models
for (t in testing_taxa){
  test_that(paste("tax_model bbdml results stay the same:", t), {
    local_edition(3)
    options(width = 80)
    expect_snapshot(names(bb_models[[t]]$b.mu))
    expect_snapshot(bb_models[[t]]$b.mu)
    expect_snapshot(bb_models[[t]]$b.phi)
  })
}

test_that("bbdml results independent of formula/variables specification", {
  expect_equal(
    object = bb_models2,
    expected = bb_models
  )
})

# tax_model output is wrong function for for plotting
test_that("can't plot tax_model output", {
  expect_error(
    taxatree_plots(ps, models_list = bb_models),
    regexp = "following names in the models_list are not in rank_names"
  )
})

# linear models
lm_models <- ps %>%
  tax_transform("compositional", rank = "Genus") %>%
  tax_model(
    tax_level = "Genus", type = "lm", verbose = FALSE,
    taxa = testing_taxa,
    formula = ~ female + overweight + obese
  )

# test lm models
for (t in testing_taxa){
  test_that(paste("tax_model compositional lm results stay the same:", t), {
    local_edition(3)
    options(width = 80)
    expect_snapshot(names(lm_models[[t]]$coefficients))
    expect_snapshot(lm_models[[t]]$coefficients)
    expect_snapshot(lm_models[[t]]$qr$qraux)
  })
}


# lm taxatree plots
tree_lm_models <- ps %>%
  tax_prepend_ranks() %>%
  tax_transform("compositional") %>%
  taxatree_models(
    type = "lm", tax_levels = 1:2,
    formula = ~ female + african, verbose = TRUE
  )

plots <- taxatree_plots(
  ps = tax_prepend_ranks(ps), models_list = tree_lm_models
)
keyplot <- taxatree_plotkey(ps = tax_prepend_ranks(ps), tax_levels = 1:2)


test_that("taxatree plots names are right", {
  expect_equal(names(plots), c("african", "female"))
})

test_that("taxatree plot scale limits are harmonised", {
  local_edition(3)
  expect_snapshot(plots$african$scales$scales[[3]]$limits)
  expect_equal(
    plots$african$scales$scales[[3]]$limits,
    plots$female$scales$scales[[3]]$limits
  )
})


for (v in c("african", "female")) {

  test_that(paste("taxatree plots and key plot layouts match:", v), {
    expect_equal(plots[[v]]$data$x, keyplot$data$x)
    expect_equal(plots[[v]]$data$y, keyplot$data$y)
    expect_equal(plots[[v]]$data$taxon_mean, keyplot$data$taxon_mean)
  })

  test_that(paste("taxatree lm plots don't change:", v), {
    local_edition(3)
    options(width = 80)
    expect_snapshot(plots[[v]]$data$y)
    expect_snapshot(plots[[v]]$data$x)
    expect_snapshot(plots[[v]]$data$taxon_mean)
    expect_snapshot(plots[[v]]$data$p.value)
    expect_snapshot(plots[[v]]$data$estimate)
    expect_snapshot(plots[[v]]$data$taxon_from)
  })
}

