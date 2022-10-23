data("ibd_phylo", package = "corncob")

phylo <- ibd_phylo %>%
  ps_filter(DiseaseState %in% c("UC", "nonIBD")) %>%
  tax_mutate(Species = NULL, Genus = NULL) %>%
  tax_fix() %>%
  ps_mutate(
    UC = ifelse(DiseaseState == "UC", yes = 1, no = 0),
    female = ifelse(gender == "female", yes = 1, no = 0),
    age_scaled = scale(age, center = TRUE, scale = TRUE)
  )

# check informative taxatree_nodes failures
test_that("taxatree_nodes errors on duplicate ranks", {
  expect_error(
    object = phylo %>% taxatree_nodes(),
    regexp = "tax_table values must not be duplicated across ranks, but some"
  )
})
test_that("taxatree_nodes errors on bad fun argument", {
  expect_error(
    object = phylo %>% taxatree_nodes(fun = "wrong"),
    regexp = "fun must be a length 1 named list holding a function for a vector"
  )
})

# build models
lm_models <- phylo %>%
  tax_prepend_ranks() %>%
  tax_transform("compositional", rank = "Family", keep_counts = TRUE) %>%
  tax_filter(min_prevalence = 0.3, undetected = 0, use_counts = TRUE) %>%
  tax_transform(trans = "log2", chain = TRUE, zero_replace = "halfmin") %>%
  taxatree_models(
    type = lm,
    ranks = NULL, # uses every rank available except the first
    variables = c("UC", "female", "age_scaled")
  )

# convert models to stats
lm_stats <- taxatree_models2stats(lm_models)

# test plotting errors
test_that("taxatree_plots throw informative errors", {
  expect_error(
    object = lm_models %>% taxatree_plots(),
    regexp = "data must be a psExtra object with taxatree_stats data.frame"
  )
  expect_error(
    object = lm_stats %>% taxatree_plots(colour_stat = "wrong"),
    regexp = "colour_stat must be the name of a column"
  )
  # remove required columns
  lm_stats_wrong <- lm_stats
  lm_stats_wrong@taxatree_stats[c("taxon", "rank")] <- NULL
  expect_error(
    object = lm_stats_wrong %>% taxatree_plots(),
    regexp = "It is missing the column\\(s\\): taxon & rank"
  )
})


test_that("taxatree_plot plotting works", {
  local_edition(3)
  lm_plots <- taxatree_plots(lm_stats)
  # test basic plotting success
  expect_equal(names(lm_plots), c("UC", "female", "age_scaled"))
  expect_s3_class(lm_plots[[1]], "ggplot")

  # visual diff plots
  skip_on_os("windows")
  vdiffr::expect_doppelganger("taxatree_plot_UC", lm_plots$UC)
  vdiffr::expect_doppelganger("taxatree_plot_age", lm_plots$age_scaled)
})



test_that("taxatree_plot plotting works with multiple sig markers", {
  local_edition(3)
  # test multiple significance markers succeeds
  lmp_multiSig <- taxatree_plots(
    lm_stats,
    sig_shape = list(4, "circle filled"), # 4 = cross, tests conversion
    sig_threshold = c(0.01, 0.1), sig_stat = "p.value", sig_stroke = 1.5,
    sig_size = c(2, 1), sig_colour = c("white", "green")
  )
  expect_equal(names(lmp_multiSig), c("UC", "female", "age_scaled"))
  expect_s3_class(lmp_multiSig[[1]], "ggplot")

  # visual diff plots
  skip_on_os("windows")
  vdiffr::expect_doppelganger("taxatree_plot_UC_m", lmp_multiSig$UC)
  vdiffr::expect_doppelganger("taxatree_plot_age_m", lmp_multiSig$age_scaled)
})


#
#
# # OLD TESTING BELOW
# # corncob stats testing
# library(dplyr)
# library(corncob)
#
# data("dietswap", package = "microbiome")
# ps <- dietswap
# # create some binary variables for easy visualisation
# ps <- ps %>% ps_mutate(
#   female = if_else(sex == "female", 1, 0, NaN),
#   overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
#   obese = if_else(bmi_group == "obese", 1, 0, NaN),
#   african = if_else(nationality == "AFR", 1, 0, NaN)
# )
#
# # filter out rare taxa
# ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
#
# testing_taxa <- c("Allistipes et rel.", "Akkermansia", "Bifidobacterium")
#
# set.seed(123) # corncob model fitting may vary stochastically
# # Model first 3 genera using all VARS as predictors (just for a quick test)
# bb_models <- tax_model(
#   ps = ps, rank = "Genus", type = "bbdml", verbose = FALSE,
#   taxa = testing_taxa, variables = c("female", "overweight", "obese")
# )
# # Alternative method using formula argument
# # instead of variables argument to produce identical results
# bb_models2 <- tax_model(
#   ps = ps, rank = "Genus", type = "bbdml", verbose = FALSE,
#   taxa = testing_taxa, formula = ~ female + overweight + obese
# )
#
# # test bbdml models
# for (t in testing_taxa) {
#   test_that(paste("tax_model bbdml results stay the same:", t), {
#     local_edition(3)
#     options(width = 80)
#     expect_snapshot(names(bb_models[[t]]$b.mu))
#     expect_snapshot(bb_models[[t]]$b.mu)
#     expect_snapshot(bb_models[[t]]$b.phi)
#   })
# }
#
# test_that("bbdml results independent of formula/variables specification", {
#   expect_equal(
#     object = bb_models2,
#     expected = bb_models
#   )
# })
#
# # tax_model output is wrong function for for plotting
# test_that("can't plot tax_model output", {
#   expect_error(
#     taxatree_plots(ps, models_list = bb_models),
#     regexp = "following names in the models_list are not in rank_names"
#   )
# })
#
# # linear models
# lm_models <- ps %>%
#   tax_transform("compositional", rank = "Genus") %>%
#   tax_model(
#     rank = "Genus", type = "lm", verbose = FALSE,
#     taxa = testing_taxa,
#     formula = ~ female + overweight + obese
#   )
#
# # test lm models
# for (t in testing_taxa) {
#   test_that(paste("tax_model compositional lm results stay the same:", t), {
#     local_edition(3)
#     options(width = 80)
#     expect_snapshot(names(lm_models[[t]]$coefficients))
#     expect_snapshot(lm_models[[t]]$coefficients)
#     expect_snapshot(lm_models[[t]]$qr$qraux)
#   })
# }
#
#
# # lm taxatree plots
# tree_lm_models <- ps %>%
#   tax_prepend_ranks() %>%
#   tax_transform("compositional") %>%
#   taxatree_models(
#     type = "lm", ranks = 1:2,
#     formula = ~ female + african, verbose = TRUE
#   )
#
# plots <- taxatree_plots(
#   ps = tax_prepend_ranks(ps), models_list = tree_lm_models$taxatree_models # temporary test until taxatree_plots code updated to take only a ps_extra with $taxatree_stats entry
# )
# keyplot <- taxatree_plotkey(ps = tax_prepend_ranks(ps), tax_levels = 1:2)
#
#
# test_that("taxatree plots names are right", {
#   expect_equal(names(plots), c("african", "female"))
# })
#
# test_that("taxatree plot scale limits are harmonised", {
#   local_edition(3)
#   expect_snapshot(plots$african$scales$scales[[3]]$limits)
#   expect_equal(
#     plots$african$scales$scales[[3]]$limits,
#     plots$female$scales$scales[[3]]$limits
#   )
# })
#
#
# for (v in c("african", "female")) {
#   test_that(paste("taxatree plots and key plot layouts match:", v), {
#     expect_equal(plots[[v]]$data$x, keyplot$data$x)
#     expect_equal(plots[[v]]$data$y, keyplot$data$y)
#     expect_equal(plots[[v]]$data$taxon_mean, keyplot$data$taxon_mean)
#   })
#
#   test_that(paste("taxatree lm plots don't change:", v), {
#     local_edition(3)
#     options(width = 80)
#     expect_snapshot(round(plots[[v]]$data$y, digits = 8))
#     expect_snapshot(round(plots[[v]]$data$x, digits = 8))
#     expect_snapshot(plots[[v]]$data$taxon_mean)
#     expect_snapshot(plots[[v]]$data$p.value)
#     expect_snapshot(plots[[v]]$data$estimate)
#     expect_snapshot(plots[[v]]$data$taxon_parent)
#   })
# }
