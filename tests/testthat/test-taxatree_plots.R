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


