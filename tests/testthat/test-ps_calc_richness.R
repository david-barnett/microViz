
test_that("ps_calc_richness supported plot doesn't change", {

  p <- corncob::ibd_phylo %>%
    ps_filter(abx == "abx") %>%
    tax_fix() %>%
    ps_calc_richness("Genus", index = "observed") %>%
    ps_calc_richness("Family", index = "chao1") %>%
    tax_transform(rank = "Genus", transform = "clr") %>%
    ord_calc("PCA") %>%
    ord_plot(
      size = "observed_Genus", colour = "chao1_Family"
    ) +
    ggplot2::scale_radius(range = c(1, 6)) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::theme_test()

  vdiffr::expect_doppelganger(title = "richness-pca", fig = p)
})

test_that("ps_calc_richness errors work", {
  psTest <- corncob::ibd_phylo %>%
    ps_filter(abx == "abx") %>%
    tax_filter(min_prevalence = 10) %>% # just for quick test purposes
    tax_fix()

  expect_error(
    object = psTest %>% ps_calc_richness(rank = "banana"),
    regexp = "`rank` must be the name of a valid rank:
Kingdom / Phylum / Class / Order / Family / Genus / Species
or: unique"
  )

  expect_error(
    object = psTest %>% ps_calc_richness(rank = "Genus", index = "lol"),
    regexp = 'must be one of "observed" or "chao1"'
  )

  expect_warning(
    object = psTest %>%
      ps_calc_richness(rank = "Genus") %>%
      ps_calc_richness(rank = "Genus"),
    regexp = "observed_Genus is already a variable in phyloseq sample data -> OVERWRITING"
  )
})
