test_that("ps_calc_diversity supported plot doesn't change", {
  p <- microViz::ibd %>%
    ps_filter(abx == "abx") %>%
    tax_fix() %>%
    ps_calc_diversity("Genus", index = "shannon", exp = TRUE) %>%
    ps_calc_diversity("Family", index = "inverse_simpson") %>%
    tax_transform(rank = "Genus", trans = "clr") %>%
    ord_calc("PCA") %>%
    ord_plot(
      colour = "exp_shannon_Genus", size = "inverse_simpson_Family"
    ) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::guides(
      size = ggplot2::guide_legend(order = 1),
      colour = ggplot2::guide_colorbar(order = 2)
    ) +
    ggplot2::theme_test()

  skip_if(utils::packageVersion("ggplot2") < "3.4.0") # changed legend order
  vdiffr::expect_doppelganger(title = "diversity-pca", fig = p)
})

test_that("ps_calc_diversity errors work", {
  psTest <- microViz::ibd %>%
    ps_filter(abx == "abx") %>%
    tax_filter(min_prevalence = 10) %>%
    tax_fix()

  expect_error(
    object = psTest %>% ps_calc_diversity(rank = "banana"),
    regexp = "`rank` must be the name of a valid rank:
Kingdom / Phylum / Class / Order / Family / Genus / Species
or: unique"
  )

  expect_error(
    object = psTest %>% ps_calc_diversity(rank = "Genus", index = "lol"),
    regexp = 'must be one of "inverse_simpson", "gini_simpson",'
  )

  expect_warning(
    object = psTest %>%
      ps_calc_diversity(rank = "Genus") %>%
      ps_calc_diversity(rank = "Genus"),
    regexp = "shannon_Genus is already a variable in phyloseq sample data -> OVERWRITING"
  )
})
