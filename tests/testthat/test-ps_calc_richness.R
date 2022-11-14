library(dplyr)

test_that("microbiome chao1 results don't change", {
  local_edition(3)

  skip_if(packageVersion("microbiome") < 1.16)

  # see https://github.com/microbiome/microbiome/issues/150
  # change seems to occur between bioconductor 3.13 and 3.14
  ps <-
    corncob::ibd_phylo %>%
    ps_filter(abx == "abx") %>%
    tax_fix() %>%
    tax_agg("Family") %>%
    ps_get()

  testthat::expect_snapshot_output(microbiome::richness(ps, "chao1"))
})


test_that("ps_calc_richness results don't change", {
  local_edition(3)

  skip_if(packageVersion("microbiome") < 1.16)

  ps <-
    corncob::ibd_phylo %>%
    ps_filter(abx == "abx") %>%
    tax_fix() %>%
    ps_calc_richness("Genus", index = "observed") %>%
    ps_calc_richness("Family", index = "chao1")

  testthat::expect_snapshot_output(
    phyloseq::sample_data(ps)[, c("observed_Genus", "chao1_Family")]
  )
})

#
test_that("ps_calc_richness supported plot doesn't change", {
  skip_if(packageVersion("microbiome") < 1.16)

  p <- corncob::ibd_phylo %>%
    ps_filter(abx == "abx") %>%
    tax_fix() %>%
    ps_calc_richness("Genus", index = "observed") %>%
    ps_calc_richness("Family", index = "chao1") %>%
    tax_transform(rank = "Genus", trans = "clr") %>%
    ord_calc("PCA") %>%
    ord_plot(
      size = "observed_Genus", colour = "chao1_Family"
    ) +
    ggplot2::scale_radius(range = c(1, 6)) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::theme_test()

  skip_if(packageVersion("ggplot2") < "3.4.0")
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
