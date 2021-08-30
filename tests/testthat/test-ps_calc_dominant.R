local_edition(3)

library(ggplot2)

ps <- corncob::ibd_phylo %>%
  tax_filter(min_prevalence = 3) %>%
  tax_fix() %>%
  phyloseq_validate()

test_that("ps_calc_dominant supported plot doesn't change", {
  p <- ps %>%
    ps_calc_dominant(
      rank = "Family", other = "Other", none = "Not dominated",
      threshold = 0.4, n_max = 3
    ) %>%
    tax_transform(rank = "Genus", trans = "clr") %>%
    ord_calc("PCA") %>%
    ord_plot(colour = "dominant_Family", size = 3, alpha = 0.6) +
    scale_colour_manual(values = c(
      Bacteroidaceae = "forestgreen", Lachnospiraceae = "darkblue",
      Ruminococcaceae = "darkorange", Other = "red", "Not dominated" = "grey"
    )) +
    theme_test()

  vdiffr::expect_doppelganger(title = "dominant-taxa-pca", fig = p)
})
