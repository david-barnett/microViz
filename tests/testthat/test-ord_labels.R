local_edition(3)

library(ggplot2)

# get example inflammatory bowel disease stool dataset from corncob package
data("ibd", package = "microViz")

# filter out rare taxa and clean up names etc
ibd <- ibd %>%
  ps_mutate(DiseaseState = as.factor(DiseaseState), active = as.factor(active)) %>%
  tax_filter(min_prevalence = 3) %>%
  tax_fix() %>%
  phyloseq_validate()

# calculate a centered-log-ratio transformed PCA ordination
ibd_ord <- ibd %>%
  tax_transform("clr", rank = "Genus") %>%
  ord_calc("PCA")

# example plot #
p1 <- ibd_ord %>%
  ord_plot(
    shape = "circle filled", fill = "ibd",
    plot_taxa = 1:10,
    taxon_renamer = function(x) stringr::str_replace_all(x, "_", " "),
    tax_vec_length = 2, tax_lab_length = 2.1,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, size = 2.5,
      fontface = "bold.italic", check_overlap = TRUE
    )
  ) +
  coord_fixed(1, clip = "off", xlim = c(-5, 5)) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.2),
    legend.background = element_rect()
  ) +
  stat_chull(mapping = aes(colour = ibd, fill = ibd), alpha = 0.1)

test_that("ord_labels/tax_vec_style help page example stays the same", {
  skip_if(packageVersion("ggplot2") < "3.4.0")
  vdiffr::expect_doppelganger("unconstrained rotated labels", fig = p1)
})


# visually check ordination vignette custom constrained plot stays the same
# slightly different data than in the vignette as filtering applied here only
p2 <- ibd %>%
  ps_mutate(
    IBD = as.numeric(ibd == "ibd"),
    Female = as.numeric(gender == "female"),
    Abx. = as.numeric(abx == "abx")
  ) %>%
  tax_transform("clr", rank = "Genus") %>%
  ord_calc(
    constraints = c("IBD", "Female", "Abx."),
    method = "RDA",
    scale_cc = FALSE # doesn't make a difference
  ) %>%
  ord_plot(
    colour = "DiseaseState", size = 2, alpha = 0.5, shape = "active",
    auto_caption = NA, plot_taxa = 1:8,
    taxon_renamer = function(x) {
      stringr::str_replace(
        string = x, pattern = "_", replacement = " "
      )
    },
    tax_vec_length = 4.5, tax_lab_length = 4.6,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, fontface = "bold.italic"
    ),
    constraint_vec_style = vec_constraint(linewidth = 1.5, alpha = 0.5),
    constraint_vec_length = 3, constraint_lab_length = 3.3,
    constraint_lab_style = constraint_lab_style(
      alpha = 0.8, size = 3, max_angle = 90, perpendicular = TRUE
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-6, 6)) +
  scale_colour_manual(
    breaks = c("CD", "IBDundef", "nonIBD", "UC"),
    values = c(CD = "red", UC = "orange", IBDundef = "purple", nonIBD = "green")
  ) +
  scale_shape_manual(values = c(
    active = "circle", mild = "circle cross",
    inactive = "circle open", control = "square open"
  )) +
  # needs manual specification of legend order and order of colour levels
  # for maintaining uniformity across operating systems!
  guides(colour = guide_legend(order = 1), shape = guide_legend(order = 2))


test_that("ordination vignette custom constrained plot stays the same", {
  skip_if(packageVersion("ggplot2") < "3.4.0")
  vdiffr::expect_doppelganger("constrained custom labels", fig = p2)
})
