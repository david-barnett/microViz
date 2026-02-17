# Create list for ord_plot() \*\_lab_style arguments

Customise taxa and constraint labels on your ordination plots. Choose
'text' or 'label' type, rotate and/or justify the text/labels and set
aesthetic appearances using `tax_lab_style()` or
`constraint_lab_style()`.

## Usage

``` r
tax_lab_style(
  type = "label",
  max_angle = 0,
  perpendicular = FALSE,
  aspect_ratio = 1,
  justify = "auto",
  size = 2,
  alpha = 1,
  colour = "black",
  ...
)

constraint_lab_style(
  type = "label",
  max_angle = 0,
  perpendicular = FALSE,
  aspect_ratio = 1,
  justify = "auto",
  size = 2.5,
  alpha = 1,
  colour = "brown",
  ...
)
```

## Arguments

- type:

  'label', 'text' or 'richtext' ('richtext' also used if 'label' type
  are rotated, when max_angle \> 0)

- max_angle:

  maximum angle of rotation to allow to match vector angle (requires
  ggtext package to rotate "label" type)

- perpendicular:

  if TRUE, sets rotated labels perpendicular to desired angle, not
  parallel

- aspect_ratio:

  aspect ratio of plot (y/x) must also be used in coord_fixed() ratio
  argument (must be set when rotated labels are used, to ensure match to
  arrow angles)

- justify:

  "center", "side", or "auto"? Should the text/label align with the
  arrows at the text center or text sides (uses hjust, if 'auto', picks
  based on whether max_angle is greater than 0)

- size:

  fixed size of text or label

- alpha:

  fixed alpha of text or label

- colour:

  fixed colour of text or label

- ...:

  further named arguments passed to geom_text, geom_label or
  geom_richtext

## Value

named list

## Examples

``` r
# These examples show styling of taxa labels with tax_lab_style().
# The same options are available for constraint labels in constrained
# ordinations. constraint_lab_style() just has different default settings.

library(ggplot2)

# get example inflammatory bowel disease stool dataset from corncob package
data("ibd", package = "microViz")

# filter out rare taxa and clean up names etc
ibd <- ibd %>%
  tax_filter(min_prevalence = 3) %>%
  tax_fix() %>%
  phyloseq_validate()

# calculate a centered-log-ratio transformed PCA ordination
ibd_ord <- ibd %>%
  tax_transform("clr", rank = "Genus") %>%
  ord_calc("PCA")

# basic plot with default label style
ibd_ord %>% ord_plot(color = "ibd", plot_taxa = 1:10)


# Rotating labels: requires the ggtext package #
# A fixed coordinate ratio must be set to ensure label rotation
# matches the vectors. It is also helpful to set the vector and label length
# multipliers manually for a good look. Rotated labels are justified to the
# 'sides' automatically by tax_lab_style() with justify = 'auto'
ibd_ord %>%
  ord_plot(
    color = "ibd", plot_taxa = 1:7,
    tax_vec_length = 1.3, tax_lab_length = 1.3,
    tax_lab_style = tax_lab_style(max_angle = 90)
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


# You can use text instead of labels
# - a bold fontface helps text to stand out
# - see ?ggplot2::geom_text for all settings available
ibd_ord %>%
  ord_plot(
    color = "ibd", plot_taxa = 1:7,
    tax_vec_length = 1.3, tax_lab_length = 1.4,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, size = 2.5, fontface = "bold.italic"
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


# With text you can prevent overlaps with check_overlap = TRUE
ibd_ord %>%
  ord_plot(
    color = "ibd", plot_taxa = 1:12,
    tax_vec_length = 1.3, tax_lab_length = 1.4,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, size = 3, fontface = "bold.italic",
      check_overlap = TRUE
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


# With labels, you can reduce the padding and line weight to free space
# but check_overlap is not available
# see ?ggtext::geom_richtext for more possibilities
ibd_ord %>%
  ord_plot(
    color = "ibd", plot_taxa = 1:7,
    tax_vec_length = 1.3, tax_lab_length = 1.35,
    tax_lab_style = tax_lab_style(
      max_angle = 90, fontface = "italic", size = 2.5, fill = "grey95",
      label.size = 0.1, # width outline
      label.padding = unit(0.1, "lines"),
      label.r = unit(0, "lines") # reduces rounding of corners to radius 0
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


# Perpendicular angled labels/text are possible
ibd_ord %>%
  ord_plot(
    color = "ibd", plot_taxa = 1:12,
    tax_lab_style = tax_lab_style(
      type = "text", max_angle = 90, perpendicular = TRUE, size = 3,
      check_overlap = TRUE
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))



# You can limit and/or attenuate the angle of rotation by:
#  - setting a lower max_angle
#  - decreasing the aspect_ratio in the tax_lab_style call
ibd_ord %>%
  ord_plot(
    shape = "circle", color = "ibd", plot_taxa = 1:7,
    tax_vec_length = 1.3, tax_lab_length = 1.3,
    tax_lab_style = tax_lab_style(
      max_angle = 10, size = 2, label.size = 0.1,
      label.padding = unit(0.1, "lines"), label.r = unit(0, "lines")
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


ibd_ord %>%
  ord_plot(
    shape = "circle", color = "ibd", plot_taxa = 1:7,
    tax_vec_length = 1.3, tax_lab_length = 1.3,
    tax_lab_style = tax_lab_style(
      max_angle = 90, size = 2, label.size = 0.1, aspect_ratio = 0.5,
      label.padding = unit(0.1, "lines"), label.r = unit(0, "lines")
    )
  ) +
  coord_fixed(ratio = 1, clip = "off", xlim = c(-3.5, 3.5))


# another example with some extras #
ibd_ord %>%
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
  theme(legend.position = c(0.8, 0.2), legend.background = element_rect()) +
  stat_chull(mapping = aes(colour = ibd, fill = ibd), alpha = 0.1)
```
