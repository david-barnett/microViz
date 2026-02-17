# Filled shapes for ggiraph interactive plots

Generates a custom ggplot2 shape scale, as used in ord_explore's
ordination. Uses filled shapes, therefore fill aesthetic must be set, in
addition to colour, to have filled shapes. Points with NA values for the
shape variable are shown as hollow circles.

## Usage

``` r
scale_shape_girafe_filled()
```

## Value

ggplot2 Scale object

## Details

Composite shapes e.g. number 7 "square cross" cause ggiraph interactive
plots to fail when a variable shape and tooltip is set.

Shapes used are, in order: "circle filled", "triangle filled", "square
filled", "diamond filled", and "triangle down filled"

## Examples

``` r
microViz::ibd %>%
  tax_fix() %>%
  phyloseq_validate() %>%
  tax_transform(rank = "Genus", trans = "clr") %>%
  ord_calc(
    method = "PCA"
  ) %>%
  ord_plot(
    axes = c(1, 2),
    plot_taxa = 1:6,
    colour = "DiseaseState", fill = "DiseaseState",
    shape = "circle", alpha = 0.5,
    size = 3
  ) +
  scale_shape_girafe_filled()
```
