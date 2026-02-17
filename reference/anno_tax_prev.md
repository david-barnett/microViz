# Helper to specify heatmap annotation for showing taxa prevalence as barplot

Use this as an argument to taxAnnotation(), which itself is used by
cor_heatmap and comp_heatmap as tax_anno argument.

## Usage

``` r
anno_tax_prev(
  undetected = 0,
  use_counts = TRUE,
  size = grid::unit(20, "mm"),
  baseline = 0,
  border = TRUE,
  bar_width = 0.6,
  gp = grid::gpar(fill = "#CCCCCC"),
  ylim = NULL,
  extend = 0.05,
  axis = TRUE,
  ...,
  data = NULL,
  taxa = NULL,
  which = NULL
)
```

## Arguments

- undetected:

  the value above which taxa are classed as detected/present in a sample

- use_counts:

  try to retrieve counts from data object?

- size:

  width or height as a grid unit object

- baseline:

  baseline of bars. The value should be "min" or "max", or a numeric
  value. It is enforced to be zero for stacked barplots.

- border:

  Wether draw borders of the annotation region?

- bar_width:

  Relative width of the bars. The value should be smaller than one.

- gp:

  Graphic parameters for bars. The length of each graphic parameter can
  be 1, length of `x` if `x` is a vector, or number of columns of `x` is
  `x` is a matrix.

- ylim:

  Data ranges. By default it is `range(x)` if `x` is a vector, or
  `range(rowSums(x))` if `x` is a matrix.

- extend:

  The extension to both side of `ylim`. The value is a percent value
  corresponding to `ylim[2] - ylim[1]`.

- axis:

  Whether to add axis?

- ...:

  Arguments passed on to
  [`ComplexHeatmap::anno_barplot`](https://rdrr.io/pkg/ComplexHeatmap/man/anno_barplot.html)

  `axis_param`

  :   parameters for controlling axis. See
      [`default_axis_param`](https://rdrr.io/pkg/ComplexHeatmap/man/default_axis_param.html)
      for all possible settings and default parameters.

- data:

  OPTIONAL phyloseq or psExtra, only set this to override use of same
  data as in heatmap

- taxa:

  OPTIONAL selection vector of taxa (names, numbers or logical), only
  set this if providing data argument to override default

- which:

  OPTIONAL indicating if it is a 'column' or a 'row' annotation, only
  set this if providing data argument to override default

## Value

function or ComplexHeatmap AnnotationFunction object

## Examples

``` r
library("ComplexHeatmap")
data("ibd", package = "microViz")
psq <- tax_filter(ibd, min_prevalence = 5)
psq <- tax_mutate(psq, Species = NULL)
psq <- tax_fix(psq)
psq <- tax_agg(psq, rank = "Family")
taxa <- tax_top(psq, n = 15, rank = "Family")

# makes a function that takes data, taxa and which (at minimum)
fun <- anno_tax_prev()

# manually specify the prevalence barplot function by giving it data etc.
heatmapAnnoFunction <- fun(data = psq, which = "row", taxa = taxa)

# draw the barplot without a heatmap, you will never normally do this!
vp <- viewport(width = 0.75, height = 0.75)

grid::grid.newpage()
pushViewport(vp)
draw(heatmapAnnoFunction)


# let's change some style options and specify the data up front
grid::grid.newpage()
pushViewport(vp)
anno_tax_prev(
  data = psq, taxa = taxa, which = "column",
  gp = grid::gpar(fill = "red", lwd = 3, alpha = 0.5),
  border = FALSE, bar_width = 1
) %>%
  draw()


# clear drawings
grid::grid.newpage()
```
