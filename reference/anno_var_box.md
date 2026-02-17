# Helper to specify heatmap annotation for variable distribution boxplots

Use this as an argument to varAnnotation(), which itself is used by
cor_heatmap as var_anno() argument.

## Usage

``` r
anno_var_box(
  fun = identity,
  size = grid::unit(30, "mm"),
  border = TRUE,
  gp = grid::gpar(fill = "#CCCCCC"),
  ylim = NULL,
  extend = 0.05,
  outline = TRUE,
  box_width = 0.6,
  pch = 1,
  pointsize = grid::unit(0.5, "mm"),
  axis = TRUE,
  ...,
  data = NULL,
  vars = NULL,
  which = NULL
)
```

## Arguments

- fun:

  function applied to all variables, with apply()

- size:

  width or height as a grid unit object

- border:

  Wether draw borders of the annotation region?

- gp:

  Graphic parameters for the boxes. The length of the graphic parameters
  should be one or the number of observations.

- ylim:

  Data ranges.

- extend:

  The extension to both side of `ylim`. The value is a percent value
  corresponding to `ylim[2] - ylim[1]`.

- outline:

  Whether draw outline of boxplots?

- box_width:

  Relative width of boxes. The value should be smaller than one.

- pch:

  Point style.

- pointsize:

  size of outlier points, as grid::unit() object

- axis:

  Whether to add axis?

- ...:

  Arguments passed on to
  [`ComplexHeatmap::anno_boxplot`](https://rdrr.io/pkg/ComplexHeatmap/man/anno_boxplot.html)

  `axis_param`

  :   parameters for controlling axis. See
      [`default_axis_param`](https://rdrr.io/pkg/ComplexHeatmap/man/default_axis_param.html)
      for all possible settings and default parameters.

- data:

  OPTIONAL phyloseq or psExtra, only set this to override use of same
  data as in heatmap

- vars:

  OPTIONAL selection vector of variable names, only set this if
  providing data argument to override default

- which:

  OPTIONAL indicating if it is a 'column' or a 'row' annotation, only
  set this if providing data argument to override default

## Value

function or ComplexHeatmap AnnotationFunction object

## Examples

``` r
library(ComplexHeatmap)
set.seed(123)
fakeData <- as.data.frame.matrix(matrix(rnorm(500, 10, 3), ncol = 10))
names(fakeData) <- paste0("var_", 1:10)

# draw the boxplot without a heatmap, you will never normally do this!
vp <- viewport(width = 0.75, height = 0.75)
grid.newpage()
pushViewport(vp)
draw(
  anno_var_box(data = fakeData, vars = names(fakeData), which = "column")
)


grid.newpage()
pushViewport(vp)
draw(
  anno_var_box(
    data = fakeData, fun = function(x) log(x + 1),
    vars = rev(names(fakeData)),
    which = "row"
  )
)
```
