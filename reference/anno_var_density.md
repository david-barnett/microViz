# Helper to specify heatmap annotation for variable distribution density plot

Use this as an argument to varAnnotation(), which itself is used by
cor_heatmap var_anno argument.

## Usage

``` r
anno_var_density(
  fun = identity,
  size = grid::unit(30, "mm"),
  type = c("lines", "violin", "heatmap"),
  xlim = NULL,
  heatmap_colors = c("white", "forestgreen"),
  joyplot_scale = 1.5,
  border = TRUE,
  gp = grid::gpar(fill = "lightgrey"),
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

- type:

  Type of graphics to represent density distribution. "lines" for normal
  density plot; "violine" for violin plot and "heatmap" for heatmap
  visualization of density distribution.

- xlim:

  Range on x-axis.

- heatmap_colors:

  A vector of colors for interpolating density values.

- joyplot_scale:

  Relative height of density distribution. A value higher than 1
  increases the height of the density distribution and the plot will
  represented as so-called "joyplot".

- border:

  Wether draw borders of the annotation region?

- gp:

  Graphic parameters for the boxes. The length of the graphic parameters
  should be one or the number of observations.

- axis:

  Whether to add axis?

- ...:

  Arguments passed on to
  [`ComplexHeatmap::anno_density`](https://rdrr.io/pkg/ComplexHeatmap/man/anno_density.html)

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

# draw the plots without a heatmap, you will never normally do this!
vp <- viewport(width = 0.75, height = 0.75)
grid.newpage()
pushViewport(vp)
draw(
  anno_var_density(data = fakeData, vars = names(fakeData), which = "row")
)


grid.newpage()
pushViewport(vp)
draw(
  anno_var_density(
    data = fakeData, fun = function(x) log(x + 1),
    vars = rev(names(fakeData)), type = "heatmap",
    which = "column"
  )
)
```
