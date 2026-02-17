# Convenience function for generating a legend for anno_cat annotations.

Convenience function for generating a legend for anno_cat annotations.

## Usage

``` r
anno_cat_legend(col, x = NULL, renamer = identity, title = "", ...)
```

## Arguments

- col:

  vector of colors, named by all levels of data (e.g. x) or not named

- x:

  optional: vector of data to pair with unnamed col or check against
  named col

- renamer:

  function applied to generate labels: from names(col) or levels of x

- title:

  title of legend

- ...:

  Arguments passed on to
  [`ComplexHeatmap::Legend`](https://rdrr.io/pkg/ComplexHeatmap/man/Legend.html)

  `labels`

  :   Labels corresponding to `at`. If it is not specified, the values
      of `at` are taken as labels.

  `nrow`

  :   For legend which is represented as grids, `nrow` controls number
      of rows of the grids if the grids are arranged into multiple rows.

  `ncol`

  :   Similar as `nrow`, `ncol` controls number of columns of the grids
      if the grids are arranged into multiple columns. Note at a same
      time only one of `nrow` and `ncol` can be specified.

  `by_row`

  :   Are the legend grids arranged by rows or by columns?

  `grid_height`

  :   The height of legend grid. It can also control the height of the
      continuous legend if it is horizontal.

  `grid_width`

  :   The width of legend grid. It can also control the width of the
      continuous legend if it is vertical.

  `gap`

  :   If legend grids are put into multiple rows or columns, this
      controls the gap between neighbouring rows or columns, measured as
      a [`unit`](https://rdrr.io/r/grid/unit.html) object.

  `labels_gp`

  :   Graphic parameters for labels.

  `labels_rot`

  :   Text rotation for labels. It should only be used for horizontal
      continuous legend.

  `border`

  :   Color of legend grid borders. It also works for the ticks in the
      continuous legend.

  `type`

  :   Type of legends. The value can be one of `grid`, `points`, `lines`
      and `boxplot`.

  `direction`

  :   Direction of the legend, vertical or horizontal?

  `title_position`

  :   Position of title relative to the legend. `topleft`, `topcenter`,
      `leftcenter-rot` and `lefttop-rot` are only for vertical legend
      and `leftcenter`, `lefttop` are only for horizontal legend.

  `title_gap`

  :   Gap between title and the legend body.

## Value

a ComplexHeatmap Legend class object

## Examples

``` r
grid::grid.newpage()
ComplexHeatmap::draw(
  anno_cat_legend(
    col = c("ibd" = "blue", "nonibd" = "grey90"),
    renamer = toupper, title = "Hi there, I'm a title"
  )
)
```
