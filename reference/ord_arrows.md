# Create ordination plot vector styling lists

Used by ord_plot, see examples there.

## Usage

``` r
vec_constraint(
  linewidth = 1,
  alpha = 0.8,
  colour = "brown",
  arrow = grid::arrow(length = grid::unit(0.005, units = "npc"), type = "closed", angle =
    30),
  lineend = "round",
  linejoin = "mitre",
  ...
)

vec_tax_sel(
  linewidth = 0.5,
  alpha = 1,
  colour = "black",
  arrow = grid::arrow(length = grid::unit(0.005, units = "npc"), type = "closed", angle =
    30),
  lineend = "round",
  linejoin = "mitre",
  ...
)

vec_tax_all(linewidth = 0.5, alpha = 0.25, arrow = NULL, ...)
```

## Arguments

- linewidth:

  width of vector

- alpha:

  opacity of vector

- colour:

  colour of vector

- arrow:

  arrow style specified with grid::arrow() or NULL for no arrow

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- ...:

  further arguments passed to geom_segment

## Value

list
