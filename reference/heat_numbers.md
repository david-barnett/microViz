# Aesthetic settings for drawing numbers on heatmap tiles

Works with comp_heatmap() and cor_heatmap(). See the help for those
functions.

## Usage

``` r
heat_numbers(
  decimals = 0,
  fontsize = 7,
  col = "darkgrey",
  fontface = "bold",
  fmt = NULL,
  ...
)
```

## Arguments

- decimals:

  number of decimal places to print

- fontsize:

  fontsize specification,

- col:

  colour of font

- fontface:

  plain, bold, italic

- fmt:

  NULL or number print format, see ?sprintf, overrides decimals arg if
  set

- ...:

  passed to grid::gpar() for grid.text

## Value

list
