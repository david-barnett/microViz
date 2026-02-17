# Easy palettes for ComplexHeatmap

Pass a named colorspace hcl palette to circlize::colorRamp2.

- If you do not specify a range this function returns a function and the
  heatmap color palette will use the range of the data automatically

- If you do specify a range, this returns a colour palette with that
  range

## Usage

``` r
heat_palette(
  palette = ifelse(sym, "Blue-Red 3", "Rocket"),
  breaks = "auto",
  range = NA,
  sym = FALSE,
  rev = FALSE
)
```

## Arguments

- palette:

  named palette from colorspace::hcl_palettes() diverging/sequential or
  a vector of colour names/hexcodes

- breaks:

  number of breaks, "auto" is 11 for a named palette, or uses palette
  length

- range:

  NA to return palette generating function that takes range or numeric
  vector indicating the range, to return a palette

- sym:

  makes palette range symmetrical around 0 if TRUE

- rev:

  reverse the palette?

## Value

circlize::colorRamp2 palette if range = NA, or function returning a
palette when given a range
