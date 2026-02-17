# tax_palette plotting helper function

Check the named palette colour vector you created with tax_palette()

## Usage

``` r
tax_palette_plot(named_pal_vec, max_n = NA)
```

## Arguments

- named_pal_vec:

  vector of colours named by taxa (e.g. tax_palette output)

- max_n:

  NA to display all colours, or limit this

## Value

ggplot

## Examples

``` r
library(ggplot2)
data(dietswap, package = "microbiome")

myPal <- tax_palette(dietswap, rank = "Genus", pal = "brewerPlus", n = 40)
myPal %>% tax_palette_plot() # just to check the palette

```
