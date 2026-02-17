# Calculate prevalence from numeric vector

Useful as helper for taxon prevalence calculation

## Usage

``` r
prev(x, undetected = 0)
```

## Arguments

- x:

  numeric vector (of taxon counts or proportions)

- undetected:

  value above which a taxon is considered present or detected

## Value

numeric value

## Examples

``` r
prev(c(0, 0, 1, 2, 4))
#> [1] 0.6
prev(c(0, 0, 1, 2, 4), undetected = 1.5)
#> [1] 0.4
```
