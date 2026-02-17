# Helper, removes unwanted arguments from geom\_\* call args list

Helper, removes unwanted arguments from geom\_\* call args list

## Usage

``` r
taxatreePruneLabelArgs(fun, args)
```

## Arguments

- fun:

  function name or function itself e.g. geom_label_repel

- args:

  named list of args

## Value

args list possibly with elements removed

## Examples

``` r
microViz:::taxatreePruneLabelArgs(fun = "geom_label", args = list(size = 2, seed = 2))
#> $size
#> [1] 2
#> 
microViz:::taxatreePruneLabelArgs(fun = library, args = list(size = 2, seed = 2))
#> $size
#> [1] 2
#> 
#> $seed
#> [1] 2
#> 
```
