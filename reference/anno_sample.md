# Helper to specify simple comp_heatmap annotation for other sample data

Use this as an argument to sampleAnnotation(), which itself is used by
comp_heatmap() as sample_anno argument.

This creates a vector, which sampleAnnotation() interprets as a simple
annotation, so then you set colours and legend parameters for each
simple annotation as further arguments in sampleAnnotation.

## Usage

``` r
anno_sample(var, fun = identity, data = NULL, samples = NULL)
```

## Arguments

- var:

  name of variable to use for annotation data

- fun:

  function to transform variable `var`

- data:

  OPTIONAL phyloseq or psExtra, only set this to override use of same
  data as in heatmap

- samples:

  OPTIONAL selection vector of sample names, only set this if providing
  data argument to override default

## Value

vector of values

## See also

[`sampleAnnotation()`](https://david-barnett.github.io/microViz/reference/sampleAnnotation.md)

## Examples

``` r
# see `?sampleAnnotation()`
```
