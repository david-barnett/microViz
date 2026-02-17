# Calculate distances between pairs of samples in phyloseq object

Can compute various sample-sample distances using the microbiota
composition of your samples:

- Bray Curtis ('bray') or any other ecological distance from
  phyloseq::distance() / vegan::vegdist()

- UniFrac distances (using the GUniFrac package)

  - generalised: 'gunifrac' (optionally set weighting alpha in gunifrac
    alpha)

  - unweighted: 'unifrac'

  - weighted: 'wunifrac'

- Aitchison distance (Euclidean distance after centered log ratio
  transform clr, see details)

- Euclidean distance

Use dist_calc with psExtra output of tax_transform (or tax_agg). It
returns a psExtra object containing the phyloseq and the name of the
distance used in addition to the distance matrix itself. The resulting
object is intended to be piped into ord_calc or dist_permanova
functions. Alternatively you can directly access the distance matrix
with dist_get().

## Usage

``` r
dist_calc(data, dist = "bray", gunifrac_alpha = 0.5, ...)
```

## Arguments

- data:

  psExtra object, e.g. output from tax_transform()

- dist:

  name of distance to calculate between pairs of samples

- gunifrac_alpha:

  setting alpha value only relevant if gunifrac distance used

- ...:

  optional distance-specific named arguments passed to
  phyloseq::distance()

## Value

psExtra object including distance matrix and name of distance used

## Aitchison distance note

You should EITHER:

1.  skip the dist_calc function and call ord_calc(method = "PCA")
    directly on an object with taxa transformed with tax_transform(trans
    = "clr")

2.  pass an object with untransformed (or 'identity' transformed) taxa
    to the data argument of dist_calc() and specify dist = "aitchison".

If ordination plots with taxon loading vectors are desired, users
require option 1. If the distance matrix is required for permanova,
users require option 2.

## Binary Jaccard distance note

Jaccard distance can be computed on abundances, but often in microbiome
research it is the Binary Jaccard distance that is desired. So remember
to first perform a "binary" transformation with
`tax_transform("binary")`, OR pass an additional argument to
`dist_calc("jaccard", binary = TRUE)`

## See also

[`tax_transform`](https://david-barnett.github.io/microViz/reference/tax_transform.md)
for the function to use before dist_calc

[`ord_calc`](https://david-barnett.github.io/microViz/reference/ord_calc.md)

[`ord_plot`](https://david-barnett.github.io/microViz/reference/ord_plot.md)

[`dist_permanova`](https://david-barnett.github.io/microViz/reference/dist_permanova.md)

`phyloseq::`[`distance`](https://rdrr.io/pkg/phyloseq/man/distance.html)

`vegan::`[`vegdist`](https://vegandevs.github.io/vegan/reference/vegdist.html)

## Examples

``` r
# bray curtis distance on genera-level features
data("dietswap", package = "microbiome")
bc <- dietswap %>%
  tax_agg("Genus") %>%
  dist_calc("bray")
bc
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Genus" 
#> 
#> bray distance matrix of size 222 
#> 0.7639533 0.7851213 0.6680796 0.7699252 0.80507 ...
class(bc)
#> [1] "psExtra"
#> attr(,"package")
#> [1] "microViz"

# gunifrac distance using phyloseq input
data("esophagus", package = "phyloseq")
gunifrac <- esophagus %>%
  dist_calc("gunifrac") %>%
  dist_get()
class(gunifrac)
#> [1] "dist"
```
