# Deselect phyloseq samples with sample_data missings

Check phyloseq object sample_data for missing values (NAs)

- specify which variables to check with vars argument, or check all

- drop samples with any missings

## Usage

``` r
ps_drop_incomplete(ps, vars = NA, verbose = FALSE)
```

## Arguments

- ps:

  phyloseq with sample_data

- vars:

  vector of variable names to check for missings (or NA, which uses all
  variables in sample data)

- verbose:

  message about number of samples dropped if verbose not FALSE, (and
  only if \> 0 samples dropped) and message about number of missing per
  variable in vars if verbose = "max" (and message even if 0 samples
  dropped)

## Value

phyloseq

## Details

This is a wrapper for
[`stats::complete.cases`](https://rdrr.io/r/stats/complete.cases.html)
function.

## See also

[`ps_filter`](https://david-barnett.github.io/microViz/reference/ps_filter.md)

## Examples

``` r
library(phyloseq)
data("enterotype", package = "phyloseq")

enterotype
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
#> sample_data() Sample Data:       [ 280 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
ps_drop_incomplete(enterotype)
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 31 samples ]
#> sample_data() Sample Data:       [ 31 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
ps_drop_incomplete(enterotype, vars = "Enterotype", verbose = TRUE)
#> Dropping samples with missings: 9
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 271 samples ]
#> sample_data() Sample Data:       [ 271 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
ps_drop_incomplete(enterotype, vars = "Sample_ID", verbose = TRUE)
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
#> sample_data() Sample Data:       [ 280 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
ps_drop_incomplete(enterotype, vars = c("Enterotype", "Sample_ID"))
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 271 samples ]
#> sample_data() Sample Data:       [ 271 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
ps_drop_incomplete(enterotype, verbose = "max")
#> Dropping samples with missings: 249
#> Enterotype has NAs: 9
#> SampleID has NAs: 241
#> Project has NAs: 241
#> Nationality has NAs: 241
#> Gender has NAs: 241
#> Age has NAs: 243
#> ClinicalStatus has NAs: 241
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 31 samples ]
#> sample_data() Sample Data:       [ 31 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
```
