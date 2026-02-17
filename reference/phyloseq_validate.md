# Check for (and fix) common problems with phyloseq objects

- It checks for, and messages about, common uninformative entries in the
  tax_table, which often cause unwanted results

- If there is no sample_data, it creates a sample_data dataframe with
  the sample_names (as "SAMPLE" variable)

- If there is no tax_table, it creates a 1-column tax_table matrix with
  the taxa_names, and calls the rank "unique"

- If remove_undetected = TRUE, it removes taxa where
  [`phyloseq::taxa_sums()`](https://rdrr.io/pkg/phyloseq/man/taxa_sums.html)
  is equal to zero, with a warning

## Usage

``` r
phyloseq_validate(
  ps,
  remove_undetected = FALSE,
  min_tax_length = 4,
  verbose = TRUE
)
```

## Arguments

- ps:

  phyloseq object

- remove_undetected:

  if TRUE, removes taxa that sum to zero across all samples

- min_tax_length:

  minimum number of characters to not consider a tax_table entry
  suspiciously short

- verbose:

  print informative messages if true

## Value

possibly modified phyloseq object

## Examples

``` r
data(dietswap, package = "microbiome")

# expect warning about taxa summing to zero
phyloseq_validate(dietswap, remove_undetected = TRUE, verbose = TRUE)
#> Warning: Some taxa_sums were zero, removing the following taxa:
#>  Aerococcus 
#>  Aneurinibacillus 
#>  Asteroleplasma et rel. 
#>  Clostridium felsineum et rel. 
#>  Clostridium thermocellum et rel. 
#>  Methylobacterium 
#>  Micrococcaceae
#> This may be caused by using `subset_samples()`.
#> Try using `ps_filter()` instead, with .keep_all_taxa = FALSE.
#> Otherwise, to avoid this warning, try filtering out taxa summing to zero with `tax_filter()`.
#> If you have already transformed and/or scaled your taxa, e.g. with a log transformation or scale,
#> seeing this warning is possible, but very unlikely and possibly a bug. Please report this.
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 123 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 123 taxa by 3 taxonomic ranks ]

# verbose = FALSE will suppress messages and warnings but still:
# replace NULL sample_data and remove taxa that sum to 0 across all samples
# (if remove_undetected = TRUE)
phyloseq_validate(dietswap, verbose = FALSE)
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# Sometimes you might have a phyloseq with no sample_data
# This isn't compatible with some microViz functions, like comp_barplot
# So some functions internally use phyloseq_validate to fix this
dietswap@sam_data <- NULL
phyloseq_validate(dietswap)
#> Note: Replacing missing sample_data with a dataframe of only sample_names.
#> Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 1 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# Sometimes you might have a phyloseq with no tax_table
# This isn't compatible with some microViz functions, like tax_top,
# so this is another reason to start your analyses with phyloseq_validate!
data("soilrep", package = "phyloseq")
soilrep # has NULL tax_table
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 16825 taxa and 56 samples ]
#> sample_data() Sample Data:       [ 56 samples by 4 sample variables ]
phyloseq_validate(soilrep)
#> Note: Replacing missing tax_table with a 1-column table of only taxa_names.
#> Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 16825 taxa and 56 samples ]
#> sample_data() Sample Data:       [ 56 samples by 4 sample variables ]
#> tax_table()   Taxonomy Table:    [ 16825 taxa by 1 taxonomic ranks ]

# If no messages or warnings are emitted,
# this means no problems were detected, and nothing was changed
# (but only if verbose = TRUE)
```
