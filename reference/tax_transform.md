# Transform taxa in phyloseq object and record transformation

Transform taxa features, and optionally aggregate at specified taxonomic
rank beforehand. You can pipe the results of `tax_agg` into
`tax_transform`, or equivalently set the rank argument in
`tax_transform`.

## Usage

``` r
tax_transform(
  data,
  trans,
  rank = NA,
  keep_counts = TRUE,
  chain = FALSE,
  zero_replace = 0,
  add = 0,
  transformation = NULL,
  ...
)
```

## Arguments

- data:

  a phyloseq object or psExtra output from `tax_agg`

- trans:

  any valid taxa transformation (e.g. from
  [`microbiome::transform`](https://rdrr.io/pkg/microbiome/man/transform.html))

- rank:

  If data is phyloseq: data are aggregated at this rank before
  transforming. If NA, runs tax_agg(data, rank = NA). If rank is NA and
  data is already psExtra, any preceding aggregation is left as is.

- keep_counts:

  if TRUE, store the pre-transformation count data in psExtra counts
  slot

- chain:

  if TRUE, transforming again is possible when data are already
  transformed i.e. multiple transformations can be chained with multiple
  tax_transform calls

- zero_replace:

  Replace any zeros with this value before transforming. Either a
  numeric, or "halfmin" which replaces zeros with half of the smallest
  value across the entire dataset. Beware: the choice of zero
  replacement is not tracked in the psExtra output.

- add:

  Add this value to the otu_table before transforming. If `add` != 0,
  `zero_replace` does nothing. Either a numeric, or "halfmin". Beware:
  this choice is not tracked in the psExtra output.

- transformation:

  deprecated, use `trans` instead!

- ...:

  any extra arguments passed to
  [`microbiome::transform`](https://rdrr.io/pkg/microbiome/man/transform.html)
  or pass undetected = `a number` when using trans = "binary"

## Value

`psExtra` object including phyloseq and extra info

## Details

This function often uses
[`microbiome::transform`](https://rdrr.io/pkg/microbiome/man/transform.html)
internally and can perform the same transformations, including many from
[`vegan::decostand`](https://vegandevs.github.io/vegan/reference/decostand.html)
(where the default MARGIN = 2). See below for notes about some of the
available transformations.

`tax_transform` returns a `psExtra` containing the transformed phyloseq
object and extra info (used for annotating `ord_plot` ordinations):

- tax_transform (a string recording the transformation),

- tax_agg (a string recording the taxonomic aggregation rank if
  specified here or earlier in `tax_agg`).

A few commonly used transformations:

- "clr", or "rclr", perform the centered log ratio transformation, or
  the robust clr, using
  [`microbiome::transform`](https://rdrr.io/pkg/microbiome/man/transform.html)

- "compositional" converts the data into proportions, from 0 to 1.

- "identity" does not transform the data, and records this choice for
  `ord_plot`

- "binary" can be used to transform tax abundances into
  presence/abundance data.

- "log2" which performs a log base 2 transformation (don't forget to set
  zero_replace if there are any zeros in your data)

## (r)clr transformation note

If any values are zero, the clr transform routine first adds a small
pseudocount of min(abundance)/2 to all values. To avoid this, you can
replace any zeros in advance by setting zero_replace to a number \> 0.

The comp_clr is provided for legacy reasons to replicate the behavior of
microbiome::transform prior to microbiome version 1.32.0 (BioC 3.22 Oct
2025). It first performs a compositional transform, then adds half the
minimum value (i.e. now as a proportion) to all entries (if any zeroes
were present) and finally performs the clr transform.

The rclr is similar to regular clr, but it allows data with zeroes. See
the vegan::decostand documentation for more details. rclr results may
vary depending on the version of vegan installed.

## Binary transformation notes

By default, otu_table values of 0 are kept as 0, and all positive values
are converted to 1 (like `decostand(method = "pa")`). You can set a
different threshold, by passing e.g. undetected = 10, for example, in
which case all abundances of 10 or below would be converted to 0. All
abundances above 10 would be converted to 1s.

Beware that the choice of detection threshold is not tracked in the
psExtra.

## See also

`microbiome::`[`transform`](https://rdrr.io/pkg/microbiome/man/transform.html)
for some more info on available transformations

`vegan::`[`decostand`](https://vegandevs.github.io/vegan/reference/decostand.html)
for even more transformation options

[`tax_agg`](https://david-barnett.github.io/microViz/reference/tax_agg.md)

## Examples

``` r
data("dietswap", package = "microbiome")

# aggregate taxa at Phylum level and center log ratio transform the phyla counts
tax_transform(dietswap, trans = "clr", rank = "Phylum")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 8 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 8 taxa by 1 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 8 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Phylum" tax_trans = "clr" 

# this is equivalent to the two-step method (agg then transform)
tax_agg(dietswap, rank = "Phylum") %>% tax_transform("clr")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 8 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 8 taxa by 1 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 8 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Phylum" tax_trans = "clr" 

# does nothing except record tax_agg as "unique" and tax_transform as "identity" in psExtra info
dietswap %>% tax_transform("identity", rank = NA)
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 4 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "unique" tax_trans = "identity" 

# binary transformation (convert abundances to presence/absence or detected/undetected)
tax_transform(dietswap, trans = "binary", rank = "Genus")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 130 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Genus" tax_trans = "binary" 
# change detection threshold by setting undetected argument (default is 0)
tax_transform(dietswap, trans = "binary", rank = "Genus", undetected = 50) %>%
  otu_get() %>%
  .[1:6, 1:4]
#> OTU Table:          [4 taxa and 6 samples]
#>                      taxa are columns
#>          Actinomycetaceae Aerococcus Aeromonas Akkermansia
#> Sample-1                0          0         0           0
#> Sample-2                0          0         0           1
#> Sample-3                0          0         0           1
#> Sample-4                0          0         0           1
#> Sample-5                0          0         0           0
#> Sample-6                0          0         0           0

# log2 transformation after replacing all zeros with a pseudocount of 1
tax_transform(dietswap, trans = "log2", rank = "Family", zero_replace = 1)
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 22 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 22 taxa by 2 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 22 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Family" tax_trans = "log2" 

# log2 transformation after replacing all zeros with a pseudocount of half
# the minimum non-zero count value in the aggregated dataset
tax_transform(dietswap, trans = "log2", rank = "Family", zero_replace = "halfmin")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 22 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 22 taxa by 2 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 22 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Family" tax_trans = "log2" 
```
