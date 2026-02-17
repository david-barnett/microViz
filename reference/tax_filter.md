# Filter rare and/or low abundance taxa from a phyloseq object

Removes taxa (from all samples) that do not meet a given criterion or
combination of criteria. If a value for min_prevalence,
min_total_abundance or min_sample_abundance is 1 or greater, then it is
treated as an absolute minimum number of samples/reads. If \<1, it is
treated as proportion of all samples/reads. This function is designed to
work with counts. otu_table must contain counts particularly if you want
to set a non-zero value for min_total_abundance.

## Usage

``` r
tax_filter(
  ps,
  min_prevalence = 1,
  prev_detection_threshold = 1,
  min_total_abundance = 0,
  min_sample_abundance = 0,
  tax_level = NA,
  names_only = FALSE,
  use_counts = TRUE,
  undetected = NULL,
  verbose = TRUE
)
```

## Arguments

- ps:

  phyloseq or psExtra (ideally with count data available)

- min_prevalence:

  number or proportion of samples that a taxon must be present in

- prev_detection_threshold:

  min required counts (or value) for a taxon to be considered present in
  that sample (or set undetected arg)

- min_total_abundance:

  minimum total readcount of a taxon, summed across all samples (can be
  proportion of all counts)

- min_sample_abundance:

  taxa must have at least this many reads in one or more samples

- tax_level:

  if given, aggregates data at named taxonomic rank before filtering,
  but returns phyloseq at the ORIGINAL level of aggregation!

- names_only:

  if names_only is true return only names of taxa, not the phyloseq

- use_counts:

  expect count data in phyloseq otu_table? default is TRUE

- undetected:

  e.g. 0, value at (or below) which a taxon is considered not present in
  that sample. If set, this overrides prev_detection_threshold.

- verbose:

  message about proportional prevalence calculations?

## Value

filtered phyloseq object AT ORIGINAL LEVEL OF AGGREGATION (not at the
level in tax_level)

## Examples

``` r
data("dietswap", package = "microbiome")
# Dropping rare and low abundance taxa #
# Filter at unique taxa level, keeping only those with a prevalence of 10% or more
# and at least 10 thousand reads when summed across all samples.
# Then aggregate to Family level taxonomy.
dietswap %>%
  tax_filter(min_prevalence = 0.1, min_total_abundance = 10000) %>%
  tax_agg("Family")
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 9 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 9 taxa by 2 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Family" 

# Keeping ubiquitous families #
# keep only families that have at least 1000 counts present in 90% of samples
# then aggregate the remaining taxa at 'Genus' level
dietswap %>%
  tax_filter(
    tax_level = "Family", min_prevalence = 0.90,
    prev_detection_threshold = 1000
  ) %>%
  tax_agg("Genus")
#> Proportional min_prevalence given: 0.9 --> min 200/222 samples.
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 28 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 28 taxa by 3 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Genus" 
```
