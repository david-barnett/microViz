# De-duplicate phyloseq samples

Use one or more variables in the sample_data to identify and remove
duplicate samples (leaving one sample per group).

**methods:**

- method = "readcount" keeps the one sample in each duplicate group with
  the highest total number of reads (phyloseq::sample_sums)

- method = "first" keeps the first sample in each duplicate group
  encountered in the row order of the sample_data

- method = "last" keeps the last sample in each duplicate group
  encountered in the row order of the sample_data

- method = "random" keeps a random sample from each duplicate group
  (set.seed for reproducibility)

More than one "duplicate" sample can be kept per group by setting `n`
samples \> 1.

## Usage

``` r
ps_dedupe(
  ps,
  vars,
  method = "readcount",
  verbose = TRUE,
  n = 1,
  .keep_group_var = FALSE,
  .keep_readcount = FALSE,
  .message_IDs = FALSE,
  .label_only = FALSE,
  .keep_all_taxa = FALSE
)
```

## Arguments

- ps:

  phyloseq object

- vars:

  names of variables, whose (combined) levels identify groups from which
  only 1 sample is desired

- method:

  keep sample with max "readcount" or the "first" or "last" or "random"
  samples encountered in given sample_data order for each duplicate
  group

- verbose:

  message about number of groups, and number of samples dropped?

- n:

  number of 'duplicates' to keep per group, defaults to 1

- .keep_group_var:

  keep grouping variable .GROUP. in phyloseq object?

- .keep_readcount:

  keep readcount variable .READCOUNT. in phyloseq object?

- .message_IDs:

  message sample names of dropped variables?

- .label_only:

  if TRUE, the samples will NOT be filtered, just labelled with a new
  logical variable .KEEP_SAMPLE.

- .keep_all_taxa:

  keep all taxa after removing duplicates? If FALSE, the default, taxa
  are removed if they never occur in any of the retained samples

## Value

phyloseq object

## Details

What happens when duplicated samples have exactly equal readcounts in
method = "readcount"? The first encountered maximum is kept (in
sample_data row order, like method = "first")

## See also

[`ps_filter`](https://david-barnett.github.io/microViz/reference/ps_filter.md)
for filtering samples by sample_data variables

## Examples

``` r
data("dietswap", package = "microbiome")

dietswap
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
# let's pretend the dietswap data contains technical replicates from each subject
# we want to keep only one of them
ps_dedupe(dietswap, vars = "subject", method = "readcount", verbose = TRUE)
#> 38 groups: with 4 to 6 samples each
#> Dropped 184 samples.
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 120 taxa and 38 samples ]
#> sample_data() Sample Data:       [ 38 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 120 taxa by 3 taxonomic ranks ]

# contrived example to show identifying "duplicates" via the interaction of multiple columns
ps1 <- ps_dedupe(
  ps = dietswap, method = "readcount", verbose = TRUE,
  vars = c("timepoint", "group", "bmi_group")
)
#> 18 groups: with 8 to 15 samples each
#> Dropped 204 samples.
phyloseq::sample_data(ps1)
#>            subject    sex nationality group     sample timepoint
#> Sample-2       nms   male         AFR    HE   Sample-2         2
#> Sample-3       olt   male         AFR    HE   Sample-3         2
#> Sample-11      olt   male         AFR    HE  Sample-11         3
#> Sample-12      pku female         AFR    HE  Sample-12         3
#> Sample-17      ufm   male         AFR    HE  Sample-17         3
#> Sample-20      pku female         AFR    DI  Sample-20         4
#> Sample-25      ufm   male         AFR    DI  Sample-25         4
#> Sample-28      pku female         AFR    DI  Sample-28         5
#> Sample-38      nmz   male         AAM    ED  Sample-38         1
#> Sample-46      mni female         AAM    ED  Sample-46         6
#> Sample-54      dwc   male         AFR    ED  Sample-54         6
#> Sample-77      zaq female         AFR    DI  Sample-77         4
#> Sample-101     zaq female         AFR    DI Sample-101         5
#> Sample-143     kpp   male         AAM    DI Sample-143         5
#> Sample-156     gtd   male         AAM    HE Sample-156         2
#> Sample-208     olt   male         AFR    ED Sample-208         1
#> Sample-214     ufm   male         AFR    ED Sample-214         1
#> Sample-217     pku female         AFR    ED Sample-217         6
#>            timepoint.within.group  bmi_group
#> Sample-2                        1       lean
#> Sample-3                        1 overweight
#> Sample-11                       2 overweight
#> Sample-12                       2      obese
#> Sample-17                       2       lean
#> Sample-20                       1      obese
#> Sample-25                       1       lean
#> Sample-28                       2      obese
#> Sample-38                       1      obese
#> Sample-46                       2 overweight
#> Sample-54                       2       lean
#> Sample-77                       1 overweight
#> Sample-101                      2 overweight
#> Sample-143                      2       lean
#> Sample-156                      1      obese
#> Sample-208                      1 overweight
#> Sample-214                      1       lean
#> Sample-217                      2      obese

ps2 <- ps_dedupe(
  ps = dietswap, method = "first", verbose = TRUE,
  vars = c("timepoint", "group", "bmi_group")
)
#> 18 groups: with 8 to 15 samples each
#> Dropped 204 samples.
phyloseq::sample_data(ps2)
#>           subject    sex nationality group    sample timepoint
#> Sample-1      byn   male         AAM    DI  Sample-1         4
#> Sample-2      nms   male         AFR    HE  Sample-2         2
#> Sample-3      olt   male         AFR    HE  Sample-3         2
#> Sample-4      pku female         AFR    HE  Sample-4         2
#> Sample-10     nms   male         AFR    HE Sample-10         3
#> Sample-11     olt   male         AFR    HE Sample-11         3
#> Sample-12     pku female         AFR    HE Sample-12         3
#> Sample-18     nms   male         AFR    DI Sample-18         4
#> Sample-19     olt   male         AFR    DI Sample-19         4
#> Sample-26     nms   male         AFR    DI Sample-26         5
#> Sample-27     olt   male         AFR    DI Sample-27         5
#> Sample-28     pku female         AFR    DI Sample-28         5
#> Sample-32     azh female         AAM    ED Sample-32         1
#> Sample-35     kpp   male         AAM    ED Sample-35         1
#> Sample-36     lot female         AAM    ED Sample-36         1
#> Sample-41     azh female         AAM    ED Sample-41         6
#> Sample-44     kpp   male         AAM    ED Sample-44         6
#> Sample-45     lot female         AAM    ED Sample-45         6
#>           timepoint.within.group  bmi_group
#> Sample-1                       1      obese
#> Sample-2                       1       lean
#> Sample-3                       1 overweight
#> Sample-4                       1      obese
#> Sample-10                      2       lean
#> Sample-11                      2 overweight
#> Sample-12                      2      obese
#> Sample-18                      1       lean
#> Sample-19                      1 overweight
#> Sample-26                      2       lean
#> Sample-27                      2 overweight
#> Sample-28                      2      obese
#> Sample-32                      1 overweight
#> Sample-35                      1       lean
#> Sample-36                      1      obese
#> Sample-41                      2 overweight
#> Sample-44                      2       lean
#> Sample-45                      2      obese
```
