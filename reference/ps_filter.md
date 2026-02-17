# Filter phyloseq samples by sample_data variables

Keep only samples with sample_data matching one or more conditions. By
default this function also removes taxa which never appear in any of the
remaining samples, by running tax_filter(min_prevalence = 1). You can
prevent this taxa filtering with .keep_all_taxa = TRUE.

## Usage

``` r
ps_filter(ps, ..., .target = "sample_data", .keep_all_taxa = FALSE)
```

## Arguments

- ps:

  phyloseq object

- ...:

  passed directly to dplyr::filter (see examples and ?dplyr::filter)

- .target:

  which slot of phyloseq to use for filtering by, currently only
  "sample_data" supported

- .keep_all_taxa:

  if FALSE (the default), remove taxa which are no longer present in the
  dataset after filtering

## Value

phyloseq object (with filtered sample_data)

## Details

Use ps_filter as you would use use dplyr::filter(), but with a phyloseq
object!

## See also

[`filter`](https://dplyr.tidyverse.org/reference/filter.html) explains
better how to give arguments to this function

[`tax_filter`](https://david-barnett.github.io/microViz/reference/tax_filter.md)
for filtering taxa (not samples)

## Examples

``` r
library(phyloseq)
library(dplyr)

data("enterotype", package = "phyloseq")
enterotype
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
#> sample_data() Sample Data:       [ 280 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]
sample_data(enterotype)[1:10, 1:5]
#>           Enterotype Sample_ID SeqTech  SampleID     Project
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>
#> DA.AD.2            3   DA.AD.2  Sanger   DA.AD.2     MetaHIT
#> DA.AD.3            3   DA.AD.3  Sanger   DA.AD.3     MetaHIT
#> DA.AD.3T        <NA>  DA.AD.3T  Sanger      <NA>        <NA>
#> DA.AD.4            2   DA.AD.4  Sanger   DA.AD.4     MetaHIT

# keep only samples with seqtech not equal to sanger
ps1 <- ps_filter(enterotype, SeqTech != "Sanger")
ps1
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 552 taxa and 239 samples ]
#> sample_data() Sample Data:       [ 239 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 552 taxa by 1 taxonomic ranks ]
sample_data(ps1)[1:10, 1:5]
#>        Enterotype Sample_ID  SeqTech SampleID Project
#> MH0001          2    MH0001 Illumina     <NA>    <NA>
#> MH0002          1    MH0002 Illumina     <NA>    <NA>
#> MH0003          1    MH0003 Illumina     <NA>    <NA>
#> MH0004          1    MH0004 Illumina     <NA>    <NA>
#> MH0005          2    MH0005 Illumina     <NA>    <NA>
#> MH0006          2    MH0006 Illumina     <NA>    <NA>
#> MH0007          1    MH0007 Illumina     <NA>    <NA>
#> MH0008          1    MH0008 Illumina     <NA>    <NA>
#> MH0009          3    MH0009 Illumina     <NA>    <NA>
#> MH0010          1    MH0010 Illumina     <NA>    <NA>

# keep only samples with no NAs in any variables
ps2 <- enterotype %>% ps_filter(!if_any(everything(), is.na))
ps2
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 221 taxa and 31 samples ]
#> sample_data() Sample Data:       [ 31 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 221 taxa by 1 taxonomic ranks ]
sample_data(ps2)[1:8, 1:8]
#>         Enterotype Sample_ID SeqTech SampleID Project Nationality Gender Age
#> DA.AD.1          2   DA.AD.1  Sanger  DA.AD.1 MetaHIT      danish      F  59
#> DA.AD.2          3   DA.AD.2  Sanger  DA.AD.2 MetaHIT      danish      M  54
#> DA.AD.3          3   DA.AD.3  Sanger  DA.AD.3 MetaHIT      danish      F  49
#> DA.AD.4          2   DA.AD.4  Sanger  DA.AD.4 MetaHIT      danish      M  59
#> ES.AD.1          1   ES.AD.1  Sanger  ES.AD.1 MetaHIT     spanish      F  25
#> ES.AD.2          2   ES.AD.2  Sanger  ES.AD.2 MetaHIT     spanish      M  49
#> ES.AD.3          2   ES.AD.3  Sanger  ES.AD.3 MetaHIT     spanish      F  47
#> ES.AD.4          3   ES.AD.4  Sanger  ES.AD.4 MetaHIT     spanish      F  38

# ps2 is equivalent to dropping samples with incomplete sample_variables and tax_filtering 0s
ps3 <- enterotype %>%
  ps_drop_incomplete() %>%
  tax_filter(undetected = 0, use_counts = FALSE)
# we needed to set a low detection threshold because this example data is proportions
identical(ps2, ps3) # TRUE
#> [1] TRUE

# function will give warning if some of the otu_values are negative
# (which may happen when filtering data that has e.g. clr-transformed taxa abundances)
# as it attempts to discard any taxa that become always absent/0 after filtering (by default)
# set .keep_all_taxa = TRUE to avoid this filtering behaviour, which is unwanted in this case
enterotype %>%
  tax_transform("clr") %>%
  ps_get() %>%
  ps_filter(SeqTech == "Sanger", .keep_all_taxa = TRUE)
#> Warning: otu_table of counts is NOT available!
#> Available otu_table contains 50166 values that are not non-negative integers
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 553 taxa and 41 samples ]
#> sample_data() Sample Data:       [ 41 samples by 9 sample variables ]
#> tax_table()   Taxonomy Table:    [ 553 taxa by 2 taxonomic ranks ]
```
