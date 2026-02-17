# Select phyloseq sample_data using dplyr::select syntax

Simple selection of phyloseq sample_data variables, might be useful for
printing reduced sample_data, or inside other functions

## Usage

``` r
ps_select(ps, ...)
```

## Arguments

- ps:

  phyloseq with sample_data

- ...:

  passed straight to dplyr::select

## Value

phyloseq object

## Examples

``` r
library(phyloseq)
library(dplyr)
data("enterotype", package = "phyloseq")

head(sample_data(enterotype))
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#>           Age ClinicalStatus
#> AM.AD.1    28        healthy
#> AM.AD.2    37        healthy
#> AM.F10.T1  NA          obese
#> AM.F10.T2  NA          obese
#> DA.AD.1    59        healthy
#> DA.AD.1T   NA           <NA>

enterotype %>%
  ps_select(!contains("Sample")) %>%
  sample_data() %>%
  head()
#>           Enterotype SeqTech     Project Nationality Gender Age ClinicalStatus
#> AM.AD.1         <NA>  Sanger      gill06    american      F  28        healthy
#> AM.AD.2         <NA>  Sanger      gill06    american      M  37        healthy
#> AM.F10.T1       <NA>  Sanger turnbaugh09    american      F  NA          obese
#> AM.F10.T2          3  Sanger turnbaugh09    american      F  NA          obese
#> DA.AD.1            2  Sanger     MetaHIT      danish      F  59        healthy
#> DA.AD.1T        <NA>  Sanger        <NA>        <NA>   <NA>  NA           <NA>
```
