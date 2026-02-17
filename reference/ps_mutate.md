# Modify or compute new sample_data in phyloseq object

Add or compute new phyloseq sample_data variables. Uses
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
syntax.

## Usage

``` r
ps_mutate(ps, ...)
```

## Arguments

- ps:

  phyloseq object with sample data

- ...:

  passed straight to dplyr::mutate (see examples and dplyr::mutate help)

## Value

phyloseq object with modified sample_data

## See also

[`tax_mutate`](https://david-barnett.github.io/microViz/reference/tax_mutate.md)
for manipulation of tax_table variables

[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)

## Examples

``` r
library(phyloseq)
library(dplyr)
data("enterotype")

sample_data(enterotype)[1:10, ]
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#> DA.AD.2            3   DA.AD.2  Sanger   DA.AD.2     MetaHIT      danish      M
#> DA.AD.3            3   DA.AD.3  Sanger   DA.AD.3     MetaHIT      danish      F
#> DA.AD.3T        <NA>  DA.AD.3T  Sanger      <NA>        <NA>        <NA>   <NA>
#> DA.AD.4            2   DA.AD.4  Sanger   DA.AD.4     MetaHIT      danish      M
#>           Age ClinicalStatus
#> AM.AD.1    28        healthy
#> AM.AD.2    37        healthy
#> AM.F10.T1  NA          obese
#> AM.F10.T2  NA          obese
#> DA.AD.1    59        healthy
#> DA.AD.1T   NA           <NA>
#> DA.AD.2    54        healthy
#> DA.AD.3    49          obese
#> DA.AD.3T   NA           <NA>
#> DA.AD.4    59          obese

months_in_year <- 12
ps <- enterotype %>%
  ps_mutate(
    age_months = Age * months_in_year,
    IDs_match = as.character(Sample_ID) == as.character(SampleID),
    placeholder = "Word"
  )

sample_data(ps)[1:10, ]
#>           Enterotype Sample_ID SeqTech  SampleID     Project Nationality Gender
#> AM.AD.1         <NA>   AM.AD.1  Sanger   AM.AD.1      gill06    american      F
#> AM.AD.2         <NA>   AM.AD.2  Sanger   AM.AD.2      gill06    american      M
#> AM.F10.T1       <NA> AM.F10.T1  Sanger AM.F10.T1 turnbaugh09    american      F
#> AM.F10.T2          3 AM.F10.T2  Sanger AM.F10.T2 turnbaugh09    american      F
#> DA.AD.1            2   DA.AD.1  Sanger   DA.AD.1     MetaHIT      danish      F
#> DA.AD.1T        <NA>  DA.AD.1T  Sanger      <NA>        <NA>        <NA>   <NA>
#> DA.AD.2            3   DA.AD.2  Sanger   DA.AD.2     MetaHIT      danish      M
#> DA.AD.3            3   DA.AD.3  Sanger   DA.AD.3     MetaHIT      danish      F
#> DA.AD.3T        <NA>  DA.AD.3T  Sanger      <NA>        <NA>        <NA>   <NA>
#> DA.AD.4            2   DA.AD.4  Sanger   DA.AD.4     MetaHIT      danish      M
#>           Age ClinicalStatus age_months IDs_match placeholder
#> AM.AD.1    28        healthy        336      TRUE        Word
#> AM.AD.2    37        healthy        444      TRUE        Word
#> AM.F10.T1  NA          obese         NA      TRUE        Word
#> AM.F10.T2  NA          obese         NA      TRUE        Word
#> DA.AD.1    59        healthy        708      TRUE        Word
#> DA.AD.1T   NA           <NA>         NA        NA        Word
#> DA.AD.2    54        healthy        648      TRUE        Word
#> DA.AD.3    49          obese        588      TRUE        Word
#> DA.AD.3T   NA           <NA>         NA        NA        Word
#> DA.AD.4    59          obese        708      TRUE        Word

# Using the dplyr::across functionality is also possible
ps <- ps %>%
  ps_mutate(
    dplyr::across(where(is.factor), toupper),
    another_var = TRUE,
    SeqTech = NULL # deletes SeqTech variable
  )

head(sample_data(ps))
#>           Enterotype Sample_ID  SampleID     Project Nationality Gender Age
#> AM.AD.1         <NA>   AM.AD.1   AM.AD.1      GILL06    AMERICAN      F  28
#> AM.AD.2         <NA>   AM.AD.2   AM.AD.2      GILL06    AMERICAN      M  37
#> AM.F10.T1       <NA> AM.F10.T1 AM.F10.T1 TURNBAUGH09    AMERICAN      F  NA
#> AM.F10.T2          3 AM.F10.T2 AM.F10.T2 TURNBAUGH09    AMERICAN      F  NA
#> DA.AD.1            2   DA.AD.1   DA.AD.1     METAHIT      DANISH      F  59
#> DA.AD.1T        <NA>  DA.AD.1T      <NA>        <NA>        <NA>   <NA>  NA
#>           ClinicalStatus age_months IDs_match placeholder another_var
#> AM.AD.1          HEALTHY        336      TRUE        Word        TRUE
#> AM.AD.2          HEALTHY        444      TRUE        Word        TRUE
#> AM.F10.T1          OBESE         NA      TRUE        Word        TRUE
#> AM.F10.T2          OBESE         NA      TRUE        Word        TRUE
#> DA.AD.1          HEALTHY        708      TRUE        Word        TRUE
#> DA.AD.1T            <NA>         NA        NA        Word        TRUE
```
