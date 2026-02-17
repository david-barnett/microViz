# Arrange samples in phyloseq by sample_data variables or taxon abundance

Uses information in the sample_data or tax_table of phyloseq object to
set the order of the samples (sample_data or tax_table specified by
.target arg)

Give this function arguments in the same way you would use
dplyr::arrange()

## Usage

``` r
ps_arrange(ps, ..., .target = "sample_data")
```

## Arguments

- ps:

  phyloseq object

- ...:

  dots passed directly to dplyr::arrange()

- .target:

  arrange samples by "sample_data" variables or "otu_table" taxa
  abundances

## Value

phyloseq

## See also

[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)

## Examples

``` r
data("dietswap", package = "microbiome")

dietswap %>%
  ps_arrange(subject, timepoint) %>%
  phyloseq::sample_data() %>%
  head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-32      azh female         AAM    ED  Sample-32         1
#> Sample-109     azh female         AAM    HE Sample-109         2
#> Sample-119     azh female         AAM    HE Sample-119         3
#> Sample-130     azh female         AAM    DI Sample-130         4
#> Sample-140     azh female         AAM    DI Sample-140         5
#> Sample-41      azh female         AAM    ED  Sample-41         6
#> Sample-61      azl female         AFR    ED  Sample-61         1
#> Sample-93      azl female         AFR    HE  Sample-93         2
#>            timepoint.within.group  bmi_group
#> Sample-32                       1 overweight
#> Sample-109                      1 overweight
#> Sample-119                      2 overweight
#> Sample-130                      1 overweight
#> Sample-140                      2 overweight
#> Sample-41                       2 overweight
#> Sample-61                       1      obese
#> Sample-93                       1      obese

ps <- dietswap %>% ps_arrange(subject, desc(timepoint))
phyloseq::sample_data(ps) %>% head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-41      azh female         AAM    ED  Sample-41         6
#> Sample-140     azh female         AAM    DI Sample-140         5
#> Sample-130     azh female         AAM    DI Sample-130         4
#> Sample-119     azh female         AAM    HE Sample-119         3
#> Sample-109     azh female         AAM    HE Sample-109         2
#> Sample-32      azh female         AAM    ED  Sample-32         1
#> Sample-67      azl female         AFR    ED  Sample-67         6
#> Sample-86      azl female         AFR    DI  Sample-86         5
#>            timepoint.within.group  bmi_group
#> Sample-41                       2 overweight
#> Sample-140                      2 overweight
#> Sample-130                      1 overweight
#> Sample-119                      2 overweight
#> Sample-109                      1 overweight
#> Sample-32                       1 overweight
#> Sample-67                       2      obese
#> Sample-86                       2      obese
phyloseq::otu_table(ps)[1:8, 1:8]
#> OTU Table:          [8 taxa and 8 samples]
#>                      taxa are rows
#>                              Sample-41 Sample-140 Sample-130 Sample-119
#> Actinomycetaceae                     0          0          0          0
#> Aerococcus                           0          0          0          0
#> Aeromonas                            0          0          0          0
#> Akkermansia                        787       2627       2506       1068
#> Alcaligenes faecalis et rel.         2          2          2          2
#> Allistipes et rel.                 564        224        919        209
#> Anaerobiospirillum                   0          0          0          0
#> Anaerofustis                         0          0          0          0
#>                              Sample-109 Sample-32 Sample-67 Sample-86
#> Actinomycetaceae                      0         0         0         0
#> Aerococcus                            0         0         0         0
#> Aeromonas                             0         0         0         0
#> Akkermansia                          52       200        35        93
#> Alcaligenes faecalis et rel.          2         2         2         2
#> Allistipes et rel.                  756       737        39        29
#> Anaerobiospirillum                    0         0         0         0
#> Anaerofustis                          1         0         0         0

# you can also arrange samples by the abundances of taxa in the otu tables
pst <- dietswap %>% ps_arrange(desc(Akkermansia), .target = "otu_table")
phyloseq::otu_table(pst)[1:8, 1:8]
#> OTU Table:          [8 taxa and 8 samples]
#>                      taxa are rows
#>                              Sample-140 Sample-130 Sample-167 Sample-119
#> Actinomycetaceae                      0          0          0          0
#> Aerococcus                            0          0          0          0
#> Aeromonas                             0          0          0          0
#> Akkermansia                        2627       2506       1214       1068
#> Alcaligenes faecalis et rel.          2          2          4          2
#> Allistipes et rel.                  224        919        923        209
#> Anaerobiospirillum                    0          0          0          0
#> Anaerofustis                          0          0          0          0
#>                              Sample-100 Sample-121 Sample-41 Sample-35
#> Actinomycetaceae                      0          0         0         0
#> Aerococcus                            0          0         0         0
#> Aeromonas                             0          0         0         0
#> Akkermansia                         974        909       787       774
#> Alcaligenes faecalis et rel.          3          2         2         2
#> Allistipes et rel.                   64         50       564       112
#> Anaerobiospirillum                    0          0         0         0
#> Anaerofustis                          0          0         0         0
phyloseq::sample_data(pst) %>% head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-140     azh female         AAM    DI Sample-140         5
#> Sample-130     azh female         AAM    DI Sample-130         4
#> Sample-167     kpb   male         AAM    HE Sample-167         3
#> Sample-119     azh female         AAM    HE Sample-119         3
#> Sample-100     wdf   male         AFR    DI Sample-100         4
#> Sample-121     gty   male         AAM    HE Sample-121         3
#> Sample-41      azh female         AAM    ED  Sample-41         6
#> Sample-35      kpp   male         AAM    ED  Sample-35         1
#>            timepoint.within.group  bmi_group
#> Sample-140                      2 overweight
#> Sample-130                      1 overweight
#> Sample-167                      2      obese
#> Sample-119                      2 overweight
#> Sample-100                      1       lean
#> Sample-121                      2 overweight
#> Sample-41                       2 overweight
#> Sample-35                       1       lean
```
