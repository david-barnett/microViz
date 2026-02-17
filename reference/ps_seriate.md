# Arrange samples in a phyloseq by microbiome similarity

Uses seriation methods from seriation::seriate and often dist_calc
(depending on if seriation method requires a distance matrix)

## Usage

``` r
ps_seriate(
  ps,
  method = "OLO_ward",
  dist = "bray",
  tax_transform = "identity",
  add_variable = FALSE,
  rank = NA
)
```

## Arguments

- ps:

  phyloseq object

- method:

  seriation method for ordering samples, from seriation::seriate

- dist:

  distance method for dist_calc (only used if required for particular
  seriation method!)

- tax_transform:

  transformation to apply before seriation or any distance calculation

- add_variable:

  add a variable to the sample data indicating seriation order

- rank:

  taxonomic rank to aggregate at, before seriation, NA for no
  aggregation

## Value

phyloseq

## See also

[`ps_arrange`](https://david-barnett.github.io/microViz/reference/ps_arrange.md)
[`ps_reorder`](https://david-barnett.github.io/microViz/reference/ps_reorder.md)

## Examples

``` r
library(phyloseq)
data("dietswap", package = "microbiome")

dietswap %>%
  sample_data() %>%
  head(8)
#>          subject    sex nationality group   sample timepoint
#> Sample-1     byn   male         AAM    DI Sample-1         4
#> Sample-2     nms   male         AFR    HE Sample-2         2
#> Sample-3     olt   male         AFR    HE Sample-3         2
#> Sample-4     pku female         AFR    HE Sample-4         2
#> Sample-5     qjy female         AFR    HE Sample-5         2
#> Sample-6     riv female         AFR    HE Sample-6         2
#> Sample-7     shj female         AFR    HE Sample-7         2
#> Sample-8     tgx   male         AFR    HE Sample-8         2
#>          timepoint.within.group  bmi_group
#> Sample-1                      1      obese
#> Sample-2                      1       lean
#> Sample-3                      1 overweight
#> Sample-4                      1      obese
#> Sample-5                      1 overweight
#> Sample-6                      1      obese
#> Sample-7                      1      obese
#> Sample-8                      1 overweight

dietswap %>%
  tax_agg("Genus") %>%
  ps_get() %>%
  ps_seriate(method = "OLO_ward", dist = "bray") %>%
  sample_data() %>%
  head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-125     nmz   male         AAM    HE Sample-125         3
#> Sample-200     jql female         AAM    DI Sample-200         4
#> Sample-207     nms   male         AFR    ED Sample-207         1
#> Sample-15      shj female         AFR    HE  Sample-15         3
#> Sample-81      fud female         AFR    DI  Sample-81         4
#> Sample-209     pku female         AFR    ED Sample-209         1
#> Sample-107     byu   male         AFR    HE Sample-107         3
#> Sample-210     qjy female         AFR    ED Sample-210         1
#>            timepoint.within.group  bmi_group
#> Sample-125                      2      obese
#> Sample-200                      1      obese
#> Sample-207                      1       lean
#> Sample-15                       2      obese
#> Sample-81                       1      obese
#> Sample-209                      1      obese
#> Sample-107                      2       lean
#> Sample-210                      1 overweight
```
