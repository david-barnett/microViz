# Set order of samples in phyloseq object

Manually set order of samples by specifying samples names in desired
order.

## Usage

``` r
ps_reorder(ps, sample_order)
```

## Arguments

- ps:

  phyloseq

- sample_order:

  names or current numerical indices of samples in desired order

## Value

phyloseq

## Details

Ordering of samples in a phyloseq is controlled from the otu_table slot!

## See also

[`ps_arrange`](https://david-barnett.github.io/microViz/reference/ps_arrange.md)
for arranging samples by sample_data variables (or otu_table)

[`ps_seriate`](https://david-barnett.github.io/microViz/reference/ps_seriate.md)
for arranging samples by microbiome similarity

[`ps_filter`](https://david-barnett.github.io/microViz/reference/ps_filter.md)
for keeping only some samples, based on sample_data

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

new_order <- rev(sample_names(dietswap))
dietswap %>%
  ps_reorder(new_order) %>%
  sample_data() %>%
  head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-222     ufm   male         AFR    ED Sample-222         6
#> Sample-221     tgx   male         AFR    ED Sample-221         6
#> Sample-220     shj female         AFR    ED Sample-220         6
#> Sample-219     riv female         AFR    ED Sample-219         6
#> Sample-218     qjy female         AFR    ED Sample-218         6
#> Sample-217     pku female         AFR    ED Sample-217         6
#> Sample-216     olt   male         AFR    ED Sample-216         6
#> Sample-215     nms   male         AFR    ED Sample-215         6
#>            timepoint.within.group  bmi_group
#> Sample-222                      2       lean
#> Sample-221                      2 overweight
#> Sample-220                      2      obese
#> Sample-219                      2      obese
#> Sample-218                      2 overweight
#> Sample-217                      2      obese
#> Sample-216                      2 overweight
#> Sample-215                      2       lean

# random ordering with numbers
set.seed(1000)
random_order <- sample(1:nsamples(dietswap))
dietswap %>%
  ps_reorder(random_order) %>%
  sample_data() %>%
  head(8)
#>            subject    sex nationality group     sample timepoint
#> Sample-68      cxj female         AAM    ED  Sample-68         1
#> Sample-43      gty   male         AAM    ED  Sample-43         6
#> Sample-214     ufm   male         AFR    ED Sample-214         1
#> Sample-51      dwc   male         AFR    ED  Sample-51         1
#> Sample-88      fud female         AFR    DI  Sample-88         5
#> Sample-29      qjy female         AFR    DI  Sample-29         5
#> Sample-99      fud female         AFR    HE  Sample-99         3
#> Sample-61      azl female         AFR    ED  Sample-61         1
#>            timepoint.within.group  bmi_group
#> Sample-68                       1 overweight
#> Sample-43                       2 overweight
#> Sample-214                      1       lean
#> Sample-51                       1       lean
#> Sample-88                       2      obese
#> Sample-29                       2 overweight
#> Sample-99                       2      obese
#> Sample-61                       1      obese
```
