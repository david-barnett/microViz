# Get names of "top" n taxa

Simple wrapper around tax_sort that:

1.  optionally aggregates taxa at `rank`

2.  sorts the aggregated taxa according to `by`

3.  returns the top `n` number of taxa names

## Usage

``` r
tax_top(data, n = 10, by = sum, rank = "unique", use_counts = FALSE, ...)
```

## Arguments

- data:

  phyloseq object or psExtra

- n:

  how many taxa names to return, or NA for all (can return fewer than n
  values, if there are fewer to return)

- by:

  how to sort taxa (see `?tax_sort()`), defaults to `sum` which sorts by
  total abundance across all samples

- rank:

  taxonomic rank to aggregate at before calculating ("unique" = no
  aggregation)

- use_counts:

  use count data if available, instead of transformed data

- ...:

  Arguments passed on to
  [`tax_sort`](https://david-barnett.github.io/microViz/reference/tax_sort.md)

  `verbose`

  :   passed to phyloseq_validate verbose (if TRUE: message about
      suspicious values in tax_table, and how to fix)

  `trans`

  :   name of transformation to apply to taxa before sorting (taxa are
      returned un-transformed)

## Value

vector of taxa names at chosen rank

## See also

[`tax_agg`](https://david-barnett.github.io/microViz/reference/tax_agg.md)
for more info on taxonomic aggregation

[`tax_sort`](https://david-barnett.github.io/microViz/reference/tax_sort.md)
for more info on sorting taxa

## Examples

``` r
data("dietswap", package = "microbiome")
tax_top(dietswap)
#>  [1] "Prevotella melaninogenica et rel."   
#>  [2] "Oscillospira guillermondii et rel."  
#>  [3] "Bacteroides vulgatus et rel."        
#>  [4] "Clostridium cellulosi et rel."       
#>  [5] "Prevotella oralis et rel."           
#>  [6] "Faecalibacterium prausnitzii et rel."
#>  [7] "Sporobacter termitidis et rel."      
#>  [8] "Clostridium symbiosum et rel."       
#>  [9] "Allistipes et rel."                  
#> [10] "Clostridium orbiscindens et rel."    
tax_top(dietswap, n = 4, by = "prev", rank = "Phylum", undetected = 30)
#> [1] "Firmicutes"     "Proteobacteria" "Bacteroidetes"  "Actinobacteria"
```
