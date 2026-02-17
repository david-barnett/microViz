# Define psExtra class S4 object

Define psExtra class S4 object

## Slots

- `info`:

  list.

- `counts`:

  otu_table.

- `dist`:

  dist.

- `ord`:

  ANY.

- `permanova`:

  ANY.

- `bdisp`:

  ANY.

- `taxatree_models`:

  list.

- `taxatree_stats`:

  data.frame.

- `tax_models`:

  list.

- `tax_stats`:

  data.frame.

## Examples

``` r
library(phyloseq)
data("shao19")

ps <- shao19 %>% ps_filter(infant_age == 12)
ps %>% tax_agg("genus")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 38 taxa and 8 samples ]
#> sample_data() Sample Data:       [ 8 samples by 11 sample variables ]
#> tax_table()   Taxonomy Table:    [ 38 taxa by 5 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "genus" 
```
