# Aggregate taxa and track aggregation in psExtra

`tax_agg` sums the abundances of the phyloseq taxa at the given rank. It
records the tax_agg rank argument in the info of the psExtra object
output. This psExtra object tracks aggregation, and any further
transformations and scaling, to help you keep track of what you have
done with your phyloseq object and automatically caption ordination
plots.

Instead of tax_agg, consider using
[`tax_transform()`](https://david-barnett.github.io/microViz/reference/tax_transform.md)
with a rank argument instead, to both aggregate and transform the taxa.
This is also useful when you want to aggregate but not transform the
taxa, and yet still log the "identity" transformation in psExtra for
captioning your ordination plots. e.g.
`tax_transform(rank = "Genus", trans = "identity")`

tax_agg allows you to pass NA or "unique" to the rank argument which
will NOT aggregate the taxa. If you use rank = "unique" or add_unique =
TRUE, it will add a new rank called unique, identical to the taxa_names
(after any aggregation)

Be aware: you should not use the top_N argument yourself without good
reason. top_N provides a feature inspired by the deprecated microbiome
function aggregate_top_taxa which is primarily useful for decluttering
compositional barplots. microViz comp_barplot (and ord_plot_iris)
already run tax_agg with a top_N argument for you, so you should not.
The tax_table produced when using top_N is otherwise INVALID FOR MOST
OTHER ANALYSES.

## Usage

``` r
tax_agg(
  ps,
  rank = NA,
  sort_by = NA,
  top_N = NA,
  force = FALSE,
  add_unique = FALSE
)
```

## Arguments

- ps:

  phyloseq object

- rank:

  NA (for tax_names level) or name of valid taxonomic rank (try
  phyloseq::rank_names(ps)) or "unique"

- sort_by:

  if not NA, how should the taxa be sorted, uses tax_sort(), takes same
  options as `by` arg

- top_N:

  NA does nothing, but if top_N is a number, it creates an extra
  tax_table column called top, which is the same as the unique column
  for the first top_N number of taxa, and "other" otherwise.

- force:

  If TRUE, this forces aggregation at chosen rank to occur regardless of
  if the output will be sensible! This avoids the "Taxa not unique at
  rank: ..." error, but may allow very inappropriate aggregation to
  occur. Do not use force = TRUE unless you know why you are doing this,
  and what the result will be. If you are getting an error with force =
  FALSE, it is almost certainly better to examine the tax_table and fix
  the problem. force = TRUE is similar to microbiome::aggregate_taxa,
  which also does not check that the taxa are uniquely defined by only
  the aggregation level.

- add_unique:

  if TRUE, adds a rank named unique, identical to the rownames after
  aggregation

## Value

psExtra object including phyloseq and tax_agg rank info

## Details

This function is inspired by
[`microbiome::aggregate_taxa`](https://rdrr.io/pkg/microbiome/man/aggregate_taxa.html).
However if
[`microbiome::aggregate_taxa`](https://rdrr.io/pkg/microbiome/man/aggregate_taxa.html)
is used, microViz cannot track this aggregation.

Comparing aggregate_taxa and tax_agg:

Except for the ordering of taxa, and the addition of a "unique" rank
being optional (in tax_agg), the resulting phyloseq objects are
identical for aggregating a phyloseq with no ambiguous taxa. Taxa are
ambiguous when the tax_table converges at a lower rank after branching,
such as if two different genera share the same species (e.g. "s\_\_").
[`microbiome::aggregate_taxa`](https://rdrr.io/pkg/microbiome/man/aggregate_taxa.html)
handles ambiguous taxa by creating a "unique" rank with all of the
taxonomic rank info pasted together into one, often very long, name.
`tax_agg` throws an error, and directs the user to
[`tax_fix()`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
to fix the ambiguous taxa before aggregation, which should then result
in (much) shorter unique names at the aggregation rank.

## See also

[`tax_fix`](https://david-barnett.github.io/microViz/reference/tax_fix.md)

[`tax_fix_interactive`](https://david-barnett.github.io/microViz/reference/tax_fix_interactive.md)

[`tax_transform`](https://david-barnett.github.io/microViz/reference/tax_transform.md)

## Examples

``` r
library(phyloseq)
library(dplyr)
data("dietswap", package = "microbiome")

tax_agg(ps = dietswap, "Phylum") %>%
  ps_get() %>%
  tax_table()
#> Taxonomy Table:     [8 taxa by 1 taxonomic ranks]:
#>                 Phylum           
#> Actinobacteria  "Actinobacteria" 
#> Firmicutes      "Firmicutes"     
#> Proteobacteria  "Proteobacteria" 
#> Verrucomicrobia "Verrucomicrobia"
#> Bacteroidetes   "Bacteroidetes"  
#> Spirochaetes    "Spirochaetes"   
#> Fusobacteria    "Fusobacteria"   
#> Cyanobacteria   "Cyanobacteria"  
tax_agg(ps = dietswap, "Family") %>%
  ps_get() %>%
  tax_table()
#> Taxonomy Table:     [22 taxa by 2 taxonomic ranks]:
#>                           Phylum            Family                     
#> Actinobacteria            "Actinobacteria"  "Actinobacteria"           
#> Bacilli                   "Firmicutes"      "Bacilli"                  
#> Proteobacteria            "Proteobacteria"  "Proteobacteria"           
#> Verrucomicrobia           "Verrucomicrobia" "Verrucomicrobia"          
#> Bacteroidetes             "Bacteroidetes"   "Bacteroidetes"            
#> Clostridium cluster XV    "Firmicutes"      "Clostridium cluster XV"   
#> Clostridium cluster XIVa  "Firmicutes"      "Clostridium cluster XIVa" 
#> Clostridium cluster IV    "Firmicutes"      "Clostridium cluster IV"   
#> Clostridium cluster XI    "Firmicutes"      "Clostridium cluster XI"   
#> Asteroleplasma            "Firmicutes"      "Asteroleplasma"           
#> Spirochaetes              "Spirochaetes"    "Spirochaetes"             
#> Clostridium cluster XVI   "Firmicutes"      "Clostridium cluster XVI"  
#> Clostridium cluster XVII  "Firmicutes"      "Clostridium cluster XVII" 
#> Clostridium cluster I     "Firmicutes"      "Clostridium cluster I"    
#> Clostridium cluster XVIII "Firmicutes"      "Clostridium cluster XVIII"
#> Clostridium cluster III   "Firmicutes"      "Clostridium cluster III"  
#> Clostridium cluster IX    "Firmicutes"      "Clostridium cluster IX"   
#> Fusobacteria              "Fusobacteria"    "Fusobacteria"             
#> Clostridium cluster XIII  "Firmicutes"      "Clostridium cluster XIII" 
#> Cyanobacteria             "Cyanobacteria"   "Cyanobacteria"            
#> Uncultured Clostridiales  "Firmicutes"      "Uncultured Clostridiales" 
#> Uncultured Mollicutes     "Firmicutes"      "Uncultured Mollicutes"    

# create some missing values
tax_table(dietswap)[3:7, "Genus"] <- "g__"

# this will produce an error, instructing the user to use tax_fix
# tax_agg(ps = dietswap, "Genus")

# this will then work:
dietswap %>%
  tax_fix() %>%
  tax_agg("Genus")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 128 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 128 taxa by 3 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Genus" 

# you can replace unknown values with `tax_fix()`
# which will fix most problems, like the common "g__" and "s__"
# but default tax_fix settings won't catch this long unknown
tax_table(dietswap)[13:17, "Family"] <- "some_unknown_family"
dietswap %>%
  tax_fix(unknowns = "some_unknown_family") %>%
  tax_agg("Family")
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 25 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 25 taxa by 2 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Family" 

# try tax_fix_interactive() to help you find and fix all the uninformative
# and converging values in your taxonomy table.

# the code below won't aggregate taxa,
# but just adds a new rank called unique, equal to taxa_names
tax_agg(ps = dietswap, rank = NA, add_unique = TRUE)
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 4 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "unique" 
identical(tax_agg(dietswap, NA, add_unique = TRUE), tax_agg(dietswap, "unique")) # TRUE
#> [1] TRUE
```
