# Add logical label column to taxatree_stats dataframe

`taxatree_label` is used internally by `taxatree_plotkey`, but can also
be used prior to `taxatree_plots` to label those plots directly

`...` arguments are passed to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html),
so all `filter` syntax can be used.

## Usage

``` r
taxatree_label(
  data,
  ...,
  .label_var = "label",
  .node_fun = list(prevalence = prev)
)
```

## Arguments

- data:

  psExtra (or phyloseq)

- ...:

  REQUIRED logical conditions for labelling e.g. rank == "Phylum",
  p.value \< 0.1 \| taxon %in% listOfTaxa

- .label_var:

  name of label indicator variable to be created. If you change this,
  beware that taxatree_plotkey will not work, you will need to called
  taxatree_plot_label with

- .node_fun:

  named list of length 1 providing `taxatree_nodes` `fun` arg. (name of
  list iterm is available for use in ...)

## Value

psExtra with (modified) taxatree_stats dataframe

## Details

If taxatree_stats missing (or if data is a phyloseq) it will create a
plain taxatree_stats dataframe using only taxatree_nodes

`node_fun` can also be a precalculated dataframe (output of
taxatree_nodes) but you should probably not use this option. This is
used internally for efficiency inside
[`taxatree_plotkey()`](https://david-barnett.github.io/microViz/reference/taxatree_plotkey.md)

## Examples

``` r
# simple example with plain phyloseq input
data("dietswap", package = "microbiome")
labelled <- dietswap %>%
  tax_prepend_ranks() %>%
  taxatree_label(rank == "Phylum", prevalence > 0.1)

# Note that "prevalence" column was available in data
# because it is created by `taxatree_nodes()` using the named function
# provided to the `node_fun` argument

# psExtra is returned
labelled
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> 
#> taxatree_stats dataframe:
#> 161 taxa at 4 ranks: root, Phylum, Family, Genus 
#> 0 terms: 

# notice how both conditions must be met for label column to be TRUE
labelled %>% taxatree_stats_get()
#> # A tibble: 161 × 5
#>    taxon              parent            rank   prevalence label
#>    <chr>              <chr>             <chr>       <dbl> <lgl>
#>  1 root               root              root       1      FALSE
#>  2 P: Actinobacteria  root              Phylum     1      TRUE 
#>  3 P: Firmicutes      root              Phylum     1      TRUE 
#>  4 P: Proteobacteria  root              Phylum     1      TRUE 
#>  5 P: Verrucomicrobia root              Phylum     1      TRUE 
#>  6 P: Bacteroidetes   root              Phylum     1      TRUE 
#>  7 P: Spirochaetes    root              Phylum     0.0811 FALSE
#>  8 P: Fusobacteria    root              Phylum     1      TRUE 
#>  9 P: Cyanobacteria   root              Phylum     0.0180 FALSE
#> 10 F: Actinobacteria  P: Actinobacteria Family     1      FALSE
#> # ℹ 151 more rows
```
