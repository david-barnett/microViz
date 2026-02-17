# Reorder taxa in phyloseq object using vector of names

Reorder taxa in phyloseq object using vector of names

## Usage

``` r
tax_reorder(
  ps,
  tax_order,
  tree_warn = TRUE,
  unmatched_warn = TRUE,
  ignore = c("other", "Other")
)
```

## Arguments

- ps:

  phyloseq object

- tax_order:

  Names of taxa in desired order; at least some must match. (Numerical
  indices are also possible)

- tree_warn:

  If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot
  be reordered. Default behaviour of tax_sort is to remove the
  phylogenetic tree and warn about this. tree_warn = FALSE will suppress
  the warning message, but still remove the tree!

- unmatched_warn:

  Warn if any names (or indices) given in tax_order are not found within
  (range of) taxa_names(ps) - these will be ignored

- ignore:

  Values that you do not want to be used for reordering taxa (useful for
  comp_barplot when custom palette names are used to set tax_order)

## Value

phyloseq object (always without phy_tree)

## Examples

``` r
data("dietswap", package = "microbiome")
new_order <- c(
  "Fusobacteria", "Cyanobacteria", "Verrucomicrobia", "Spirochaetes",
  "Actinobacteria", "Firmicutes", "Proteobacteria", "Bacteroidetes"
)
tax_agg(dietswap, rank = "Phylum") %>%
  ps_get() %>%
  phyloseq::taxa_names()
#> [1] "Actinobacteria"  "Firmicutes"      "Proteobacteria"  "Verrucomicrobia"
#> [5] "Bacteroidetes"   "Spirochaetes"    "Fusobacteria"    "Cyanobacteria"  

tax_agg(dietswap, rank = "Phylum") %>%
  ps_get() %>%
  tax_reorder(tax_order = new_order) %>%
  phyloseq::taxa_names()
#> [1] "Fusobacteria"    "Cyanobacteria"   "Verrucomicrobia" "Spirochaetes"   
#> [5] "Actinobacteria"  "Firmicutes"      "Proteobacteria"  "Bacteroidetes"  

# partial reordering (of the frontmost positions only) is possible
tax_agg(dietswap, rank = "Phylum") %>%
  ps_get() %>%
  tax_reorder(tax_order = c("Cyanobacteria", "Bacteroidetes")) %>%
  phyloseq::taxa_names()
#> [1] "Cyanobacteria"   "Bacteroidetes"   "Actinobacteria"  "Firmicutes"     
#> [5] "Proteobacteria"  "Verrucomicrobia" "Spirochaetes"    "Fusobacteria"   
```
