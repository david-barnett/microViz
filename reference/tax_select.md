# Subset phyloseq object by (partial) taxa names

Convenient name-based taxa selection/filtering of phyloseq object,
including approximate name matching. Takes a phyloseq with tax table and
a (partial) taxonomic name, or a list/vector of taxonomic names (full or
partial matches).

## Usage

``` r
tax_select(
  ps,
  tax_list,
  ranks_searched = "all",
  strict_matches = FALSE,
  n_typos = 1,
  deselect = FALSE
)
```

## Arguments

- ps:

  phyloseq object

- tax_list:

  e.g. c('g\_\_Bifidobacterium', 'g\_\_Akkermansia', 'g\_\_Bacteroides',
  'g\_\_Streptococcus')

- ranks_searched:

  'all' or a list of which taxonomic ranks should be searched for the
  names in tax_list?

- strict_matches:

  only perfect full name matches allowed if TRUE

- n_typos:

  how many typos to allow in each name? uses agrep approximate matching
  if \> 0

- deselect:

  if TRUE, the matching taxa will be REMOVED instead!

## Value

phyloseq object with fewer taxa

## Details

tax_select will also search the otu names/rownames, BUT only for perfect
matches.

## See also

[`ps_select`](https://david-barnett.github.io/microViz/reference/ps_select.md)
for selecting variables in phyloseq sample_data

[`agrep`](https://rdrr.io/r/base/agrep.html) for the function that
powers the approximate matching in tax_select

## Examples

``` r
# Get example phyloseq object data
data("dietswap", package = "microbiome")
pSeq <- dietswap

# SELECTION EXAMPLES #
a <- pSeq %>% tax_select(tax_list = "Bif", n_typos = 0, ranks_searched = "Genus")
b <- pSeq %>% tax_select(tax_list = "Bifidobacterium", n_typos = 0)
c <- pSeq %>% tax_select(tax_list = "Bif", n_typos = 1)
identical(a, b) # TRUE
#> [1] TRUE
identical(a, c) # FALSE
#> [1] FALSE

pSeq %>% tax_select(tax_list = "Bifidobactrium") # default 1 typo allowed
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 1 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 1 taxa by 3 taxonomic ranks ]
one <- pSeq %>% tax_select(tax_list = "Akkarmensia", n_typos = 2)
two <- pSeq %>% tax_select(tax_list = "Akkermansia", n_typos = 0)
identical(one, two) # TRUE
#> [1] TRUE

# DESELECTION EXAMPLE # #
pSeq %>% tax_select(tax_list = "Bif", strict_matches = FALSE, deselect = TRUE)
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 125 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 125 taxa by 3 taxonomic ranks ]
# Incorrect example
# pSeq %>% tax_select(tax_list = "Bif", strict_matches = TRUE) # fails
```
