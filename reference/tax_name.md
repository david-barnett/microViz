# Simple way to set unique taxa_names for phyloseq object

If your current taxa_names aren't what you want (e.g. they are long DNA
sequences), this function will help you set sensible unique names.

It combines:

- a prefix like tax, asv, or otu (pick an appropriate prefix or set your
  own)

- a unique (sequential) number

- classification information from a chosen taxonomic rank (optional)

## Usage

``` r
tax_name(
  ps,
  prefix = c("tax", "asv", "otu")[1],
  rank = NA,
  pad_number = TRUE,
  sep = "_"
)
```

## Arguments

- ps:

  phyloseq object

- prefix:

  e.g. 'tax', 'asv', or 'otu' (or set your own)

- rank:

  name of taxonomic rank from which to use classifications in new names

- pad_number:

  should unique numbers have zeros added to the front (e.g. 001, 002) to
  be made the same number of characters?

- sep:

  character to separate the unique number and any taxonomic
  classification info (relevant if rank given)

## Value

phyloseq object

## Details

Don't confuse this with the phyloseq function
[`taxa_names()`](https://rdrr.io/pkg/phyloseq/man/taxa_names-methods.html)
or the newer microViz function
[`tax_rename()`](https://david-barnett.github.io/microViz/reference/tax_rename.md).

## See also

[`tax_rename`](https://david-barnett.github.io/microViz/reference/tax_rename.md)
for a more informative taxon naming tool

`phyloseq::`[`taxa_names`](https://rdrr.io/pkg/phyloseq/man/taxa_names-methods.html)
for accessing and manually setting names

## Examples

``` r
library(phyloseq)
# get example data
data("enterotype")
ps <- enterotype
head(taxa_names(ps)) # these are mostly fine (except the -1), but imagine you wanted new names
#> [1] "-1"               "Bacteria"         "Prosthecochloris" "Chloroflexus"    
#> [5] "Dehalococcoides"  "Thermus"         

# consider storing the original names for reference (e.g. if they are DNA sequences)
old_taxa_names <- taxa_names(ps)

ps <- tax_name(ps)
taxa_names(ps) %>% head()
#> [1] "tax001" "tax002" "tax003" "tax004" "tax005" "tax006"

# probably better to include the genus info to make these names more informative
ps <- tax_name(ps, rank = "Genus")
taxa_names(ps) %>% head()
#> [1] "tax001_NA"               "tax002_NA"              
#> [3] "tax003_Prosthecochloris" "tax004_Chloroflexus"    
#> [5] "tax005_Dehalococcoides"  "tax006_Thermus"         

# store new names with old names in dataframe for reference
names_df <- tibble::tibble(old = old_taxa_names, new = taxa_names(ps))

# alternative settings
tax_name(ps, pad_number = FALSE) %>%
  taxa_names() %>%
  head()
#> [1] "tax1" "tax2" "tax3" "tax4" "tax5" "tax6"
tax_name(ps, prefix = "whateveryoulike") %>%
  taxa_names() %>%
  head()
#> [1] "whateveryoulike001" "whateveryoulike002" "whateveryoulike003"
#> [4] "whateveryoulike004" "whateveryoulike005" "whateveryoulike006"
tax_name(ps, rank = "Genus", sep = "-") %>%
  taxa_names() %>%
  head()
#> [1] "tax001-NA"               "tax002-NA"              
#> [3] "tax003-Prosthecochloris" "tax004-Chloroflexus"    
#> [5] "tax005-Dehalococcoides"  "tax006-Thermus"         
```
