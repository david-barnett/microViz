# Add rank prefixes to phyloseq tax_table values

Prepend the start of rank names to each taxon at each rank (useful
particularly in case of duplicated taxa names across ranks, e.g.
dietswap dataset)

## Usage

``` r
tax_prepend_ranks(ps, sep = ": ", nchar = 1)
```

## Arguments

- ps:

  phyloseq object

- sep:

  characters to paste in between rank initial and taxon name

- nchar:

  number of characters to use from start of rank_names

## Value

phyloseq

## See also

[`tax_fix`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
for fixing other tax_table problems

## Examples

``` r
data("dietswap", package = "microbiome")
phyloseq::tax_table(dietswap) %>% head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                              Phylum            Family           
#> Actinomycetaceae             "Actinobacteria"  "Actinobacteria" 
#> Aerococcus                   "Firmicutes"      "Bacilli"        
#> Aeromonas                    "Proteobacteria"  "Proteobacteria" 
#> Akkermansia                  "Verrucomicrobia" "Verrucomicrobia"
#> Alcaligenes faecalis et rel. "Proteobacteria"  "Proteobacteria" 
#> Allistipes et rel.           "Bacteroidetes"   "Bacteroidetes"  
#>                              Genus                         
#> Actinomycetaceae             "Actinomycetaceae"            
#> Aerococcus                   "Aerococcus"                  
#> Aeromonas                    "Aeromonas"                   
#> Akkermansia                  "Akkermansia"                 
#> Alcaligenes faecalis et rel. "Alcaligenes faecalis et rel."
#> Allistipes et rel.           "Allistipes et rel."          
dietswap %>%
  tax_prepend_ranks() %>%
  phyloseq::tax_table() %>%
  head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                              Phylum               Family              
#> Actinomycetaceae             "P: Actinobacteria"  "F: Actinobacteria" 
#> Aerococcus                   "P: Firmicutes"      "F: Bacilli"        
#> Aeromonas                    "P: Proteobacteria"  "F: Proteobacteria" 
#> Akkermansia                  "P: Verrucomicrobia" "F: Verrucomicrobia"
#> Alcaligenes faecalis et rel. "P: Proteobacteria"  "F: Proteobacteria" 
#> Allistipes et rel.           "P: Bacteroidetes"   "F: Bacteroidetes"  
#>                              Genus                            
#> Actinomycetaceae             "G: Actinomycetaceae"            
#> Aerococcus                   "G: Aerococcus"                  
#> Aeromonas                    "G: Aeromonas"                   
#> Akkermansia                  "G: Akkermansia"                 
#> Alcaligenes faecalis et rel. "G: Alcaligenes faecalis et rel."
#> Allistipes et rel.           "G: Allistipes et rel."          
```
