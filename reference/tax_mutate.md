# Modify or compute new taxonomic ranks in phyloseq

Add or overwrite tax_table ranks. Use dplyr::mutate() syntax.

## Usage

``` r
tax_mutate(ps, ...)
```

## Arguments

- ps:

  phyloseq object with a tax_table, or just a tax_table

- ...:

  passed straight to dplyr::mutate (see examples and dplyr::mutate help)

## Value

phyloseq object with modified tax_table

## See also

[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)

[`ps_mutate`](https://david-barnett.github.io/microViz/reference/ps_mutate.md)

## Examples

``` r
library(phyloseq)
library(dplyr)
data("dietswap", package = "microbiome")

# compute new rank
tax_mutate(dietswap, loud_genus = toupper(Genus)) %>%
  tt_get() %>%
  head()
#> Taxonomy Table:     [6 taxa by 4 taxonomic ranks]:
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
#>                              loud_genus                    
#> Actinomycetaceae             "ACTINOMYCETACEAE"            
#> Aerococcus                   "AEROCOCCUS"                  
#> Aeromonas                    "AEROMONAS"                   
#> Akkermansia                  "AKKERMANSIA"                 
#> Alcaligenes faecalis et rel. "ALCALIGENES FAECALIS ET REL."
#> Allistipes et rel.           "ALLISTIPES ET REL."          

# overwrite a current rank
tax_mutate(dietswap, Genus = toupper(Genus)) %>%
  tt_get() %>%
  head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                              Phylum            Family           
#> Actinomycetaceae             "Actinobacteria"  "Actinobacteria" 
#> Aerococcus                   "Firmicutes"      "Bacilli"        
#> Aeromonas                    "Proteobacteria"  "Proteobacteria" 
#> Akkermansia                  "Verrucomicrobia" "Verrucomicrobia"
#> Alcaligenes faecalis et rel. "Proteobacteria"  "Proteobacteria" 
#> Allistipes et rel.           "Bacteroidetes"   "Bacteroidetes"  
#>                              Genus                         
#> Actinomycetaceae             "ACTINOMYCETACEAE"            
#> Aerococcus                   "AEROCOCCUS"                  
#> Aeromonas                    "AEROMONAS"                   
#> Akkermansia                  "AKKERMANSIA"                 
#> Alcaligenes faecalis et rel. "ALCALIGENES FAECALIS ET REL."
#> Allistipes et rel.           "ALLISTIPES ET REL."          

# overwrite all ranks
tax_mutate(dietswap, across(everything(), .fns = toupper)) %>%
  tt_get() %>%
  head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                              Phylum            Family           
#> Actinomycetaceae             "ACTINOBACTERIA"  "ACTINOBACTERIA" 
#> Aerococcus                   "FIRMICUTES"      "BACILLI"        
#> Aeromonas                    "PROTEOBACTERIA"  "PROTEOBACTERIA" 
#> Akkermansia                  "VERRUCOMICROBIA" "VERRUCOMICROBIA"
#> Alcaligenes faecalis et rel. "PROTEOBACTERIA"  "PROTEOBACTERIA" 
#> Allistipes et rel.           "BACTEROIDETES"   "BACTEROIDETES"  
#>                              Genus                         
#> Actinomycetaceae             "ACTINOMYCETACEAE"            
#> Aerococcus                   "AEROCOCCUS"                  
#> Aeromonas                    "AEROMONAS"                   
#> Akkermansia                  "AKKERMANSIA"                 
#> Alcaligenes faecalis et rel. "ALCALIGENES FAECALIS ET REL."
#> Allistipes et rel.           "ALLISTIPES ET REL."          

# add a new rank at the beginning
tax_mutate(dietswap, Root = "Bacteria", .before = 1) %>%
  tt_get() %>%
  head()
#> Taxonomy Table:     [6 taxa by 4 taxonomic ranks]:
#>                              Root       Phylum            Family           
#> Actinomycetaceae             "Bacteria" "Actinobacteria"  "Actinobacteria" 
#> Aerococcus                   "Bacteria" "Firmicutes"      "Bacilli"        
#> Aeromonas                    "Bacteria" "Proteobacteria"  "Proteobacteria" 
#> Akkermansia                  "Bacteria" "Verrucomicrobia" "Verrucomicrobia"
#> Alcaligenes faecalis et rel. "Bacteria" "Proteobacteria"  "Proteobacteria" 
#> Allistipes et rel.           "Bacteria" "Bacteroidetes"   "Bacteroidetes"  
#>                              Genus                         
#> Actinomycetaceae             "Actinomycetaceae"            
#> Aerococcus                   "Aerococcus"                  
#> Aeromonas                    "Aeromonas"                   
#> Akkermansia                  "Akkermansia"                 
#> Alcaligenes faecalis et rel. "Alcaligenes faecalis et rel."
#> Allistipes et rel.           "Allistipes et rel."          

# this is an error as ranks can't be any other class than character
# tax_mutate(dietswap, Genus = 1:ntaxa(dietswap))
```
