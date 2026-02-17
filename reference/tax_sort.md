# Sort taxa in phyloseq otu_table and tax_table

Multiple ways of sorting taxa are possible and determined by the `by`
argument. The `by` argument must be one of:

- 'rev' to reverse the current order

- 'name' (sort alphabetically by `at`)

- 'asis' to keep current order as is

- a sample name (descending abundance sorting within that sample)

- summary stat. function e.g. `sum` or `mean`

The `at` argument must be "names" for sorting unique taxa, or a rank
name, for sorting at that rank. `at` is ignored when `by` is "rev".

## Usage

``` r
tax_sort(
  data,
  by = "name",
  at = "names",
  ...,
  tree_warn = TRUE,
  verbose = TRUE,
  trans = "identity",
  use_counts = TRUE,
  counts_warn = TRUE
)
```

## Arguments

- data:

  psExtra or phyloseq

- by:

  how to sort, see description

- at:

  "names" or a taxonomic rank to apply sorting method to, as specified
  in `by`.

- ...:

  used if summary function given, or pass `undetected` arg for
  tax_transform("binary") if by = "prev" or "prevalence"

- tree_warn:

  If phylogenetic tree is present in phyloseq phy_tree slot, taxa cannot
  be reordered. Default behaviour of tax_sort is to remove the
  phylogenetic tree and warn about this. tree_warn = FALSE will suppress
  the warning message, but still remove the tree!

- verbose:

  passed to phyloseq_validate verbose (if TRUE: message about suspicious
  values in tax_table, and how to fix)

- trans:

  name of transformation to apply to taxa before sorting (taxa are
  returned un-transformed)

- use_counts:

  use count data if available, instead of transformed data

- counts_warn:

  warn if count data are not available?

## Value

sorted phyloseq or psExtra

## Details

Don't forget to pass `na.rm = TRUE` to `...` if using a summary stat
function in `by`

## Examples

``` r
library(phyloseq)
data("dietswap", package = "microbiome")
dietswap
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# reverse current order
dietswap %>%
  tax_sort("rev") %>%
  tax_table() %>%
  head(30)
#> Taxonomy Table:     [30 taxa by 3 taxonomic ranks]:
#>                                   Phylum           Family                    
#> Yersinia et rel.                  "Proteobacteria" "Proteobacteria"          
#> Xanthomonadaceae                  "Proteobacteria" "Proteobacteria"          
#> Wissella et rel.                  "Firmicutes"     "Bacilli"                 
#> Weissella et rel.                 "Firmicutes"     "Bacilli"                 
#> Vibrio                            "Proteobacteria" "Proteobacteria"          
#> Veillonella                       "Firmicutes"     "Clostridium cluster IX"  
#> Uncultured Selenomonadaceae       "Firmicutes"     "Clostridium cluster IX"  
#> Uncultured Mollicutes             "Firmicutes"     "Uncultured Mollicutes"   
#> Uncultured Clostridiales II       "Firmicutes"     "Uncultured Clostridiales"
#> Uncultured Clostridiales I        "Firmicutes"     "Uncultured Clostridiales"
#> Uncultured Chroococcales          "Cyanobacteria"  "Cyanobacteria"           
#> Uncultured Bacteroidetes          "Bacteroidetes"  "Bacteroidetes"           
#> Tannerella et rel.                "Bacteroidetes"  "Bacteroidetes"           
#> Sutterella wadsworthia et rel.    "Proteobacteria" "Proteobacteria"          
#> Subdoligranulum variable at rel.  "Firmicutes"     "Clostridium cluster IV"  
#> Streptococcus mitis et rel.       "Firmicutes"     "Bacilli"                 
#> Streptococcus intermedius et rel. "Firmicutes"     "Bacilli"                 
#> Streptococcus bovis et rel.       "Firmicutes"     "Bacilli"                 
#> Staphylococcus                    "Firmicutes"     "Bacilli"                 
#> Sporobacter termitidis et rel.    "Firmicutes"     "Clostridium cluster IV"  
#> Serratia                          "Proteobacteria" "Proteobacteria"          
#> Ruminococcus obeum et rel.        "Firmicutes"     "Clostridium cluster XIVa"
#> Ruminococcus lactaris et rel.     "Firmicutes"     "Clostridium cluster XIVa"
#> Ruminococcus gnavus et rel.       "Firmicutes"     "Clostridium cluster XIVa"
#> Ruminococcus callidus et rel.     "Firmicutes"     "Clostridium cluster IV"  
#> Ruminococcus bromii et rel.       "Firmicutes"     "Clostridium cluster IV"  
#> Roseburia intestinalis et rel.    "Firmicutes"     "Clostridium cluster XIVa"
#> Pseudomonas                       "Proteobacteria" "Proteobacteria"          
#> Proteus et rel.                   "Proteobacteria" "Proteobacteria"          
#> Propionibacterium                 "Actinobacteria" "Actinobacteria"          
#>                                   Genus                              
#> Yersinia et rel.                  "Yersinia et rel."                 
#> Xanthomonadaceae                  "Xanthomonadaceae"                 
#> Wissella et rel.                  "Wissella et rel."                 
#> Weissella et rel.                 "Weissella et rel."                
#> Vibrio                            "Vibrio"                           
#> Veillonella                       "Veillonella"                      
#> Uncultured Selenomonadaceae       "Uncultured Selenomonadaceae"      
#> Uncultured Mollicutes             "Uncultured Mollicutes"            
#> Uncultured Clostridiales II       "Uncultured Clostridiales II"      
#> Uncultured Clostridiales I        "Uncultured Clostridiales I"       
#> Uncultured Chroococcales          "Uncultured Chroococcales"         
#> Uncultured Bacteroidetes          "Uncultured Bacteroidetes"         
#> Tannerella et rel.                "Tannerella et rel."               
#> Sutterella wadsworthia et rel.    "Sutterella wadsworthia et rel."   
#> Subdoligranulum variable at rel.  "Subdoligranulum variable at rel." 
#> Streptococcus mitis et rel.       "Streptococcus mitis et rel."      
#> Streptococcus intermedius et rel. "Streptococcus intermedius et rel."
#> Streptococcus bovis et rel.       "Streptococcus bovis et rel."      
#> Staphylococcus                    "Staphylococcus"                   
#> Sporobacter termitidis et rel.    "Sporobacter termitidis et rel."   
#> Serratia                          "Serratia"                         
#> Ruminococcus obeum et rel.        "Ruminococcus obeum et rel."       
#> Ruminococcus lactaris et rel.     "Ruminococcus lactaris et rel."    
#> Ruminococcus gnavus et rel.       "Ruminococcus gnavus et rel."      
#> Ruminococcus callidus et rel.     "Ruminococcus callidus et rel."    
#> Ruminococcus bromii et rel.       "Ruminococcus bromii et rel."      
#> Roseburia intestinalis et rel.    "Roseburia intestinalis et rel."   
#> Pseudomonas                       "Pseudomonas"                      
#> Proteus et rel.                   "Proteus et rel."                  
#> Propionibacterium                 "Propionibacterium"                

# sort alphabetically by a taxonomic rank (or "names" for taxa_names)
dietswap %>%
  tax_sort(by = "name", at = "Phylum") %>%
  tax_table() %>%
  head(30)
#> Taxonomy Table:     [30 taxa by 3 taxonomic ranks]:
#>                                    Phylum           Family                    
#> Actinomycetaceae                   "Actinobacteria" "Actinobacteria"          
#> Atopobium                          "Actinobacteria" "Actinobacteria"          
#> Bifidobacterium                    "Actinobacteria" "Actinobacteria"          
#> Collinsella                        "Actinobacteria" "Actinobacteria"          
#> Corynebacterium                    "Actinobacteria" "Actinobacteria"          
#> Eggerthella lenta et rel.          "Actinobacteria" "Actinobacteria"          
#> Micrococcaceae                     "Actinobacteria" "Actinobacteria"          
#> Propionibacterium                  "Actinobacteria" "Actinobacteria"          
#> Allistipes et rel.                 "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides fragilis et rel.       "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides intestinalis et rel.   "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides ovatus et rel.         "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides plebeius et rel.       "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides splachnicus et rel.    "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides stercoris et rel.      "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides uniformis et rel.      "Bacteroidetes"  "Bacteroidetes"           
#> Bacteroides vulgatus et rel.       "Bacteroidetes"  "Bacteroidetes"           
#> Parabacteroides distasonis et rel. "Bacteroidetes"  "Bacteroidetes"           
#> Prevotella melaninogenica et rel.  "Bacteroidetes"  "Bacteroidetes"           
#> Prevotella oralis et rel.          "Bacteroidetes"  "Bacteroidetes"           
#> Prevotella ruminicola et rel.      "Bacteroidetes"  "Bacteroidetes"           
#> Prevotella tannerae et rel.        "Bacteroidetes"  "Bacteroidetes"           
#> Tannerella et rel.                 "Bacteroidetes"  "Bacteroidetes"           
#> Uncultured Bacteroidetes           "Bacteroidetes"  "Bacteroidetes"           
#> Uncultured Chroococcales           "Cyanobacteria"  "Cyanobacteria"           
#> Aerococcus                         "Firmicutes"     "Bacilli"                 
#> Anaerofustis                       "Firmicutes"     "Clostridium cluster XV"  
#> Anaerostipes caccae et rel.        "Firmicutes"     "Clostridium cluster XIVa"
#> Anaerotruncus colihominis et rel.  "Firmicutes"     "Clostridium cluster IV"  
#> Anaerovorax odorimutans et rel.    "Firmicutes"     "Clostridium cluster XI"  
#>                                    Genus                               
#> Actinomycetaceae                   "Actinomycetaceae"                  
#> Atopobium                          "Atopobium"                         
#> Bifidobacterium                    "Bifidobacterium"                   
#> Collinsella                        "Collinsella"                       
#> Corynebacterium                    "Corynebacterium"                   
#> Eggerthella lenta et rel.          "Eggerthella lenta et rel."         
#> Micrococcaceae                     "Micrococcaceae"                    
#> Propionibacterium                  "Propionibacterium"                 
#> Allistipes et rel.                 "Allistipes et rel."                
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Parabacteroides distasonis et rel. "Parabacteroides distasonis et rel."
#> Prevotella melaninogenica et rel.  "Prevotella melaninogenica et rel." 
#> Prevotella oralis et rel.          "Prevotella oralis et rel."         
#> Prevotella ruminicola et rel.      "Prevotella ruminicola et rel."     
#> Prevotella tannerae et rel.        "Prevotella tannerae et rel."       
#> Tannerella et rel.                 "Tannerella et rel."                
#> Uncultured Bacteroidetes           "Uncultured Bacteroidetes"          
#> Uncultured Chroococcales           "Uncultured Chroococcales"          
#> Aerococcus                         "Aerococcus"                        
#> Anaerofustis                       "Anaerofustis"                      
#> Anaerostipes caccae et rel.        "Anaerostipes caccae et rel."       
#> Anaerotruncus colihominis et rel.  "Anaerotruncus colihominis et rel." 
#> Anaerovorax odorimutans et rel.    "Anaerovorax odorimutans et rel."   

# sequentially sorting by higher ranks
# sets tax_table in nested alphabetical order
dietswap %>%
  tax_sort(at = "names") %>%
  tax_sort(at = "Genus") %>%
  tax_sort(at = "Family") %>%
  tax_sort(at = "Phylum") %>%
  tax_table() %>%
  head(30)
#> Taxonomy Table:     [30 taxa by 3 taxonomic ranks]:
#>                                    Phylum           Family          
#> Actinomycetaceae                   "Actinobacteria" "Actinobacteria"
#> Atopobium                          "Actinobacteria" "Actinobacteria"
#> Bifidobacterium                    "Actinobacteria" "Actinobacteria"
#> Collinsella                        "Actinobacteria" "Actinobacteria"
#> Corynebacterium                    "Actinobacteria" "Actinobacteria"
#> Eggerthella lenta et rel.          "Actinobacteria" "Actinobacteria"
#> Micrococcaceae                     "Actinobacteria" "Actinobacteria"
#> Propionibacterium                  "Actinobacteria" "Actinobacteria"
#> Allistipes et rel.                 "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides fragilis et rel.       "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides intestinalis et rel.   "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides ovatus et rel.         "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides plebeius et rel.       "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides splachnicus et rel.    "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides stercoris et rel.      "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides uniformis et rel.      "Bacteroidetes"  "Bacteroidetes" 
#> Bacteroides vulgatus et rel.       "Bacteroidetes"  "Bacteroidetes" 
#> Parabacteroides distasonis et rel. "Bacteroidetes"  "Bacteroidetes" 
#> Prevotella melaninogenica et rel.  "Bacteroidetes"  "Bacteroidetes" 
#> Prevotella oralis et rel.          "Bacteroidetes"  "Bacteroidetes" 
#> Prevotella ruminicola et rel.      "Bacteroidetes"  "Bacteroidetes" 
#> Prevotella tannerae et rel.        "Bacteroidetes"  "Bacteroidetes" 
#> Tannerella et rel.                 "Bacteroidetes"  "Bacteroidetes" 
#> Uncultured Bacteroidetes           "Bacteroidetes"  "Bacteroidetes" 
#> Uncultured Chroococcales           "Cyanobacteria"  "Cyanobacteria" 
#> Asteroleplasma et rel.             "Firmicutes"     "Asteroleplasma"
#> Aerococcus                         "Firmicutes"     "Bacilli"       
#> Aneurinibacillus                   "Firmicutes"     "Bacilli"       
#> Bacillus                           "Firmicutes"     "Bacilli"       
#> Enterococcus                       "Firmicutes"     "Bacilli"       
#>                                    Genus                               
#> Actinomycetaceae                   "Actinomycetaceae"                  
#> Atopobium                          "Atopobium"                         
#> Bifidobacterium                    "Bifidobacterium"                   
#> Collinsella                        "Collinsella"                       
#> Corynebacterium                    "Corynebacterium"                   
#> Eggerthella lenta et rel.          "Eggerthella lenta et rel."         
#> Micrococcaceae                     "Micrococcaceae"                    
#> Propionibacterium                  "Propionibacterium"                 
#> Allistipes et rel.                 "Allistipes et rel."                
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Parabacteroides distasonis et rel. "Parabacteroides distasonis et rel."
#> Prevotella melaninogenica et rel.  "Prevotella melaninogenica et rel." 
#> Prevotella oralis et rel.          "Prevotella oralis et rel."         
#> Prevotella ruminicola et rel.      "Prevotella ruminicola et rel."     
#> Prevotella tannerae et rel.        "Prevotella tannerae et rel."       
#> Tannerella et rel.                 "Tannerella et rel."                
#> Uncultured Bacteroidetes           "Uncultured Bacteroidetes"          
#> Uncultured Chroococcales           "Uncultured Chroococcales"          
#> Asteroleplasma et rel.             "Asteroleplasma et rel."            
#> Aerococcus                         "Aerococcus"                        
#> Aneurinibacillus                   "Aneurinibacillus"                  
#> Bacillus                           "Bacillus"                          
#> Enterococcus                       "Enterococcus"                      

# sort by function e.g. total sum or median abundance
dietswap %>%
  tax_sort(by = sum) %>%
  taxa_names() %>%
  head(20)
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
#> [11] "Subdoligranulum variable at rel."    
#> [12] "Ruminococcus obeum et rel."          
#> [13] "Butyrivibrio crossotus et rel."      
#> [14] "Bacteroides fragilis et rel."        
#> [15] "Akkermansia"                         
#> [16] "Bacteroides ovatus et rel."          
#> [17] "Parabacteroides distasonis et rel."  
#> [18] "Dorea formicigenerans et rel."       
#> [19] "Bacteroides uniformis et rel."       
#> [20] "Dialister"                           

# transform to compositional data (proportions) before sorting
# note that abundances are returned untransformed
dietswap %>%
  tax_sort(by = sum, trans = "compositional") %>%
  taxa_names() %>%
  head(20)
#>  [1] "Prevotella melaninogenica et rel."   
#>  [2] "Oscillospira guillermondii et rel."  
#>  [3] "Bacteroides vulgatus et rel."        
#>  [4] "Clostridium cellulosi et rel."       
#>  [5] "Prevotella oralis et rel."           
#>  [6] "Faecalibacterium prausnitzii et rel."
#>  [7] "Sporobacter termitidis et rel."      
#>  [8] "Allistipes et rel."                  
#>  [9] "Clostridium symbiosum et rel."       
#> [10] "Clostridium orbiscindens et rel."    
#> [11] "Ruminococcus obeum et rel."          
#> [12] "Subdoligranulum variable at rel."    
#> [13] "Bacteroides fragilis et rel."        
#> [14] "Butyrivibrio crossotus et rel."      
#> [15] "Akkermansia"                         
#> [16] "Parabacteroides distasonis et rel."  
#> [17] "Bacteroides ovatus et rel."          
#> [18] "Bacteroides uniformis et rel."       
#> [19] "Dialister"                           
#> [20] "Dorea formicigenerans et rel."       

# order by descending abundance in a single named sample
dietswap %>%
  tax_sort(by = "Sample-1") %>%
  otu_table() %>%
  .[1:8, 1:4]
#> OTU Table:          [4 taxa and 8 samples]
#>                      taxa are columns
#>          Bacteroides vulgatus et rel. Oscillospira guillermondii et rel.
#> Sample-1                         2774                                861
#> Sample-2                           48                               4697
#> Sample-3                          455                               2915
#> Sample-4                          469                               4279
#> Sample-5                          167                                392
#> Sample-6                           75                                200
#> Sample-7                           33                               2201
#> Sample-8                           85                               1123
#>          Bacteroides fragilis et rel. Bacteroides ovatus et rel.
#> Sample-1                          443                        427
#> Sample-2                           21                         15
#> Sample-3                           73                         31
#> Sample-4                           29                         46
#> Sample-5                           33                         29
#> Sample-6                           14                         13
#> Sample-7                           22                         15
#> Sample-8                          560                         19


# sum order should always equal mean order if non-negative abundances
# don't forget to add na.rm = TRUE if you expect NAs in otu_table somehow
dietswap %>%
  tax_sort(by = sum, na.rm = TRUE) %>%
  taxa_names() %>%
  head(20)
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
#> [11] "Subdoligranulum variable at rel."    
#> [12] "Ruminococcus obeum et rel."          
#> [13] "Butyrivibrio crossotus et rel."      
#> [14] "Bacteroides fragilis et rel."        
#> [15] "Akkermansia"                         
#> [16] "Bacteroides ovatus et rel."          
#> [17] "Parabacteroides distasonis et rel."  
#> [18] "Dorea formicigenerans et rel."       
#> [19] "Bacteroides uniformis et rel."       
#> [20] "Dialister"                           

# if your phyloseq object has a phylogenetic tree,
# tax_sort will remove the tree, and warn you about this
# unless you disable that warning with tree_warn = FALSE

# You can sort by abundance at higher taxonomic ranks,
# without losing lower rank info
# e.g. sort (descending) by phyla abundances
dietswap %>%
  tax_sort(by = sum, at = "Phylum") %>%
  tax_table() %>%
  head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                                  Phylum          Family         
#> Allistipes et rel.               "Bacteroidetes" "Bacteroidetes"
#> Bacteroides fragilis et rel.     "Bacteroidetes" "Bacteroidetes"
#> Bacteroides intestinalis et rel. "Bacteroidetes" "Bacteroidetes"
#> Bacteroides ovatus et rel.       "Bacteroidetes" "Bacteroidetes"
#> Bacteroides plebeius et rel.     "Bacteroidetes" "Bacteroidetes"
#> Bacteroides splachnicus et rel.  "Bacteroidetes" "Bacteroidetes"
#>                                  Genus                             
#> Allistipes et rel.               "Allistipes et rel."              
#> Bacteroides fragilis et rel.     "Bacteroides fragilis et rel."    
#> Bacteroides intestinalis et rel. "Bacteroides intestinalis et rel."
#> Bacteroides ovatus et rel.       "Bacteroides ovatus et rel."      
#> Bacteroides plebeius et rel.     "Bacteroides plebeius et rel."    
#> Bacteroides splachnicus et rel.  "Bacteroides splachnicus et rel." 

# You can sort by ascending abundance (or prevalence etc) by reversing after
dietswap %>%
  tax_sort(by = "prev", at = "Phylum") %>%
  tax_sort(by = "rev") %>%
  tax_table() %>%
  head()
#> Taxonomy Table:     [6 taxa by 3 taxonomic ranks]:
#>                             Phylum          Family         
#> Uncultured Chroococcales    "Cyanobacteria" "Cyanobacteria"
#> Brachyspira                 "Spirochaetes"  "Spirochaetes" 
#> Fusobacteria                "Fusobacteria"  "Fusobacteria" 
#> Uncultured Bacteroidetes    "Bacteroidetes" "Bacteroidetes"
#> Tannerella et rel.          "Bacteroidetes" "Bacteroidetes"
#> Prevotella tannerae et rel. "Bacteroidetes" "Bacteroidetes"
#>                             Genus                        
#> Uncultured Chroococcales    "Uncultured Chroococcales"   
#> Brachyspira                 "Brachyspira"                
#> Fusobacteria                "Fusobacteria"               
#> Uncultured Bacteroidetes    "Uncultured Bacteroidetes"   
#> Tannerella et rel.          "Tannerella et rel."         
#> Prevotella tannerae et rel. "Prevotella tannerae et rel."
```
