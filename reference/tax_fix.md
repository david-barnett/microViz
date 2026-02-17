# Replace unknown, NA, or short tax_table values

Identifies phyloseq tax_table values as unknown or uninformative and
replaces them with the first informative value from a higher taxonomic
rank.

- Short values in phyloseq tax_table are typically empty strings or " ",
  or "g\_\_" etc. so it is helpful to replace them. (If this is
  unwanted: set `min_length` = 0 to avoid filtering on length.)

- Values in `unknowns` are also removed, even if longer than
  `min_length`. It is up to the user to specify sensible values in
  `unknowns` if their dataset has other unwanted values.

- NA values are also replaced.

See this article for an extended discussion of tax_table fixing.
<https://david-barnett.github.io/microViz/articles/web-only/tax-fixing.html>

## Usage

``` r
tax_fix(
  ps,
  min_length = 4,
  unknowns = NA,
  suffix_rank = "classified",
  sep = " ",
  anon_unique = TRUE,
  verbose = TRUE
)
```

## Arguments

- ps:

  phyloseq or tax_table (taxonomyTable)

- min_length:

  replace strings shorter than this, must be integer \> 0

- unknowns:

  also replace strings matching any in this vector, NA default vector
  shown in details!

- suffix_rank:

  "classified" (default) or "current", when replacing an entry, should
  the suffix be taken from the lowest classified rank for that taxon,
  "classified", or the "current" unclassified rank?

- sep:

  character(s) separating new name and taxonomic rank level suffix (see
  suffix_rank)

- anon_unique:

  make anonymous taxa unique by replacing unknowns with taxa_name?
  otherwise they are replaced with paste("unknown", first_rank_name),
  which is therefore the same for every anonymous taxon, meaning they
  will be merged if tax_agg is used. (anonymous taxa are taxa with all
  unknown values in their tax_table row, i.e. cannot be classified even
  at highest rank available)

- verbose:

  emit warnings when cannot replace with informative name?

## Value

object same class as ps

## Details

By default (unknowns = NA), unknowns is set to a vector containing:

's\_\_' 'g\_\_' 'f\_\_' 'o\_\_' 'c\_\_' 'p\_\_' 'k\_\_' 'S\_\_' 'G\_\_'
'F\_\_' 'O\_\_' 'C\_\_' 'P\_\_' 'K\_\_' 'NA' 'NaN' ' ' ” 'unknown'
'Unknown' 's\_\_unknown' 's\_\_Unknown' 's\_\_NA' 'g\_\_unknown'
'g\_\_Unknown' 'g\_\_NA' 'f\_\_unknown' 'f\_\_Unknown' 'f\_\_NA'
'o\_\_unknown' 'o\_\_Unknown' 'o\_\_NA' 'c\_\_unknown' 'c\_\_Unknown'
'c\_\_NA' 'p\_\_unknown' 'p\_\_Unknown' 'p\_\_NA' 'k\_\_unknown'
'k\_\_Unknown' 'k\_\_NA' 'S\_\_unknown' 'S\_\_Unknown' 'S\_\_NA'
'G\_\_unknown' 'G\_\_Unknown' 'G\_\_NA' 'F\_\_unknown' 'F\_\_Unknown'
'F\_\_NA' 'O\_\_unknown' 'O\_\_Unknown' 'O\_\_NA' 'C\_\_unknown'
'C\_\_Unknown' 'C\_\_NA' 'P\_\_unknown' 'P\_\_Unknown' 'P\_\_NA'
'K\_\_unknown' 'K\_\_Unknown' 'K\_\_NA'

## See also

[`tax_fix_interactive`](https://david-barnett.github.io/microViz/reference/tax_fix_interactive.md)
for interactive tax_fix help

## Examples

``` r
library(dplyr)
library(phyloseq)

data(dietswap, package = "microbiome")
ps <- dietswap

# create unknowns to test filling
tt <- tax_table(ps)
ntax <- ntaxa(ps)
set.seed(123)
g <- sample(1:ntax, 30)
f <- sample(g, 10)
p <- sample(f, 3)
tt[g, 3] <- "g__"
tt[f, 2] <- "f__"
tt[p, 1] <- "p__"
tt[sample(1:ntax, 10), 3] <- "unknown"
# create a row with only NAs
tt[1, ] <- NA

tax_table(ps) <- tax_table(tt)

ps
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
# tax_fix with defaults should solve most problems
tax_table(ps) %>% head(50)
#> Taxonomy Table:     [50 taxa by 3 taxonomic ranks]:
#>                                    Phylum           
#> Actinomycetaceae                   NA               
#> Aerococcus                         "Firmicutes"     
#> Aeromonas                          "Proteobacteria" 
#> Akkermansia                        "Verrucomicrobia"
#> Alcaligenes faecalis et rel.       "Proteobacteria" 
#> Allistipes et rel.                 "Bacteroidetes"  
#> Anaerobiospirillum                 "Proteobacteria" 
#> Anaerofustis                       "Firmicutes"     
#> Anaerostipes caccae et rel.        "Firmicutes"     
#> Anaerotruncus colihominis et rel.  "Firmicutes"     
#> Anaerovorax odorimutans et rel.    "Firmicutes"     
#> Aneurinibacillus                   "Firmicutes"     
#> Aquabacterium                      "Proteobacteria" 
#> Asteroleplasma et rel.             "Firmicutes"     
#> Atopobium                          "Actinobacteria" 
#> Bacillus                           "Firmicutes"     
#> Bacteroides fragilis et rel.       "Bacteroidetes"  
#> Bacteroides intestinalis et rel.   "Bacteroidetes"  
#> Bacteroides ovatus et rel.         "Bacteroidetes"  
#> Bacteroides plebeius et rel.       "Bacteroidetes"  
#> Bacteroides splachnicus et rel.    "Bacteroidetes"  
#> Bacteroides stercoris et rel.      "Bacteroidetes"  
#> Bacteroides uniformis et rel.      "Bacteroidetes"  
#> Bacteroides vulgatus et rel.       "Bacteroidetes"  
#> Bifidobacterium                    "p__"            
#> Bilophila et rel.                  "Proteobacteria" 
#> Brachyspira                        "Spirochaetes"   
#> Bryantella formatexigens et rel.   "Firmicutes"     
#> Bulleidia moorei et rel.           "Firmicutes"     
#> Burkholderia                       "Proteobacteria" 
#> Butyrivibrio crossotus et rel.     "Firmicutes"     
#> Campylobacter                      "Proteobacteria" 
#> Catenibacterium mitsuokai et rel.  "Firmicutes"     
#> Clostridium (sensu stricto)        "Firmicutes"     
#> Clostridium cellulosi et rel.      "Firmicutes"     
#> Clostridium colinum et rel.        "p__"            
#> Clostridium difficile et rel.      "Firmicutes"     
#> Clostridium felsineum et rel.      "Firmicutes"     
#> Clostridium leptum et rel.         "Firmicutes"     
#> Clostridium nexile et rel.         "Firmicutes"     
#> Clostridium orbiscindens et rel.   "Firmicutes"     
#> Clostridium ramosum et rel.        "Firmicutes"     
#> Clostridium sphenoides et rel.     "Firmicutes"     
#> Clostridium stercorarium et rel.   "Firmicutes"     
#> Clostridium symbiosum et rel.      "Firmicutes"     
#> Clostridium thermocellum et rel.   "Firmicutes"     
#> Collinsella                        "Actinobacteria" 
#> Coprobacillus catenaformis et rel. "Firmicutes"     
#> Coprococcus eutactus et rel.       "Firmicutes"     
#> Corynebacterium                    "Actinobacteria" 
#>                                    Family                     
#> Actinomycetaceae                   NA                         
#> Aerococcus                         "Bacilli"                  
#> Aeromonas                          "Proteobacteria"           
#> Akkermansia                        "Verrucomicrobia"          
#> Alcaligenes faecalis et rel.       "Proteobacteria"           
#> Allistipes et rel.                 "Bacteroidetes"            
#> Anaerobiospirillum                 "Proteobacteria"           
#> Anaerofustis                       "Clostridium cluster XV"   
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa" 
#> Anaerotruncus colihominis et rel.  "Clostridium cluster IV"   
#> Anaerovorax odorimutans et rel.    "Clostridium cluster XI"   
#> Aneurinibacillus                   "Bacilli"                  
#> Aquabacterium                      "Proteobacteria"           
#> Asteroleplasma et rel.             "Asteroleplasma"           
#> Atopobium                          "Actinobacteria"           
#> Bacillus                           "Bacilli"                  
#> Bacteroides fragilis et rel.       "Bacteroidetes"            
#> Bacteroides intestinalis et rel.   "Bacteroidetes"            
#> Bacteroides ovatus et rel.         "Bacteroidetes"            
#> Bacteroides plebeius et rel.       "Bacteroidetes"            
#> Bacteroides splachnicus et rel.    "Bacteroidetes"            
#> Bacteroides stercoris et rel.      "Bacteroidetes"            
#> Bacteroides uniformis et rel.      "Bacteroidetes"            
#> Bacteroides vulgatus et rel.       "Bacteroidetes"            
#> Bifidobacterium                    "f__"                      
#> Bilophila et rel.                  "f__"                      
#> Brachyspira                        "Spirochaetes"             
#> Bryantella formatexigens et rel.   "Clostridium cluster XIVa" 
#> Bulleidia moorei et rel.           "Clostridium cluster XVI"  
#> Burkholderia                       "Proteobacteria"           
#> Butyrivibrio crossotus et rel.     "Clostridium cluster XIVa" 
#> Campylobacter                      "Proteobacteria"           
#> Catenibacterium mitsuokai et rel.  "Clostridium cluster XVII" 
#> Clostridium (sensu stricto)        "Clostridium cluster I"    
#> Clostridium cellulosi et rel.      "Clostridium cluster IV"   
#> Clostridium colinum et rel.        "f__"                      
#> Clostridium difficile et rel.      "Clostridium cluster XI"   
#> Clostridium felsineum et rel.      "Clostridium cluster XI"   
#> Clostridium leptum et rel.         "Clostridium cluster IV"   
#> Clostridium nexile et rel.         "Clostridium cluster XIVa" 
#> Clostridium orbiscindens et rel.   "Clostridium cluster IV"   
#> Clostridium ramosum et rel.        "f__"                      
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa" 
#> Clostridium stercorarium et rel.   "Clostridium cluster III"  
#> Clostridium symbiosum et rel.      "Clostridium cluster XIVa" 
#> Clostridium thermocellum et rel.   "Clostridium cluster III"  
#> Collinsella                        "Actinobacteria"           
#> Coprobacillus catenaformis et rel. "Clostridium cluster XVIII"
#> Coprococcus eutactus et rel.       "Clostridium cluster XIVa" 
#> Corynebacterium                    "Actinobacteria"           
#>                                    Genus                               
#> Actinomycetaceae                   NA                                  
#> Aerococcus                         "Aerococcus"                        
#> Aeromonas                          "Aeromonas"                         
#> Akkermansia                        "Akkermansia"                       
#> Alcaligenes faecalis et rel.       "Alcaligenes faecalis et rel."      
#> Allistipes et rel.                 "Allistipes et rel."                
#> Anaerobiospirillum                 "g__"                               
#> Anaerofustis                       "Anaerofustis"                      
#> Anaerostipes caccae et rel.        "g__"                               
#> Anaerotruncus colihominis et rel.  "Anaerotruncus colihominis et rel." 
#> Anaerovorax odorimutans et rel.    "Anaerovorax odorimutans et rel."   
#> Aneurinibacillus                   "Aneurinibacillus"                  
#> Aquabacterium                      "unknown"                           
#> Asteroleplasma et rel.             "g__"                               
#> Atopobium                          "Atopobium"                         
#> Bacillus                           "Bacillus"                          
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Bifidobacterium                    "g__"                               
#> Bilophila et rel.                  "g__"                               
#> Brachyspira                        "Brachyspira"                       
#> Bryantella formatexigens et rel.   "Bryantella formatexigens et rel."  
#> Bulleidia moorei et rel.           "Bulleidia moorei et rel."          
#> Burkholderia                       "Burkholderia"                      
#> Butyrivibrio crossotus et rel.     "Butyrivibrio crossotus et rel."    
#> Campylobacter                      "Campylobacter"                     
#> Catenibacterium mitsuokai et rel.  "Catenibacterium mitsuokai et rel." 
#> Clostridium (sensu stricto)        "unknown"                           
#> Clostridium cellulosi et rel.      "Clostridium cellulosi et rel."     
#> Clostridium colinum et rel.        "g__"                               
#> Clostridium difficile et rel.      "Clostridium difficile et rel."     
#> Clostridium felsineum et rel.      "Clostridium felsineum et rel."     
#> Clostridium leptum et rel.         "Clostridium leptum et rel."        
#> Clostridium nexile et rel.         "Clostridium nexile et rel."        
#> Clostridium orbiscindens et rel.   "Clostridium orbiscindens et rel."  
#> Clostridium ramosum et rel.        "g__"                               
#> Clostridium sphenoides et rel.     "g__"                               
#> Clostridium stercorarium et rel.   "Clostridium stercorarium et rel."  
#> Clostridium symbiosum et rel.      "Clostridium symbiosum et rel."     
#> Clostridium thermocellum et rel.   "Clostridium thermocellum et rel."  
#> Collinsella                        "Collinsella"                       
#> Coprobacillus catenaformis et rel. "Coprobacillus catenaformis et rel."
#> Coprococcus eutactus et rel.       "Coprococcus eutactus et rel."      
#> Corynebacterium                    "g__"                               

# this will replace "unknown"s as well as short values including "g__" and "f__"
tax_fix(ps) %>%
  tax_table() %>%
  head(50)
#> Row named: Actinomycetaceae
#> contains no non-unknown values, returning:
#> 'Actinomycetaceae' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Bifidobacterium
#> contains no non-unknown values, returning:
#> 'Bifidobacterium' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Clostridium colinum et rel.
#> contains no non-unknown values, returning:
#> 'Clostridium colinum et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Escherichia coli et rel.
#> contains no non-unknown values, returning:
#> 'Escherichia coli et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Taxonomy Table:     [50 taxa by 3 taxonomic ranks]:
#>                                    Phylum                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Firmicutes"                 
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Firmicutes"                 
#> Anaerostipes caccae et rel.        "Firmicutes"                 
#> Anaerotruncus colihominis et rel.  "Firmicutes"                 
#> Anaerovorax odorimutans et rel.    "Firmicutes"                 
#> Aneurinibacillus                   "Firmicutes"                 
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Firmicutes"                 
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Firmicutes"                 
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria"             
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Firmicutes"                 
#> Bulleidia moorei et rel.           "Firmicutes"                 
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Firmicutes"                 
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Firmicutes"                 
#> Clostridium (sensu stricto)        "Firmicutes"                 
#> Clostridium cellulosi et rel.      "Firmicutes"                 
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Firmicutes"                 
#> Clostridium felsineum et rel.      "Firmicutes"                 
#> Clostridium leptum et rel.         "Firmicutes"                 
#> Clostridium nexile et rel.         "Firmicutes"                 
#> Clostridium orbiscindens et rel.   "Firmicutes"                 
#> Clostridium ramosum et rel.        "Firmicutes"                 
#> Clostridium sphenoides et rel.     "Firmicutes"                 
#> Clostridium stercorarium et rel.   "Firmicutes"                 
#> Clostridium symbiosum et rel.      "Firmicutes"                 
#> Clostridium thermocellum et rel.   "Firmicutes"                 
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Firmicutes"                 
#> Coprococcus eutactus et rel.       "Firmicutes"                 
#> Corynebacterium                    "Actinobacteria"             
#>                                    Family                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Bacilli"                    
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Clostridium cluster XV"     
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa"   
#> Anaerotruncus colihominis et rel.  "Clostridium cluster IV"     
#> Anaerovorax odorimutans et rel.    "Clostridium cluster XI"     
#> Aneurinibacillus                   "Bacilli"                    
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Asteroleplasma"             
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Bacilli"                    
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria Phylum"      
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Clostridium cluster XIVa"   
#> Bulleidia moorei et rel.           "Clostridium cluster XVI"    
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Clostridium cluster XIVa"   
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Clostridium cluster XVII"   
#> Clostridium (sensu stricto)        "Clostridium cluster I"      
#> Clostridium cellulosi et rel.      "Clostridium cluster IV"     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Clostridium cluster XI"     
#> Clostridium felsineum et rel.      "Clostridium cluster XI"     
#> Clostridium leptum et rel.         "Clostridium cluster IV"     
#> Clostridium nexile et rel.         "Clostridium cluster XIVa"   
#> Clostridium orbiscindens et rel.   "Clostridium cluster IV"     
#> Clostridium ramosum et rel.        "Firmicutes Phylum"          
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa"   
#> Clostridium stercorarium et rel.   "Clostridium cluster III"    
#> Clostridium symbiosum et rel.      "Clostridium cluster XIVa"   
#> Clostridium thermocellum et rel.   "Clostridium cluster III"    
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Clostridium cluster XVIII"  
#> Coprococcus eutactus et rel.       "Clostridium cluster XIVa"   
#> Corynebacterium                    "Actinobacteria"             
#>                                    Genus                               
#> Actinomycetaceae                   "Actinomycetaceae"                  
#> Aerococcus                         "Aerococcus"                        
#> Aeromonas                          "Aeromonas"                         
#> Akkermansia                        "Akkermansia"                       
#> Alcaligenes faecalis et rel.       "Alcaligenes faecalis et rel."      
#> Allistipes et rel.                 "Allistipes et rel."                
#> Anaerobiospirillum                 "Proteobacteria Family"             
#> Anaerofustis                       "Anaerofustis"                      
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa Family"   
#> Anaerotruncus colihominis et rel.  "Anaerotruncus colihominis et rel." 
#> Anaerovorax odorimutans et rel.    "Anaerovorax odorimutans et rel."   
#> Aneurinibacillus                   "Aneurinibacillus"                  
#> Aquabacterium                      "Proteobacteria Family"             
#> Asteroleplasma et rel.             "Asteroleplasma Family"             
#> Atopobium                          "Atopobium"                         
#> Bacillus                           "Bacillus"                          
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Bifidobacterium                    "Bifidobacterium"                   
#> Bilophila et rel.                  "Proteobacteria Phylum"             
#> Brachyspira                        "Brachyspira"                       
#> Bryantella formatexigens et rel.   "Bryantella formatexigens et rel."  
#> Bulleidia moorei et rel.           "Bulleidia moorei et rel."          
#> Burkholderia                       "Burkholderia"                      
#> Butyrivibrio crossotus et rel.     "Butyrivibrio crossotus et rel."    
#> Campylobacter                      "Campylobacter"                     
#> Catenibacterium mitsuokai et rel.  "Catenibacterium mitsuokai et rel." 
#> Clostridium (sensu stricto)        "Clostridium cluster I Family"      
#> Clostridium cellulosi et rel.      "Clostridium cellulosi et rel."     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."       
#> Clostridium difficile et rel.      "Clostridium difficile et rel."     
#> Clostridium felsineum et rel.      "Clostridium felsineum et rel."     
#> Clostridium leptum et rel.         "Clostridium leptum et rel."        
#> Clostridium nexile et rel.         "Clostridium nexile et rel."        
#> Clostridium orbiscindens et rel.   "Clostridium orbiscindens et rel."  
#> Clostridium ramosum et rel.        "Firmicutes Phylum"                 
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa Family"   
#> Clostridium stercorarium et rel.   "Clostridium stercorarium et rel."  
#> Clostridium symbiosum et rel.      "Clostridium symbiosum et rel."     
#> Clostridium thermocellum et rel.   "Clostridium thermocellum et rel."  
#> Collinsella                        "Collinsella"                       
#> Coprobacillus catenaformis et rel. "Coprobacillus catenaformis et rel."
#> Coprococcus eutactus et rel.       "Coprococcus eutactus et rel."      
#> Corynebacterium                    "Actinobacteria Family"             

# This will only replace short entries, and so won't replace literal "unknown" values
ps %>%
  tax_fix(unknowns = NULL) %>%
  tax_table() %>%
  head(50)
#> Row named: Actinomycetaceae
#> contains no non-unknown values, returning:
#> 'Actinomycetaceae' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Bifidobacterium
#> contains no non-unknown values, returning:
#> 'Bifidobacterium' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Clostridium colinum et rel.
#> contains no non-unknown values, returning:
#> 'Clostridium colinum et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Escherichia coli et rel.
#> contains no non-unknown values, returning:
#> 'Escherichia coli et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Taxonomy Table:     [50 taxa by 3 taxonomic ranks]:
#>                                    Phylum                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Firmicutes"                 
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Firmicutes"                 
#> Anaerostipes caccae et rel.        "Firmicutes"                 
#> Anaerotruncus colihominis et rel.  "Firmicutes"                 
#> Anaerovorax odorimutans et rel.    "Firmicutes"                 
#> Aneurinibacillus                   "Firmicutes"                 
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Firmicutes"                 
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Firmicutes"                 
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria"             
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Firmicutes"                 
#> Bulleidia moorei et rel.           "Firmicutes"                 
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Firmicutes"                 
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Firmicutes"                 
#> Clostridium (sensu stricto)        "Firmicutes"                 
#> Clostridium cellulosi et rel.      "Firmicutes"                 
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Firmicutes"                 
#> Clostridium felsineum et rel.      "Firmicutes"                 
#> Clostridium leptum et rel.         "Firmicutes"                 
#> Clostridium nexile et rel.         "Firmicutes"                 
#> Clostridium orbiscindens et rel.   "Firmicutes"                 
#> Clostridium ramosum et rel.        "Firmicutes"                 
#> Clostridium sphenoides et rel.     "Firmicutes"                 
#> Clostridium stercorarium et rel.   "Firmicutes"                 
#> Clostridium symbiosum et rel.      "Firmicutes"                 
#> Clostridium thermocellum et rel.   "Firmicutes"                 
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Firmicutes"                 
#> Coprococcus eutactus et rel.       "Firmicutes"                 
#> Corynebacterium                    "Actinobacteria"             
#>                                    Family                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Bacilli"                    
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Clostridium cluster XV"     
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa"   
#> Anaerotruncus colihominis et rel.  "Clostridium cluster IV"     
#> Anaerovorax odorimutans et rel.    "Clostridium cluster XI"     
#> Aneurinibacillus                   "Bacilli"                    
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Asteroleplasma"             
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Bacilli"                    
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria Phylum"      
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Clostridium cluster XIVa"   
#> Bulleidia moorei et rel.           "Clostridium cluster XVI"    
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Clostridium cluster XIVa"   
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Clostridium cluster XVII"   
#> Clostridium (sensu stricto)        "Clostridium cluster I"      
#> Clostridium cellulosi et rel.      "Clostridium cluster IV"     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Clostridium cluster XI"     
#> Clostridium felsineum et rel.      "Clostridium cluster XI"     
#> Clostridium leptum et rel.         "Clostridium cluster IV"     
#> Clostridium nexile et rel.         "Clostridium cluster XIVa"   
#> Clostridium orbiscindens et rel.   "Clostridium cluster IV"     
#> Clostridium ramosum et rel.        "Firmicutes Phylum"          
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa"   
#> Clostridium stercorarium et rel.   "Clostridium cluster III"    
#> Clostridium symbiosum et rel.      "Clostridium cluster XIVa"   
#> Clostridium thermocellum et rel.   "Clostridium cluster III"    
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Clostridium cluster XVIII"  
#> Coprococcus eutactus et rel.       "Clostridium cluster XIVa"   
#> Corynebacterium                    "Actinobacteria"             
#>                                    Genus                               
#> Actinomycetaceae                   "Actinomycetaceae"                  
#> Aerococcus                         "Aerococcus"                        
#> Aeromonas                          "Aeromonas"                         
#> Akkermansia                        "Akkermansia"                       
#> Alcaligenes faecalis et rel.       "Alcaligenes faecalis et rel."      
#> Allistipes et rel.                 "Allistipes et rel."                
#> Anaerobiospirillum                 "Proteobacteria Family"             
#> Anaerofustis                       "Anaerofustis"                      
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa Family"   
#> Anaerotruncus colihominis et rel.  "Anaerotruncus colihominis et rel." 
#> Anaerovorax odorimutans et rel.    "Anaerovorax odorimutans et rel."   
#> Aneurinibacillus                   "Aneurinibacillus"                  
#> Aquabacterium                      "unknown"                           
#> Asteroleplasma et rel.             "Asteroleplasma Family"             
#> Atopobium                          "Atopobium"                         
#> Bacillus                           "Bacillus"                          
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Bifidobacterium                    "Bifidobacterium"                   
#> Bilophila et rel.                  "Proteobacteria Phylum"             
#> Brachyspira                        "Brachyspira"                       
#> Bryantella formatexigens et rel.   "Bryantella formatexigens et rel."  
#> Bulleidia moorei et rel.           "Bulleidia moorei et rel."          
#> Burkholderia                       "Burkholderia"                      
#> Butyrivibrio crossotus et rel.     "Butyrivibrio crossotus et rel."    
#> Campylobacter                      "Campylobacter"                     
#> Catenibacterium mitsuokai et rel.  "Catenibacterium mitsuokai et rel." 
#> Clostridium (sensu stricto)        "unknown"                           
#> Clostridium cellulosi et rel.      "Clostridium cellulosi et rel."     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."       
#> Clostridium difficile et rel.      "Clostridium difficile et rel."     
#> Clostridium felsineum et rel.      "Clostridium felsineum et rel."     
#> Clostridium leptum et rel.         "Clostridium leptum et rel."        
#> Clostridium nexile et rel.         "Clostridium nexile et rel."        
#> Clostridium orbiscindens et rel.   "Clostridium orbiscindens et rel."  
#> Clostridium ramosum et rel.        "Firmicutes Phylum"                 
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa Family"   
#> Clostridium stercorarium et rel.   "Clostridium stercorarium et rel."  
#> Clostridium symbiosum et rel.      "Clostridium symbiosum et rel."     
#> Clostridium thermocellum et rel.   "Clostridium thermocellum et rel."  
#> Collinsella                        "Collinsella"                       
#> Coprobacillus catenaformis et rel. "Coprobacillus catenaformis et rel."
#> Coprococcus eutactus et rel.       "Coprococcus eutactus et rel."      
#> Corynebacterium                    "Actinobacteria Family"             

# Change rank suffix and separator settings
tax_fix(ps, suffix_rank = "current", sep = " - ") %>%
  tax_table() %>%
  head(50)
#> Row named: Actinomycetaceae
#> contains no non-unknown values, returning:
#> 'Actinomycetaceae' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Bifidobacterium
#> contains no non-unknown values, returning:
#> 'Bifidobacterium' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Clostridium colinum et rel.
#> contains no non-unknown values, returning:
#> 'Clostridium colinum et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Escherichia coli et rel.
#> contains no non-unknown values, returning:
#> 'Escherichia coli et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Taxonomy Table:     [50 taxa by 3 taxonomic ranks]:
#>                                    Phylum                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Firmicutes"                 
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Firmicutes"                 
#> Anaerostipes caccae et rel.        "Firmicutes"                 
#> Anaerotruncus colihominis et rel.  "Firmicutes"                 
#> Anaerovorax odorimutans et rel.    "Firmicutes"                 
#> Aneurinibacillus                   "Firmicutes"                 
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Firmicutes"                 
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Firmicutes"                 
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria"             
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Firmicutes"                 
#> Bulleidia moorei et rel.           "Firmicutes"                 
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Firmicutes"                 
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Firmicutes"                 
#> Clostridium (sensu stricto)        "Firmicutes"                 
#> Clostridium cellulosi et rel.      "Firmicutes"                 
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Firmicutes"                 
#> Clostridium felsineum et rel.      "Firmicutes"                 
#> Clostridium leptum et rel.         "Firmicutes"                 
#> Clostridium nexile et rel.         "Firmicutes"                 
#> Clostridium orbiscindens et rel.   "Firmicutes"                 
#> Clostridium ramosum et rel.        "Firmicutes"                 
#> Clostridium sphenoides et rel.     "Firmicutes"                 
#> Clostridium stercorarium et rel.   "Firmicutes"                 
#> Clostridium symbiosum et rel.      "Firmicutes"                 
#> Clostridium thermocellum et rel.   "Firmicutes"                 
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Firmicutes"                 
#> Coprococcus eutactus et rel.       "Firmicutes"                 
#> Corynebacterium                    "Actinobacteria"             
#>                                    Family                       
#> Actinomycetaceae                   "Actinomycetaceae"           
#> Aerococcus                         "Bacilli"                    
#> Aeromonas                          "Proteobacteria"             
#> Akkermansia                        "Verrucomicrobia"            
#> Alcaligenes faecalis et rel.       "Proteobacteria"             
#> Allistipes et rel.                 "Bacteroidetes"              
#> Anaerobiospirillum                 "Proteobacteria"             
#> Anaerofustis                       "Clostridium cluster XV"     
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa"   
#> Anaerotruncus colihominis et rel.  "Clostridium cluster IV"     
#> Anaerovorax odorimutans et rel.    "Clostridium cluster XI"     
#> Aneurinibacillus                   "Bacilli"                    
#> Aquabacterium                      "Proteobacteria"             
#> Asteroleplasma et rel.             "Asteroleplasma"             
#> Atopobium                          "Actinobacteria"             
#> Bacillus                           "Bacilli"                    
#> Bacteroides fragilis et rel.       "Bacteroidetes"              
#> Bacteroides intestinalis et rel.   "Bacteroidetes"              
#> Bacteroides ovatus et rel.         "Bacteroidetes"              
#> Bacteroides plebeius et rel.       "Bacteroidetes"              
#> Bacteroides splachnicus et rel.    "Bacteroidetes"              
#> Bacteroides stercoris et rel.      "Bacteroidetes"              
#> Bacteroides uniformis et rel.      "Bacteroidetes"              
#> Bacteroides vulgatus et rel.       "Bacteroidetes"              
#> Bifidobacterium                    "Bifidobacterium"            
#> Bilophila et rel.                  "Proteobacteria - Family"    
#> Brachyspira                        "Spirochaetes"               
#> Bryantella formatexigens et rel.   "Clostridium cluster XIVa"   
#> Bulleidia moorei et rel.           "Clostridium cluster XVI"    
#> Burkholderia                       "Proteobacteria"             
#> Butyrivibrio crossotus et rel.     "Clostridium cluster XIVa"   
#> Campylobacter                      "Proteobacteria"             
#> Catenibacterium mitsuokai et rel.  "Clostridium cluster XVII"   
#> Clostridium (sensu stricto)        "Clostridium cluster I"      
#> Clostridium cellulosi et rel.      "Clostridium cluster IV"     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."
#> Clostridium difficile et rel.      "Clostridium cluster XI"     
#> Clostridium felsineum et rel.      "Clostridium cluster XI"     
#> Clostridium leptum et rel.         "Clostridium cluster IV"     
#> Clostridium nexile et rel.         "Clostridium cluster XIVa"   
#> Clostridium orbiscindens et rel.   "Clostridium cluster IV"     
#> Clostridium ramosum et rel.        "Firmicutes - Family"        
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa"   
#> Clostridium stercorarium et rel.   "Clostridium cluster III"    
#> Clostridium symbiosum et rel.      "Clostridium cluster XIVa"   
#> Clostridium thermocellum et rel.   "Clostridium cluster III"    
#> Collinsella                        "Actinobacteria"             
#> Coprobacillus catenaformis et rel. "Clostridium cluster XVIII"  
#> Coprococcus eutactus et rel.       "Clostridium cluster XIVa"   
#> Corynebacterium                    "Actinobacteria"             
#>                                    Genus                               
#> Actinomycetaceae                   "Actinomycetaceae"                  
#> Aerococcus                         "Aerococcus"                        
#> Aeromonas                          "Aeromonas"                         
#> Akkermansia                        "Akkermansia"                       
#> Alcaligenes faecalis et rel.       "Alcaligenes faecalis et rel."      
#> Allistipes et rel.                 "Allistipes et rel."                
#> Anaerobiospirillum                 "Proteobacteria - Genus"            
#> Anaerofustis                       "Anaerofustis"                      
#> Anaerostipes caccae et rel.        "Clostridium cluster XIVa - Genus"  
#> Anaerotruncus colihominis et rel.  "Anaerotruncus colihominis et rel." 
#> Anaerovorax odorimutans et rel.    "Anaerovorax odorimutans et rel."   
#> Aneurinibacillus                   "Aneurinibacillus"                  
#> Aquabacterium                      "Proteobacteria - Genus"            
#> Asteroleplasma et rel.             "Asteroleplasma - Genus"            
#> Atopobium                          "Atopobium"                         
#> Bacillus                           "Bacillus"                          
#> Bacteroides fragilis et rel.       "Bacteroides fragilis et rel."      
#> Bacteroides intestinalis et rel.   "Bacteroides intestinalis et rel."  
#> Bacteroides ovatus et rel.         "Bacteroides ovatus et rel."        
#> Bacteroides plebeius et rel.       "Bacteroides plebeius et rel."      
#> Bacteroides splachnicus et rel.    "Bacteroides splachnicus et rel."   
#> Bacteroides stercoris et rel.      "Bacteroides stercoris et rel."     
#> Bacteroides uniformis et rel.      "Bacteroides uniformis et rel."     
#> Bacteroides vulgatus et rel.       "Bacteroides vulgatus et rel."      
#> Bifidobacterium                    "Bifidobacterium"                   
#> Bilophila et rel.                  "Proteobacteria - Genus"            
#> Brachyspira                        "Brachyspira"                       
#> Bryantella formatexigens et rel.   "Bryantella formatexigens et rel."  
#> Bulleidia moorei et rel.           "Bulleidia moorei et rel."          
#> Burkholderia                       "Burkholderia"                      
#> Butyrivibrio crossotus et rel.     "Butyrivibrio crossotus et rel."    
#> Campylobacter                      "Campylobacter"                     
#> Catenibacterium mitsuokai et rel.  "Catenibacterium mitsuokai et rel." 
#> Clostridium (sensu stricto)        "Clostridium cluster I - Genus"     
#> Clostridium cellulosi et rel.      "Clostridium cellulosi et rel."     
#> Clostridium colinum et rel.        "Clostridium colinum et rel."       
#> Clostridium difficile et rel.      "Clostridium difficile et rel."     
#> Clostridium felsineum et rel.      "Clostridium felsineum et rel."     
#> Clostridium leptum et rel.         "Clostridium leptum et rel."        
#> Clostridium nexile et rel.         "Clostridium nexile et rel."        
#> Clostridium orbiscindens et rel.   "Clostridium orbiscindens et rel."  
#> Clostridium ramosum et rel.        "Firmicutes - Genus"                
#> Clostridium sphenoides et rel.     "Clostridium cluster XIVa - Genus"  
#> Clostridium stercorarium et rel.   "Clostridium stercorarium et rel."  
#> Clostridium symbiosum et rel.      "Clostridium symbiosum et rel."     
#> Clostridium thermocellum et rel.   "Clostridium thermocellum et rel."  
#> Collinsella                        "Collinsella"                       
#> Coprobacillus catenaformis et rel. "Coprobacillus catenaformis et rel."
#> Coprococcus eutactus et rel.       "Coprococcus eutactus et rel."      
#> Corynebacterium                    "Actinobacteria - Genus"            

# by default, completely unclassified (anonymous) taxa are named by their
# taxa_names / rownames at all ranks.
# This makes anonymous taxa distinct from each other,
# and so they won't be merged on aggregation with tax_agg.
# If you think your anonymous taxa should merge on tax_agg,
# or you just want them to be named the all same for another reason,
# set anon_unique = FALSE (compare the warning messages)
tax_fix(ps, anon_unique = FALSE)
#> Row named: Actinomycetaceae
#> contains no non-unknown values, returning:
#> 'unclassified Phylum' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Bifidobacterium
#> contains no non-unknown values, returning:
#> 'unclassified Phylum' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Clostridium colinum et rel.
#> contains no non-unknown values, returning:
#> 'unclassified Phylum' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Escherichia coli et rel.
#> contains no non-unknown values, returning:
#> 'unclassified Phylum' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
tax_fix(ps, anon_unique = TRUE)
#> Row named: Actinomycetaceae
#> contains no non-unknown values, returning:
#> 'Actinomycetaceae' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Bifidobacterium
#> contains no non-unknown values, returning:
#> 'Bifidobacterium' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Clostridium colinum et rel.
#> contains no non-unknown values, returning:
#> 'Clostridium colinum et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> Row named: Escherichia coli et rel.
#> contains no non-unknown values, returning:
#> 'Escherichia coli et rel.' for all replaced levels.
#> Consider editing this tax_table entry manually.
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# here's a larger example tax_table shows its still fast with 1000s rows,
# from microbiomeutilities package
# library(microbiomeutilities)
# data("hmp2")
# system.time(tax_fix(hmp2, min_length = 1))
```
