# microbiome's dietswap data hasn't changed

    Code
      dietswap
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# tax_agg gives appropriate errors

    psExtra object - a phyloseq object with extra slots:
    
    phyloseq-class experiment-level object
    otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
    sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
    tax_table()   Taxonomy Table:    [ 130 taxa by 4 taxonomic ranks ]
    
    psExtra info:
    tax_agg = "unique" 

# microbiome::aggregate_taxa output hasn't changed: Phylum

    Code
      microbiome::aggregate_taxa(x = dietswap, level = level)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 8 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 8 taxa by 2 taxonomic ranks ]

# microViz::tax_agg output hasn't changed: Phylum

    Code
      ps_get(tax_agg(ps = dietswap, level, sort_by = "name", add_unique = TRUE))
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 8 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 8 taxa by 2 taxonomic ranks ]

# microbiome::aggregate_taxa output hasn't changed: Family

    Code
      microbiome::aggregate_taxa(x = dietswap, level = level)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 22 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 22 taxa by 3 taxonomic ranks ]

# microViz::tax_agg output hasn't changed: Family

    Code
      ps_get(tax_agg(ps = dietswap, level, sort_by = "name", add_unique = TRUE))
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 22 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 22 taxa by 3 taxonomic ranks ]

# tax_fix error prompt looks right

    Code
      cat(taxFixPrompt())
    Output
      
      
      To fix the problem, try:
        `yourData %>% tax_fix()`
      
      Try tax_fix_interactive() to find and fix further problems

---

    Code
      cat(taxFixPrompt(unknowns = c("anUnknown", "another")))
    Output
      
      
      To fix the problem, try:
        `yourData %>% tax_fix(unknowns = c("anUnknown", "another"))`
      
      Try tax_fix_interactive() to find and fix further problems

# tax_agg errors on NAs, '', or convergent values

    Code
      tax_agg(dietswap, rank = "Genus")
    Condition
      Error in `tax_agg()`:
      ! NAs in tax_table at rank: Genus
      
      To fix the problem, try:
        `yourData %>% tax_fix()`
      
      Try tax_fix_interactive() to find and fix further problems

---

    Code
      tax_agg(dietswap, rank = "Genus")
    Condition
      Error in `tax_agg()`:
      ! zero-length name(s) in tax_table at rank: Genus
      
      To fix the problem, try:
        `yourData %>% tax_fix()`
      
      Try tax_fix_interactive() to find and fix further problems

---

    Code
      tax_agg(dietswap, rank = "Genus")
    Message
      Problematic Genus values detected in tax_table:
      g__
      
      Convergent rows:
        `Taxon name`                      Phylum          Family                 Genus
      1 Aeromonas                         Proteobacteria  Proteobacteria         g__  
      2 Akkermansia                       Verrucomicrobia Verrucomicrobia        g__  
      3 Allistipes et rel.                Bacteroidetes   Bacteroidetes          g__  
      4 Anaerofustis                      Firmicutes      Clostridium cluster XV g__  
      5 Anaerostipes caccae et rel.       Firmicutes      Clostridium cluster X~ g__  
      6 Anaerotruncus colihominis et rel. Firmicutes      Clostridium cluster IV g__  
    Condition
      Error:
      ! Taxa cannot be aggregated at rank: Genus
      See last message for convergent taxa rows.
      
      To fix the problem, try:
        `yourData %>% tax_fix(unknowns = c("g__"))`
      
      Try tax_fix_interactive() to find and fix further problems

