# microbiome's dietswap data hasn't changed

    Code
      dietswap
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

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

# tax_agg errors on NAs or convergent values

    Code
      tax_agg(dietswap, rank = "Genus")
    Error <simpleError>
      NAs in tax_table at rank: Genus
      To fix the problem, try:
      `yourData %>% tax_fix()`
      Try tax_fix_interactive() to find and fix further problems

---

    Code
      tax_agg(dietswap, rank = "Genus")
    Message <simpleMessage>
      Problematic Genus values detected in tax_table:
      g__
      -
      taxa_name / Phylum / Family / Genus
      Aeromonas / Proteobacteria / Proteobacteria / g__
      Akkermansia / Verrucomicrobia / Verrucomicrobia / g__
      Allistipes et rel. / Bacteroidetes / Bacteroidetes / g__
      Anaerofustis / Firmicutes / Clostridium cluster XV / g__
      Anaerostipes caccae et rel. / Firmicutes / Clostridium cluster XIVa / g__
      Anaerotruncus colihominis et rel. / Firmicutes / Clostridium cluster IV / g__
      -
    Error <simpleError>
      Taxa not unique at rank: Genus
      See last messages for convergent taxa rows.
      To fix the problem, try:
      `yourData %>% tax_fix(unknowns = c("g__"))`
      Try tax_fix_interactive() to find and fix further problems

