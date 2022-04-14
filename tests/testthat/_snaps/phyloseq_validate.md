# phyloseq_validate warns about removing all zero taxa

    Code
      phyloseq_validate(ps = dietswap, remove_undetected = TRUE, verbose = TRUE)
    Condition
      [1m[33mWarning[39m in [38;5;252m`phyloseq_validate()`[39m:[22m
      Some taxa_sums were zero, removing the following taxa:
      	Aerococcus 
      	Aneurinibacillus 
      	Asteroleplasma et rel. 
      	Clostridium felsineum et rel. 
      	Clostridium thermocellum et rel. 
      	Methylobacterium 
      	Micrococcaceae
      This may be caused by using `subset_samples()`.
      Try using `ps_filter()` instead, with .keep_all_taxa = FALSE.
      Otherwise, to avoid this warning, try filtering out taxa summing to zero with `tax_filter()`.
      If you have already transformed and/or scaled your taxa, e.g. with a log transformation or scale,
      seeing this warning is possible, but very unlikely and possibly a bug. Please report this.
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 123 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 123 taxa by 3 taxonomic ranks ]

# phyloseq_validate fixes missing sam_data with message

    Code
      phyloseq_validate(ps = dietswap, verbose = TRUE)
    Message
      Note: Replacing missing sample_data with a dataframe of only sample_names.
      Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 1 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]

# phyloseq_validate fixes missing tax_table with message

    Code
      phyloseq_validate(soilrep, verbose = TRUE)
    Message
      Note: Replacing missing tax_table with a 1-column table of only taxa_names.
      Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 16825 taxa and 56 samples ]
      sample_data() Sample Data:       [ 56 samples by 4 sample variables ]
      tax_table()   Taxonomy Table:    [ 16825 taxa by 1 taxonomic ranks ]

