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
      ps_get(tax_agg(ps = dietswap, level, sort_by = "name"))
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
      ps_get(tax_agg(ps = dietswap, level, sort_by = "name"))
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 22 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 22 taxa by 3 taxonomic ranks ]

