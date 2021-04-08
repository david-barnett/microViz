# full join stays the same

    Code
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
      sample_data() Sample Data:       [ 280 samples by 12 sample variables ]
      tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]

# inner join stays the same

    Code
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 533 taxa and 100 samples ]
      sample_data() Sample Data:       [ 100 samples by 12 sample variables ]
      tax_table()   Taxonomy Table:    [ 533 taxa by 1 taxonomic ranks ]

# left join stays the same

    Code
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 553 taxa and 280 samples ]
      sample_data() Sample Data:       [ 280 samples by 12 sample variables ]
      tax_table()   Taxonomy Table:    [ 553 taxa by 1 taxonomic ranks ]

# anti join stays the same

    Code
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 549 taxa and 180 samples ]
      sample_data() Sample Data:       [ 180 samples by 10 sample variables ]
      tax_table()   Taxonomy Table:    [ 549 taxa by 1 taxonomic ranks ]

# semi join stays the same

    Code
      ps_join(x = x, y = y, match_sample_names = "ID_var", type = j)
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 533 taxa and 100 samples ]
      sample_data() Sample Data:       [ 100 samples by 10 sample variables ]
      tax_table()   Taxonomy Table:    [ 533 taxa by 1 taxonomic ranks ]

