# tax_models2stats works

    Code
      lmBirthweight_psx
    Output
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 61 taxa and 49 samples ]
      sample_data() Sample Data:       [ 49 samples by 11 sample variables ]
      tax_table()   Taxonomy Table:    [ 61 taxa by 5 taxonomic ranks ]
      
      otu_get(counts = TRUE)		 [ 61 taxa and 49 samples ]
      
      psExtra info:
      tax_agg = "genus" tax_trans = "compositional" 
      
      tax_models list at rank: class 
      
      tax_stats dataframe:
      12 taxa at rank of class 
      1 terms: birth_weight

---

    Code
      wilcox2_psx
    Output
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 61 taxa and 49 samples ]
      sample_data() Sample Data:       [ 49 samples by 11 sample variables ]
      tax_table()   Taxonomy Table:    [ 61 taxa by 5 taxonomic ranks ]
      
      otu_get(counts = TRUE)		 [ 61 taxa and 49 samples ]
      
      psExtra info:
      tax_agg = "genus" tax_trans = "compositional" 
      
      tax_models list at rank: phylum 
      
      tax_stats dataframe:
      5 taxa at rank of phylum 
      2 terms: birth_mode, sex

