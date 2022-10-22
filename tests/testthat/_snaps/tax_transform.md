# tax_transform 'maaslin2-default' chaining works

    Code
      ord
    Output
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
      
      otu_get(counts = TRUE)		 [ 130 taxa and 222 samples ]
      
      psExtra info:
      tax_agg = 'Genus'	tax_trans = 'compositional&log2'	
      
      ordination of class: rda cca 
      rda(formula = OTU ~ 1, data = data)
      Ordination info:
      method = 'PCA'	

