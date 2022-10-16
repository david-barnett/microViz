# ord_explore app works: unconstrained ords

    Code
      ps
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 88 taxa and 8 samples ]
      sample_data() Sample Data:       [ 8 samples by 11 sample variables ]
      tax_table()   Taxonomy Table:    [ 88 taxa by 6 taxonomic ranks ]
      phy_tree()    Phylogenetic Tree: [ 88 tips and 87 internal nodes ]

---

    Code
      phyloseq::sample_variables(ps)
    Output
       [1] "subject_id"              "family_id"              
       [3] "sex"                     "family_role"            
       [5] "age"                     "infant_age"             
       [7] "birth_weight"            "birth_mode"             
       [9] "c_section_type"          "antibiotics_current_use"
      [11] "number_reads"           

---

    Code
      phyloseq::sample_names(ps)
    Output
      [1] "B01042_ba_10"    "SID513122_ba_10" "SID515126_ba_10" "SID521130_ba_10"
      [5] "SID604136_ba_10" "SID606141_ba_10" "BBS0007_ba_10"   "BBS0008_ba_10"  

