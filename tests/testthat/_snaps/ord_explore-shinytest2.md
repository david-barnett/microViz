# ord_explore app works: unconstrained ords

    Code
      ps
    Output
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 70 taxa and 5 samples ]
      sample_data() Sample Data:       [ 5 samples by 11 sample variables ]
      tax_table()   Taxonomy Table:    [ 70 taxa by 6 taxonomic ranks ]
      phy_tree()    Phylogenetic Tree: [ 70 tips and 69 internal nodes ]

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
      [1] "SID513122_ba_10" "SID515126_ba_10" "SID521130_ba_10" "SID604136_ba_10"
      [5] "SID606141_ba_10"

