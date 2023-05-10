# ord_explore_init stays the same

    Code
      ord_explore_init(dietswap)
    Output
      $data
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 9 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 4 taxonomic ranks ]
      
      psExtra info:
      tax_agg = "unique" tax_trans = "identity" 
      
      $info
      $info$rank
      [1] "unique"
      
      $info$trans
      [1] "identity"
      
      $info$scale
      character(0)
      
      $info$dist
      [1] "none"
      
      $info$ord
      [1] "auto"
      
      $info$constraints
      NULL
      
      $info$conditions
      NULL
      
      $info$isCon
      [1] FALSE
      
      
      $vars
      $vars$all
      [1] "subject"                "sex"                    "nationality"           
      [4] "group"                  "sample"                 "timepoint"             
      [7] "timepoint.within.group" "bmi_group"              "SAMPLE"                
      
      $vars$num
      [1] "timepoint"              "timepoint.within.group"
      
      $vars$cat
      [1] "subject"     "sex"         "nationality" "group"       "sample"     
      [6] "bmi_group"   "SAMPLE"     
      
      $vars$shapeSafe
      [1] "sex"                    "nationality"            "group"                 
      [4] "timepoint.within.group" "bmi_group"             
      
      
      $ranks
      [1] "Phylum" "Family" "Genus"  "unique"
      
      $warn
      [1] FALSE
      

---

    Code
      ord_explore_init(ord)
    Output
      $data
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 11 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 4 taxonomic ranks ]
      
      otu_get(counts = TRUE)		 [ 130 taxa and 222 samples ]
      
      psExtra info:
      tax_agg = "Genus" tax_trans = "clr" 
      
      ordination of class: rda cca 
      rda(formula = OTU ~ weight + female, data = data)
      Ordination info:
      method = 'RDA'	constraints = 'weight+female'	
      
      $info
      $info$rank
      [1] "Genus"
      
      $info$trans
      [1] "clr"
      
      $info$scale
      character(0)
      
      $info$dist
      [1] "none"
      
      $info$ord
      [1] "RDA"
      
      $info$constraints
      [1] "weight" "female"
      
      $info$conditions
      NULL
      
      $info$isCon
      [1] TRUE
      
      
      $vars
      $vars$all
       [1] "subject"                "sex"                    "nationality"           
       [4] "group"                  "sample"                 "timepoint"             
       [7] "timepoint.within.group" "bmi_group"              "weight"                
      [10] "female"                 "SAMPLE"                
      
      $vars$num
      [1] "timepoint"              "timepoint.within.group" "weight"                
      [4] "female"                
      
      $vars$cat
      [1] "subject"     "sex"         "nationality" "group"       "sample"     
      [6] "bmi_group"   "SAMPLE"     
      
      $vars$shapeSafe
      [1] "sex"                    "nationality"            "group"                 
      [4] "timepoint.within.group" "bmi_group"              "weight"                
      [7] "female"                
      
      
      $ranks
      [1] "Phylum" "Family" "Genus"  "unique"
      
      $warn
      [1] FALSE
      

---

    Code
      ord_explore_init(esophagus)
    Message
      Note: Replacing missing sample_data with a dataframe of only sample_names.
      Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
      Note: Replacing missing tax_table with a 1-column table of only taxa_names.
      Try `ps <- phyloseq_validate(ps, verbose = FALSE)` to avoid this message
    Output
      $data
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 58 taxa and 3 samples ]
      sample_data() Sample Data:       [ 3 samples by 1 sample variables ]
      tax_table()   Taxonomy Table:    [ 58 taxa by 1 taxonomic ranks ]
      phy_tree()    Phylogenetic Tree: [ 58 tips and 57 internal nodes ]
      
      psExtra info:
      tax_agg = "unique" tax_trans = "identity" 
      
      $info
      $info$rank
      [1] "unique"
      
      $info$trans
      [1] "identity"
      
      $info$scale
      character(0)
      
      $info$dist
      [1] "none"
      
      $info$ord
      [1] "auto"
      
      $info$constraints
      NULL
      
      $info$conditions
      NULL
      
      $info$isCon
      [1] FALSE
      
      
      $vars
      $vars$all
      [1] "SAMPLE"
      
      $vars$num
      character(0)
      
      $vars$cat
      NULL
      
      $vars$shapeSafe
      NULL
      
      
      $ranks
      [1] "unique"
      
      $warn
      [1] FALSE
      

# dist_choices helper works

    Code
      dist_choices(dietswap, type = "tree")
    Output
      named character(0)

---

    Code
      dist_choices(esophagus, type = "tree")
    Output
             gunifrac: Generalised UniFrac, alpha=0.5 
                                           "gunifrac" 
                           wunifrac: weighted UniFrac 
                                           "wunifrac" 
                          unifrac: unweighted UniFrac 
                                            "unifrac" 
      va-wunifrac: variance-adjusted weighted UniFrac 
                                        "va-wunifrac" 
                                                dpcoa 
                                              "dpcoa" 

# ord_choices helper works

    Code
      cat(type)
    Output
      all
    Code
      cat(ord_choices(type))
    Output
      auto PCA PCoA RDA CAP CCA NMDS

---

    Code
      cat(type)
    Output
      constrained
    Code
      cat(ord_choices(type))
    Output
      auto RDA CAP CCA

---

    Code
      cat(type)
    Output
      unconstrained
    Code
      cat(ord_choices(type))
    Output
      auto PCA PCoA NMDS

---

    Code
      cat(type)
    Output
      dist
    Code
      cat(ord_choices(type))
    Output
      auto PCoA CAP NMDS

---

    Code
      cat(type)
    Output
      noDist
    Code
      cat(ord_choices(type))
    Output
      auto PCA RDA CCA

---

    Code
      cat(type)
    Output
      constrained dist
    Code
      cat(ord_choices(type))
    Output
      auto CAP

---

    Code
      cat(type)
    Output
      unconstrained dist
    Code
      cat(ord_choices(type))
    Output
      auto PCoA NMDS

---

    Code
      cat(type)
    Output
      constrained noDist
    Code
      cat(ord_choices(type))
    Output
      auto RDA CCA

---

    Code
      cat(type)
    Output
      unconstrained noDist
    Code
      cat(ord_choices(type))
    Output
      auto PCA

# ord_code helper works

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      FALSE 	0.5 	 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        colour = "v", fill = "v",
        shape = "var", alpha = 0.5,
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      FALSE 	0.5 	test1 test2 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        constraints = c("test1", "test2"),
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        colour = "v", fill = "v",
        shape = "var", alpha = 0.5,
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      FALSE 	aVariable 	 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        colour = "v", fill = "v",
        shape = "var", alpha = "aVariable",
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      FALSE 	aVariable 	test1 test2 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        constraints = c("test1", "test2"),
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        colour = "v", fill = "v",
        shape = "var", alpha = "aVariable",
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      1 2 3 4 5 6 	0.5 	 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        plot_taxa = 1:6,
        colour = "v", fill = "v",
        shape = "var", alpha = 0.5,
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      1 2 3 4 5 6 	0.5 	test1 test2 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        constraints = c("test1", "test2"),
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        plot_taxa = 1:6,
        colour = "v", fill = "v",
        shape = "var", alpha = 0.5,
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      1 2 3 4 5 6 	aVariable 	 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        plot_taxa = 1:6,
        colour = "v", fill = "v",
        shape = "var", alpha = "aVariable",
        size = 1
       )

---

    Code
      for (x in list(p, a, c)) cat(x, "\t")
    Output
      1 2 3 4 5 6 	aVariable 	test1 test2 	
    Code
      ord_code(rank = "Genus", trans = "identity", dist = "none", ord = "RDA", const = c,
        conds = NULL, x = 1, y = 2, colour = "v", fill = "v", shape = "var", alpha = a,
        size = 1, plot_taxa = p, ellipses = FALSE, chulls = FALSE, paths = NULL)
    Output
      your_phyloseq %>%
       tax_transform(rank = "Genus", trans = "identity") %>%
       ord_calc(
        constraints = c("test1", "test2"),
        method = "RDA"
       ) %>% 
       ord_plot(
        axes = c(1, 2),
        plot_taxa = 1:6,
        colour = "v", fill = "v",
        shape = "var", alpha = "aVariable",
        size = 1
       )

# ord_code_dist helper works

    Code
      cat(ord_code_dist("aitchison"))
    Output
       dist_calc(dist = "aitchison") %>%

---

    Code
      cat(ord_code_dist("none"))

# Testing ord_code_stat() different combinations of ellipses and chulls

    Code
      cat(ord_code_stat(ellipses = TRUE, chulls = FALSE, colour = "aVar"))
    Output
       ) +
       ggplot2::stat_ellipse(
        ggplot2::aes(colour = aVar)
       )

---

    Code
      cat(ord_code_stat(ellipses = FALSE, chulls = FALSE, colour = "aVar"))
    Output
       )

---

    Code
      cat(ord_code_stat(ellipses = FALSE, chulls = TRUE, colour = "aVar"))
    Output
       ) +
       stat_chull(
        ggplot2::aes(colour = aVar)
       )

---

    Code
      cat(ord_code_stat(ellipses = TRUE, chulls = TRUE, colour = "aVar"))
    Output
       ) +
       stat_chull(
        ggplot2::aes(colour = aVar)
       )

# Testing ord_code_paths() with different all_vars options (string & vec)

    Code
      cat(ord_code_paths(paths = list(colour = "aVar", id_var = "bVar", id_values = letters[
        1:4], all_vars = "aVar")))
    Output
       ) %>%
       add_paths(
        id_var = "bVar", 
        id_values = c("a", "b", "c", "d"),
        mapping = ggplot2::aes(colour = aVar)
       )

---

    Code
      cat(ord_code_paths(paths = list(colour = "aVar", id_var = "bVar", id_values = letters[
        1:4], all_vars = c("otherVar", "anotherVar"))))
    Output
       ) %>%
       add_paths(
        id_var = "bVar", 
        id_values = c("a", "b", "c", "d"),
        colour = "aVar"
       )

# ord_build works

    Code
      ord_build(data = dietswap, rank = "Genus", trans = "identity", dist = "bray",
        method = "PCoA", constraints = NULL, conditions = NULL)
    Output
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
      
      psExtra info:
      tax_agg = "Genus" tax_trans = "identity" 
      
      bray distance matrix of size 222 
      0.7639533 0.7851213 0.6680796 0.7699252 0.80507 ...
      
      ordination of class: capscale rda cca 
      capscale(formula = distance ~ 1, data = data)
      Ordination info:
      method = 'PCoA'	

---

    Code
      ord_build(data = dietswap, rank = "Genus", trans = "clr", dist = NA, method = "auto",
        constraints = NULL, conditions = NULL)
    Output
      psExtra object - a phyloseq object with extra slots:
      
      phyloseq-class experiment-level object
      otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
      sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
      tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
      
      otu_get(counts = TRUE)		 [ 130 taxa and 222 samples ]
      
      psExtra info:
      tax_agg = "Genus" tax_trans = "clr" 
      
      ordination of class: rda cca 
      rda(formula = OTU ~ 1, data = data)
      Ordination info:
      method = 'PCA'	

# ord_explore_palet_fun works

    Code
      ord_explore_palet_fun(dietswap, "Genus")
    Output
         Prevotella melaninogenica et rel.   Oscillospira guillermondii et rel. 
                                 "#A6CEE3"                            "#1F78B4" 
              Bacteroides vulgatus et rel.        Clostridium cellulosi et rel. 
                                 "#B2DF8A"                            "#33A02C" 
                 Prevotella oralis et rel. Faecalibacterium prausnitzii et rel. 
                                 "#FB9A99"                            "#E31A1C" 
            Sporobacter termitidis et rel.        Clostridium symbiosum et rel. 
                                 "#FDBF6F"                            "#FF7F00" 
                        Allistipes et rel.     Clostridium orbiscindens et rel. 
                                 "#CAB2D6"                            "#6A3D9A" 
          Subdoligranulum variable at rel.           Ruminococcus obeum et rel. 
                                 "#FFFF99"                            "#B15928" 
            Butyrivibrio crossotus et rel.         Bacteroides fragilis et rel. 
                                 "#1ff8ff"                            "#1B9E77" 
                               Akkermansia           Bacteroides ovatus et rel. 
                                 "#D95F02"                            "#7570B3" 
        Parabacteroides distasonis et rel.        Dorea formicigenerans et rel. 
                                 "#E7298A"                            "#66A61E" 
             Bacteroides uniformis et rel.                            Dialister 
                                 "#E6AB02"                            "#A6761D" 
          Bryantella formatexigens et rel.           Uncultured Clostridiales I 
                                 "#666666"                            "#4b6a53" 
              Coprococcus eutactus et rel.           Clostridium leptum et rel. 
                                 "#b249d5"                            "#7edc45" 
            Clostridium sphenoides et rel.             Escherichia coli et rel. 
                                 "#5c47b8"                            "#cfd251" 
               Streptococcus bovis et rel.          Uncultured Clostridiales II 
                                 "#ff69b4"                            "#69c86c" 
                           Bifidobacterium    Anaerotruncus colihominis et rel. 
                                 "#cd3e50"                            "#83d5af" 
         Lachnospira pectinoschiza et rel.          Anaerostipes caccae et rel. 
                                 "#da6130"                            "#5e79b2" 
             Ruminococcus callidus et rel.      Bacteroides splachnicus et rel. 
                                 "#c29545"                            "#532a5a" 
               Ruminococcus bromii et rel.          Prevotella tannerae et rel. 
                                 "#5f7b35"                            "#c497cf" 
              Lachnobacillus bovis et rel.          Eubacterium rectale et rel. 
                                 "#773a27"                            "#7cb9cb" 
            Mitsuokella multiacida et rel. Outgrouping clostridium cluster XIVa 
                                 "#594e50"                            "#d3c4a8" 
                Clostridium nexile et rel.                                Other 
                                 "#c17e7f"                             "grey90" 

---

    Code
      ord_explore_palet_fun(ps = dietswap, tax_level = "Family", top_by = median,
        other = "colourz")
    Output
                  Bacteroidetes    Clostridium cluster IV  Clostridium cluster XIVa 
                      "#A6CEE3"                 "#1F78B4"                 "#B2DF8A" 
                 Proteobacteria    Clostridium cluster IX                   Bacilli 
                      "#33A02C"                 "#FB9A99"                 "#E31A1C" 
       Uncultured Clostridiales            Actinobacteria           Verrucomicrobia 
                      "#FDBF6F"                 "#FF7F00"                 "#CAB2D6" 
         Clostridium cluster XI     Clostridium cluster I   Clostridium cluster XVI 
                      "#6A3D9A"                 "#FFFF99"                 "#B15928" 
          Uncultured Mollicutes Clostridium cluster XVIII              Fusobacteria 
                      "#1ff8ff"                 "#1B9E77"                 "#D95F02" 
        Clostridium cluster III  Clostridium cluster XIII    Clostridium cluster XV 
                      "#7570B3"                 "#E7298A"                 "#66A61E" 
       Clostridium cluster XVII            Asteroleplasma              Spirochaetes 
                      "#E6AB02"                 "#A6761D"                 "#666666" 
                  Cyanobacteria                     Other 
                      "#4b6a53"                 "colourz" 

