# taxatree_plotkey works as expected

    Code
      attr(unlabeledKey$data, "graph")
    Output
      # A tbl_graph: 62 nodes and 61 edges
      #
      # A rooted tree
      #
      # Edge Data: 61 × 7 (active)
          from    to parent            rank   prevalence label direction
         <int> <int> <chr>             <chr>       <dbl> <lgl> <fct>    
       1     1     2 root              phylum      0.8   TRUE  right    
       2     1     3 root              phylum      0.933 TRUE  right    
       3     1     4 root              phylum      1     TRUE  left     
       4     1     5 root              phylum      0.467 TRUE  left     
       5     2     6 p: Proteobacteria class       0.8   TRUE  left     
       6     3     7 p: Actinobacteria class       0.933 TRUE  right    
       7     4     8 p: Firmicutes     class       1     TRUE  right    
       8     3     9 p: Actinobacteria class       0.4   TRUE  left     
       9     5    10 p: Bacteroidetes  class       0.467 TRUE  left     
      10     4    11 p: Firmicutes     class       0.533 TRUE  left     
      # ℹ 51 more rows
      #
      # Node Data: 62 × 6
        taxon             parent rank   prevalence label .ggraph.orig_index
        <chr>             <chr>  <chr>       <dbl> <lgl>              <int>
      1 root              root   root        1     TRUE                   1
      2 p: Proteobacteria root   phylum      0.8   TRUE                   2
      3 p: Actinobacteria root   phylum      0.933 TRUE                   3
      # ℹ 59 more rows

---

    Code
      attr(unlabeledKey_rect$data, "graph")
    Output
      # A tbl_graph: 62 nodes and 61 edges
      #
      # A rooted tree
      #
      # Edge Data: 61 × 7 (active)
          from    to parent            rank   prevalence label direction
         <int> <int> <chr>             <chr>       <dbl> <lgl> <fct>    
       1     1     2 root              phylum      0.8   TRUE  right    
       2     1     3 root              phylum      0.933 TRUE  right    
       3     1     4 root              phylum      1     TRUE  left     
       4     1     5 root              phylum      0.467 TRUE  left     
       5     2     6 p: Proteobacteria class       0.8   TRUE  left     
       6     3     7 p: Actinobacteria class       0.933 TRUE  right    
       7     4     8 p: Firmicutes     class       1     TRUE  right    
       8     3     9 p: Actinobacteria class       0.4   TRUE  left     
       9     5    10 p: Bacteroidetes  class       0.467 TRUE  left     
      10     4    11 p: Firmicutes     class       0.533 TRUE  left     
      # ℹ 51 more rows
      #
      # Node Data: 62 × 6
        taxon             parent rank   prevalence label .ggraph.orig_index
        <chr>             <chr>  <chr>       <dbl> <lgl>              <int>
      1 root              root   root        1     TRUE                   1
      2 p: Proteobacteria root   phylum      0.8   TRUE                   2
      3 p: Actinobacteria root   phylum      0.933 TRUE                   3
      # ℹ 59 more rows

