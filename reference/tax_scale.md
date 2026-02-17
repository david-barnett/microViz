# Mean-center and SD-scale taxa in phyloseq

Wrapper for applying base scale function to phyloseq otu_table

## Usage

``` r
tax_scale(data, center = TRUE, scale = TRUE, do = NA, keep_counts = TRUE)
```

## Arguments

- data:

  phyloseq or psExtra or otu_table

- center:

  if TRUE: center each taxon by subtracting its mean

- scale:

  if TRUE, divide each centred taxon by its standard deviation (or
  divide by RMS if not centred!)

- do:

  alternative argument that overrides center and scale options! takes
  "both", "scale", "center" or "neither"

- keep_counts:

  if TRUE, retain the original count data in psExtra counts slot

## Examples

``` r
data("dietswap", package = "microbiome")
ps <- dietswap
ps %>%
  otu_get() %>%
  .[1:6, 1:6]
#> OTU Table:          [6 taxa and 6 samples]
#>                      taxa are columns
#>          Actinomycetaceae Aerococcus Aeromonas Akkermansia
#> Sample-1                0          0         0          18
#> Sample-2                1          0         0          97
#> Sample-3                0          0         0          67
#> Sample-4                1          0         0         256
#> Sample-5                0          0         0          21
#> Sample-6                0          0         0          16
#>          Alcaligenes faecalis et rel. Allistipes et rel.
#> Sample-1                            1                336
#> Sample-2                            2                 63
#> Sample-3                            3                 36
#> Sample-4                            2                 96
#> Sample-5                            2                 49
#> Sample-6                            2                 17

# standard use (mean center and SD scale)
tax_scale(ps) %>%
  otu_get() %>%
  .[1:6, 1:6] # Aerococcus is NaN as standard deviation = 0 (0 prevalence)
#> OTU Table:          [6 taxa and 6 samples]
#>                      taxa are columns
#>          Actinomycetaceae Aerococcus Aeromonas Akkermansia
#> Sample-1                0          0         0          18
#> Sample-2                1          0         0          97
#> Sample-3                0          0         0          67
#> Sample-4                1          0         0         256
#> Sample-5                0          0         0          21
#> Sample-6                0          0         0          16
#>          Alcaligenes faecalis et rel. Allistipes et rel.
#> Sample-1                            1                336
#> Sample-2                            2                 63
#> Sample-3                            3                 36
#> Sample-4                            2                 96
#> Sample-5                            2                 49
#> Sample-6                            2                 17

# RMS scale only (directly on otu_table)
otu_get(ps) %>%
  tax_scale(center = FALSE) %>%
  .[1:6, 1:6] # Aerococcus is NaN as standard deviation = 0 (0 prevalence)
#> OTU Table:          [6 taxa and 6 samples]
#>                      taxa are columns
#>          Actinomycetaceae Aerococcus Aeromonas Akkermansia
#> Sample-1         0.000000        NaN         0  0.05741261
#> Sample-2         3.716517        NaN         0  0.30939020
#> Sample-3         0.000000        NaN         0  0.21370251
#> Sample-4         3.716517        NaN         0  0.81653496
#> Sample-5         0.000000        NaN         0  0.06698138
#> Sample-6         0.000000        NaN         0  0.05103343
#>          Alcaligenes faecalis et rel. Allistipes et rel.
#> Sample-1                   0.05065049         0.83097699
#> Sample-2                   0.10130097         0.15580819
#> Sample-3                   0.15195146         0.08903325
#> Sample-4                   0.10130097         0.23742200
#> Sample-5                   0.10130097         0.12118414
#> Sample-6                   0.10130097         0.04204348

# example using alternative `do` argument (to center only, no scaling)
tax_scale(ps, do = "center") %>%
  otu_get() %>%
  .[1:6, 1:6]
#> OTU Table:          [6 taxa and 6 samples]
#>                      taxa are columns
#>          Actinomycetaceae Aerococcus Aeromonas Akkermansia
#> Sample-1                0          0         0          18
#> Sample-2                1          0         0          97
#> Sample-3                0          0         0          67
#> Sample-4                1          0         0         256
#> Sample-5                0          0         0          21
#> Sample-6                0          0         0          16
#>          Alcaligenes faecalis et rel. Allistipes et rel.
#> Sample-1                            1                336
#> Sample-2                            2                 63
#> Sample-3                            3                 36
#> Sample-4                            2                 96
#> Sample-5                            2                 49
#> Sample-6                            2                 17

# preserves existing info
tax_transform(ps, "compositional", rank = "Genus") %>% tax_scale()
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 130 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Genus" tax_trans = "compositional" tax_scale = "centered&scaled" 

# drops other psExtra objects previously calculated with unscaled data
psxDist <- tax_agg(ps, "Genus") %>% dist_calc()
psxDist
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> psExtra info:
#> tax_agg = "Genus" 
#> 
#> bray distance matrix of size 222 
#> 0.7639533 0.7851213 0.6680796 0.7699252 0.80507 ...
psxDist %>% tax_scale()
#> psExtra object - a phyloseq object with extra slots:
#> 
#> phyloseq-class experiment-level object
#> otu_table()   OTU Table:         [ 130 taxa and 222 samples ]
#> sample_data() Sample Data:       [ 222 samples by 8 sample variables ]
#> tax_table()   Taxonomy Table:    [ 130 taxa by 3 taxonomic ranks ]
#> 
#> otu_get(counts = TRUE)        [ 130 taxa and 222 samples ]
#> 
#> psExtra info:
#> tax_agg = "Genus" tax_scale = "centered&scaled" 
#> 
#>  distance matrix of size 222 
#> 0.7639533 0.7851213 0.6680796 0.7699252 0.80507 ...
tax_scale(psxDist) %>% info_get()
#> psExtra info:
#> tax_agg = "Genus" tax_scale = "centered&scaled" 
```
