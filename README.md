
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microViz

<!-- badges: start -->

<!-- badges: end -->

The goal of microViz is to make wrangling, stats and visualisation of
microbiome (16S) sequencing readcount data easier, modular and
reproducible. microViz wraps and extends core microbial ecology packages
like phyloseq, vegan, and microbiome.

## Installation

You can install the latest available package version using the following
instructions.

``` r
# # If you are on windows you will need to install RTools so that your computer can build this package
# # Follow instructions here: http://jtleek.com/modules/01_DataScientistToolbox/02_10_rtools/

# # How to authenticate your R to install from this private github repo # #
# install.packages("usethis")
# usethis::browse_github_pat() 
# # click "generate token" on the webpage and then follow instructions in the R console.
# # For more help see: https://happygitwithr.com/github-pat.html#step-by-step

# # Installing the latest version of this package # #
# install.packages("devtools")
devtools::install_github("MUMC-MEDMIC/microViz")
# # advanced tip: add @<commit-hash> after microViz to install a particular version
```

## Setup

``` r
library(microViz)
library(phyloseq)
library(vegan)
library(microbiome)
library(dplyr)
library(ggplot2)
```

``` r
# get some example data
data("dietswap", package = "microbiome")

# create a couple of numerical variables to use as constraints or conditions
sample_data(dietswap)$weight <-
  recode(sample_data(dietswap)$bmi_group, obese = 3, overweight = 2, lean = 1)
# add a couple of missing values to demo automated dropping of observations with missings
sample_data(dietswap)$female <-
  if_else(sample_data(dietswap)$sex == "female", true = 1, false = 0)
sample_data(dietswap)$female[c(3, 4)] <- NA
```

## Example ordination plot workflow

You want to visually see if overall microbial ecosystem composition
differs markedly between groups, e.g. BMI.

Here is one option to try first:

1.  Filter out rare taxa (e.g. remove Genera not present in at least 10%
    of samples) - use `tax_filter()`
2.  Aggregate the taxa into bacterial families (for example) - use
    `tax_agg()`
3.  Transform the microbial data with the centre-log-ratio
    transformation - use `tax_transform()`
4.  Perform PCA with the clr-transformed features (equivalent to
    aitchison distance PCoA) - use `ordin8()`
5.  Plot the first 2 axes of this PCA ordination, colouring samples by
    group and adding taxon loading arrows to visualise which taxa
    generally differ across your samples - use `plot_ordin8()`
6.  Customise the theme of the ggplot as you like and/or add features
    like ellipses or annotations

<!-- end list -->

``` r
# perform ordination
unconstrained_aitchison_pca <- 
  dietswap %>%
  tax_filter(min_prevalence = 0.1, tax_level = 'Genus') %>% 
  tax_agg("Family") %>%
  tax_transform('clr') %>%
  ordin8(method = 'RDA')
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.

# create plot
pca_plot <- unconstrained_aitchison_pca %>%
  plot_ordin8(plot_taxa = 1:5, colour = 'bmi_group', auto_title = TRUE)

# customise plot
customised_plot <- pca_plot + 
  stat_ellipse(aes(linetype = bmi_group, colour = bmi_group)) + 
  scale_colour_brewer(palette = 'Set1') + 
  xlim(c(-1.5,2.75)) + theme(legend.position = 'bottom')

# show plot
customised_plot
```

<img src="man/figures/README-ordination-plot-1.png" width="100%" />

## PERMANOVA

You visualised your ordinated data in the plot above. Below you can see
how to perform a PERMANOVA to test the significance of BMI. This example
uses the Family-level aitchison distance to correspond with the plot
above.

``` r
# calculate distances
aitchison_dists <- 
  dietswap %>%
  tax_filter(min_prevalence = 0.1, tax_level = 'Genus') %>% 
  tax_agg("Family") %>%
  calc_dist('aitchison')
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.

# the more permutations you request, the longer it takes, 
# but also the more stable and precise your p-values become
aitchison_perm <- permanova(aitchison_dists, n_perms = 99, n_processes = 1,
                            variables = c('bmi_group'))
#> 2020-10-30 10:19:23 - Starting PERMANOVA with 99 perms with 1 processes
#> 2020-10-30 10:19:24 - Finished PERMANOVA
# view the permanova results
aitchison_perm$permanova
#> 
#> Call:
#> vegan::adonis(formula = FORMULA, data = metadata, permutations = n_perms,      parallel = cl) 
#> 
#> Permutation: free
#> Number of permutations: 99
#> 
#> Terms added sequentially (first to last)
#> 
#>            Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)   
#> bmi_group   2    104.07  52.034  4.7734 0.04177   0.01 **
#> Residuals 219   2387.29  10.901         0.95823          
#> Total     221   2491.35                 1.00000          
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# view the info stored about the distance calculation
aitchison_perm$info
#> $tax_level
#> [1] "Family"
#> 
#> $tax_transformation
#> [1] "none specified"
#> 
#> $distName
#> [1] "aitchison"
```

## Constrained ordination

You could visualise the effect of the (numeric/logical) variables in
your permanova directly using the plot\_ordin8 function with
constraints.

``` r
perm2 <- permanova(aitchison_dists, variables = c('weight', 'female'))
#> WARNING: Dropping samples with NAs for "female". At least 2
#> 2020-10-30 10:19:24 - Starting PERMANOVA with 999 perms with 1 processes
#> 2020-10-30 10:19:26 - Finished PERMANOVA
perm2$permanova
#> 
#> Call:
#> vegan::adonis(formula = FORMULA, data = metadata, permutations = n_perms,      parallel = cl) 
#> 
#> Permutation: free
#> Number of permutations: 999
#> 
#> Terms added sequentially (first to last)
#> 
#>            Df SumsOfSqs MeanSqs F.Model      R2 Pr(>F)    
#> weight      1     55.43  55.427  5.1340 0.02259  0.001 ***
#> female      1     55.83  55.831  5.1714 0.02275  0.001 ***
#> Residuals 217   2342.73  10.796         0.95466           
#> Total     219   2453.98                 1.00000           
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

ordin8(perm2, constraints = c('weight', 'female')) %>% 
  plot_ordin8(colour = 'sex', constraint_label_length = 2) +
  stat_ellipse(aes(colour = sex)) + scale_color_brewer(palette = "Set2")
#> 
#> Centering (mean) and scaling (sd) the constraint and conditioning vars:
#>  weight
#>  female
```

<img src="man/figures/README-constrained-ord-1.png" width="100%" />

You can make your model more complicated, including the addition of
interactions. Currently I don’t provide options for random effects. Ask
me if you want this.

``` r
# example with interactions
permanova(aitchison_dists, variables = c('bmi_group', 'sex'),
          interactions = "bmi_group * sex", 
          adonis2 = TRUE, # important when testing interactions
          return = "permanova" # return only the permanova output
)
#> 2020-10-30 10:19:26 - Starting PERMANOVA with 999 perms with 1 processes
#> 2020-10-30 10:19:28 - Finished PERMANOVA
#> Permutation test for adonis under reduced model
#> Marginal effects of terms
#> Permutation: free
#> Number of permutations: 999
#> 
#> vegan::adonis2(formula = FORMULA, data = metadata, permutations = n_perms, by = "margin", parallel = cl)
#>                Df SumOfSqs      R2      F Pr(>F)    
#> bmi_group:sex   1    55.43 0.02225 5.2811  0.001 ***
#> Residual      217  2277.56 0.91419                  
#> Total         221  2491.35 1.00000                  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

# Session info

``` r
options(width = 100)
devtools::session_info()
#> - Session info -----------------------------------------------------------------------------------
#>  setting  value                       
#>  version  R version 4.0.1 (2020-06-06)
#>  os       Windows 10 x64              
#>  system   x86_64, mingw32             
#>  ui       RTerm                       
#>  language (EN)                        
#>  collate  English_United Kingdom.1252 
#>  ctype    English_United Kingdom.1252 
#>  tz       Europe/Berlin               
#>  date     2020-10-30                  
#> 
#> - Packages ---------------------------------------------------------------------------------------
#>  package      * version    date       lib source                               
#>  ade4           1.7-15     2020-02-13 [1] CRAN (R 4.0.3)                       
#>  ape            5.4-1      2020-08-13 [1] CRAN (R 4.0.3)                       
#>  assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                       
#>  backports      1.1.8      2020-06-17 [1] CRAN (R 4.0.1)                       
#>  Biobase        2.48.0     2020-04-27 [1] Bioconductor                         
#>  BiocGenerics   0.34.0     2020-04-27 [1] Bioconductor                         
#>  biomformat     1.16.0     2020-04-27 [1] Bioconductor                         
#>  Biostrings     2.56.0     2020-04-27 [1] Bioconductor                         
#>  callr          3.5.1      2020-10-13 [1] CRAN (R 4.0.3)                       
#>  cli            2.1.0      2020-10-12 [1] CRAN (R 4.0.3)                       
#>  cluster        2.1.0      2019-06-19 [1] CRAN (R 4.0.1)                       
#>  codetools      0.2-16     2018-12-24 [1] CRAN (R 4.0.1)                       
#>  colorspace     1.4-1      2019-03-18 [1] CRAN (R 4.0.0)                       
#>  crayon         1.3.4      2017-09-16 [1] CRAN (R 4.0.0)                       
#>  data.table     1.13.2     2020-10-19 [1] CRAN (R 4.0.3)                       
#>  desc           1.2.0      2018-05-01 [1] CRAN (R 4.0.0)                       
#>  devtools       2.3.2      2020-09-18 [1] CRAN (R 4.0.3)                       
#>  digest         0.6.25     2020-02-23 [1] CRAN (R 4.0.0)                       
#>  dplyr        * 1.0.2      2020-08-18 [1] CRAN (R 4.0.2)                       
#>  ellipsis       0.3.1      2020-05-15 [1] CRAN (R 4.0.0)                       
#>  evaluate       0.14       2019-05-28 [1] CRAN (R 4.0.0)                       
#>  fansi          0.4.1      2020-01-08 [1] CRAN (R 4.0.0)                       
#>  farver         2.0.3      2020-01-16 [1] CRAN (R 4.0.0)                       
#>  foreach        1.5.1      2020-10-15 [1] CRAN (R 4.0.3)                       
#>  fs             1.5.0      2020-07-31 [1] CRAN (R 4.0.2)                       
#>  generics       0.0.2      2018-11-29 [1] CRAN (R 4.0.0)                       
#>  ggplot2      * 3.3.2      2020-06-19 [1] CRAN (R 4.0.2)                       
#>  glue           1.4.2      2020-08-27 [1] CRAN (R 4.0.2)                       
#>  gtable         0.3.0      2019-03-25 [1] CRAN (R 4.0.0)                       
#>  htmltools      0.5.0      2020-06-16 [1] CRAN (R 4.0.2)                       
#>  igraph         1.2.6      2020-10-06 [1] CRAN (R 4.0.3)                       
#>  IRanges        2.22.2     2020-05-21 [1] Bioconductor                         
#>  iterators      1.0.13     2020-10-15 [1] CRAN (R 4.0.3)                       
#>  jsonlite       1.7.1      2020-09-07 [1] CRAN (R 4.0.3)                       
#>  knitr          1.30       2020-09-22 [1] CRAN (R 4.0.2)                       
#>  labeling       0.4.2      2020-10-20 [1] CRAN (R 4.0.3)                       
#>  lattice      * 0.20-41    2020-04-02 [1] CRAN (R 4.0.1)                       
#>  lifecycle      0.2.0      2020-03-06 [1] CRAN (R 4.0.0)                       
#>  magrittr       1.5        2014-11-22 [1] CRAN (R 4.0.0)                       
#>  MASS           7.3-53     2020-09-09 [1] CRAN (R 4.0.3)                       
#>  Matrix         1.2-18     2019-11-27 [1] CRAN (R 4.0.1)                       
#>  memoise        1.1.0      2017-04-21 [1] CRAN (R 4.0.0)                       
#>  mgcv           1.8-33     2020-08-27 [1] CRAN (R 4.0.3)                       
#>  microbiome   * 1.10.0     2020-04-27 [1] Bioconductor                         
#>  microViz     * 0.0.0.9000 2020-10-30 [1] Github (MUMC-MEDMIC/microViz@988fdbc)
#>  multtest       2.44.0     2020-04-27 [1] Bioconductor                         
#>  munsell        0.5.0      2018-06-12 [1] CRAN (R 4.0.0)                       
#>  nlme           3.1-150    2020-10-24 [1] CRAN (R 4.0.3)                       
#>  permute      * 0.9-5      2019-03-12 [1] CRAN (R 4.0.0)                       
#>  phyloseq     * 1.32.0     2020-04-27 [1] Bioconductor                         
#>  pillar         1.4.6      2020-07-10 [1] CRAN (R 4.0.2)                       
#>  pkgbuild       1.1.0      2020-07-13 [1] CRAN (R 4.0.2)                       
#>  pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.0.0)                       
#>  pkgload        1.1.0      2020-05-29 [1] CRAN (R 4.0.0)                       
#>  plyr           1.8.6      2020-03-03 [1] CRAN (R 4.0.0)                       
#>  prettyunits    1.1.1      2020-01-24 [1] CRAN (R 4.0.0)                       
#>  processx       3.4.4      2020-09-03 [1] CRAN (R 4.0.3)                       
#>  ps             1.3.3      2020-05-08 [1] CRAN (R 4.0.0)                       
#>  purrr          0.3.4      2020-04-17 [1] CRAN (R 4.0.0)                       
#>  R6             2.5.0      2020-10-28 [1] CRAN (R 4.0.1)                       
#>  RColorBrewer   1.1-2      2014-12-07 [1] CRAN (R 4.0.0)                       
#>  Rcpp           1.0.5      2020-07-06 [1] CRAN (R 4.0.2)                       
#>  remotes        2.2.0      2020-07-21 [1] CRAN (R 4.0.3)                       
#>  reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.0.0)                       
#>  rhdf5          2.32.4     2020-10-05 [1] Bioconductor                         
#>  Rhdf5lib       1.10.1     2020-07-09 [1] Bioconductor                         
#>  rlang          0.4.7      2020-07-09 [1] CRAN (R 4.0.2)                       
#>  rmarkdown      2.5        2020-10-21 [1] CRAN (R 4.0.3)                       
#>  rprojroot      1.3-2      2018-01-03 [1] CRAN (R 4.0.0)                       
#>  Rtsne          0.15       2018-11-10 [1] CRAN (R 4.0.2)                       
#>  S4Vectors      0.26.1     2020-05-16 [1] Bioconductor                         
#>  scales         1.1.1      2020-05-11 [1] CRAN (R 4.0.0)                       
#>  sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 4.0.0)                       
#>  stringi        1.5.3      2020-09-09 [1] CRAN (R 4.0.3)                       
#>  stringr        1.4.0      2019-02-10 [1] CRAN (R 4.0.0)                       
#>  survival       3.2-7      2020-09-28 [1] CRAN (R 4.0.3)                       
#>  testthat       2.3.2      2020-03-02 [1] CRAN (R 4.0.0)                       
#>  tibble         3.0.4      2020-10-12 [1] CRAN (R 4.0.3)                       
#>  tidyr          1.1.2      2020-08-27 [1] CRAN (R 4.0.3)                       
#>  tidyselect     1.1.0      2020-05-11 [1] CRAN (R 4.0.0)                       
#>  usethis        1.6.3      2020-09-17 [1] CRAN (R 4.0.3)                       
#>  vctrs          0.3.4      2020-08-29 [1] CRAN (R 4.0.3)                       
#>  vegan        * 2.5-6      2019-09-01 [1] CRAN (R 4.0.0)                       
#>  withr          2.3.0      2020-09-22 [1] CRAN (R 4.0.2)                       
#>  xfun           0.18       2020-09-29 [1] CRAN (R 4.0.3)                       
#>  XVector        0.28.0     2020-04-27 [1] Bioconductor                         
#>  yaml           2.2.1      2020-02-01 [1] CRAN (R 4.0.0)                       
#>  zlibbioc       1.34.0     2020-04-27 [1] Bioconductor                         
#> 
#> [1] C:/Program Files/R/R-4.0.1/library
```