
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microViz

<!-- badges: start -->

<!-- badges: end -->

The goal of microViz is to make wrangling, stats and visualisation of
microbiome (16S) sequencing readcount data easier, modular and
reproducible. microViz wraps and extends core microbial ecology packages
like phyloseq, vegan, and microbiome.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MUMC-MEDMIC/microViz@bbecb5d33dc0b6f987d31a788105a3dbc61cfab5")
```

## Setup

``` r
library(microViz)
library(phyloseq)
library(vegan)
library(microbiome)
library(dplyr)
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

## Example: ordination plot workflow

You want to visually see if overall microbial ecosystem composition
differs markedly between groups, e.g. BMI.

Here is one option to try first:

1.  Aggregate the taxa into bacterial families (for example) - use
    `tax_agg()`
2.  Transform the microbial data with the centre-log-ratio
    transformation - use `tax_transform()`
3.  Perform PCA with the clr-transformed features (equivalent to
    aitchison distance PCoA) - use `ordin8()`
4.  Plot the first 2 axes of this PCA ordination, colouring samples by
    group and adding taxon loading arrows to visualise which taxa
    generally differ across your samples - use `plot_ordin8()`
5.  Customise the theme of the ggplot as you like and/or add features
    like ellipses or annotations

<!-- end list -->

``` r
unconstrained_aitchison_pca <- 
  dietswap %>%
  tax_agg("Family") %>%
  tax_transform('clr') %>%
  ordin8(method = 'RDA')

unconstrained_aitchison_pca %>%
  plot_ordin8(plot_taxa = 1:5, colour = 'bmi_group', auto_title = TRUE) +
  stat_ellipse(aes(linetype = bmi_group, colour = bmi_group)) + 
  scale_colour_brewer(palette = 'Set1')
```

<img src="man/figures/README-ordination-plot-1.png" width="100%" />

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
#>  date     2020-10-28                  
#> 
#> - Packages ---------------------------------------------------------------------------------------
#>  package      * version    date       lib source                                 
#>  ade4           1.7-15     2020-02-13 [1] CRAN (R 4.0.0)                         
#>  ape            5.4        2020-06-03 [1] CRAN (R 4.0.0)                         
#>  assertthat     0.2.1      2019-03-21 [1] CRAN (R 4.0.0)                         
#>  backports      1.1.8      2020-06-17 [1] CRAN (R 4.0.1)                         
#>  Biobase        2.48.0     2020-04-27 [1] Bioconductor                           
#>  BiocGenerics   0.34.0     2020-04-27 [1] Bioconductor                           
#>  biomformat     1.16.0     2020-04-27 [1] Bioconductor                           
#>  Biostrings     2.56.0     2020-04-27 [1] Bioconductor                           
#>  callr          3.5.1      2020-10-13 [1] CRAN (R 4.0.3)                         
#>  cli            2.0.2      2020-02-28 [1] CRAN (R 4.0.0)                         
#>  cluster        2.1.0      2019-06-19 [1] CRAN (R 4.0.1)                         
#>  codetools      0.2-16     2018-12-24 [1] CRAN (R 4.0.1)                         
#>  colorspace     1.4-1      2019-03-18 [1] CRAN (R 4.0.0)                         
#>  crayon         1.3.4      2017-09-16 [1] CRAN (R 4.0.0)                         
#>  data.table     1.12.8     2019-12-09 [1] CRAN (R 4.0.0)                         
#>  desc           1.2.0      2018-05-01 [1] CRAN (R 4.0.0)                         
#>  devtools       2.3.2      2020-09-18 [1] CRAN (R 4.0.3)                         
#>  digest         0.6.25     2020-02-23 [1] CRAN (R 4.0.0)                         
#>  dplyr        * 1.0.2      2020-08-18 [1] CRAN (R 4.0.2)                         
#>  ellipsis       0.3.1      2020-05-15 [1] CRAN (R 4.0.0)                         
#>  evaluate       0.14       2019-05-28 [1] CRAN (R 4.0.0)                         
#>  fansi          0.4.1      2020-01-08 [1] CRAN (R 4.0.0)                         
#>  farver         2.0.3      2020-01-16 [1] CRAN (R 4.0.0)                         
#>  foreach        1.5.0      2020-03-30 [1] CRAN (R 4.0.0)                         
#>  fs             1.5.0      2020-07-31 [1] CRAN (R 4.0.2)                         
#>  generics       0.0.2      2018-11-29 [1] CRAN (R 4.0.0)                         
#>  ggplot2      * 3.3.2      2020-06-19 [1] CRAN (R 4.0.2)                         
#>  glue           1.4.2      2020-08-27 [1] CRAN (R 4.0.2)                         
#>  gtable         0.3.0      2019-03-25 [1] CRAN (R 4.0.0)                         
#>  htmltools      0.5.0      2020-06-16 [1] CRAN (R 4.0.2)                         
#>  igraph         1.2.5      2020-03-19 [1] CRAN (R 4.0.0)                         
#>  IRanges        2.22.2     2020-05-21 [1] Bioconductor                           
#>  iterators      1.0.12     2019-07-26 [1] CRAN (R 4.0.0)                         
#>  jsonlite       1.7.1      2020-09-07 [1] CRAN (R 4.0.3)                         
#>  knitr          1.30       2020-09-22 [1] CRAN (R 4.0.2)                         
#>  labeling       0.3        2014-08-23 [1] CRAN (R 4.0.0)                         
#>  lattice      * 0.20-41    2020-04-02 [1] CRAN (R 4.0.1)                         
#>  lifecycle      0.2.0      2020-03-06 [1] CRAN (R 4.0.0)                         
#>  magrittr       1.5        2014-11-22 [1] CRAN (R 4.0.0)                         
#>  MASS           7.3-51.6   2020-04-26 [1] CRAN (R 4.0.1)                         
#>  Matrix         1.2-18     2019-11-27 [1] CRAN (R 4.0.1)                         
#>  memoise        1.1.0      2017-04-21 [1] CRAN (R 4.0.0)                         
#>  mgcv           1.8-31     2019-11-09 [1] CRAN (R 4.0.1)                         
#>  microbiome   * 1.10.0     2020-04-27 [1] Bioconductor                           
#>  microViz     * 0.0.0.9000 2020-10-27 [1] Github (david-barnett/microViz@39a1175)
#>  multtest       2.44.0     2020-04-27 [1] Bioconductor                           
#>  munsell        0.5.0      2018-06-12 [1] CRAN (R 4.0.0)                         
#>  nlme           3.1-148    2020-05-24 [1] CRAN (R 4.0.1)                         
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
#>  R6             2.4.1      2019-11-12 [1] CRAN (R 4.0.0)                         
#>  RColorBrewer   1.1-2      2014-12-07 [1] CRAN (R 4.0.0)                         
#>  Rcpp           1.0.5      2020-07-06 [1] CRAN (R 4.0.2)                         
#>  remotes        2.2.0      2020-07-21 [1] CRAN (R 4.0.3)                         
#>  reshape2       1.4.4      2020-04-09 [1] CRAN (R 4.0.0)                         
#>  rhdf5          2.32.2     2020-07-03 [1] Bioconductor                           
#>  Rhdf5lib       1.10.1     2020-07-09 [1] Bioconductor                           
#>  rlang          0.4.7      2020-07-09 [1] CRAN (R 4.0.2)                         
#>  rmarkdown      2.3        2020-06-18 [1] CRAN (R 4.0.2)                         
#>  rprojroot      1.3-2      2018-01-03 [1] CRAN (R 4.0.0)                         
#>  Rtsne          0.15       2018-11-10 [1] CRAN (R 4.0.2)                         
#>  S4Vectors      0.26.1     2020-05-16 [1] Bioconductor                           
#>  scales         1.1.1      2020-05-11 [1] CRAN (R 4.0.0)                         
#>  sessioninfo    1.1.1      2018-11-05 [1] CRAN (R 4.0.0)                         
#>  stringi        1.4.6      2020-02-17 [1] CRAN (R 4.0.0)                         
#>  stringr        1.4.0      2019-02-10 [1] CRAN (R 4.0.0)                         
#>  survival       3.2-3      2020-06-13 [1] CRAN (R 4.0.2)                         
#>  testthat       2.3.2      2020-03-02 [1] CRAN (R 4.0.0)                         
#>  tibble         3.0.3      2020-07-10 [1] CRAN (R 4.0.2)                         
#>  tidyr          1.1.0      2020-05-20 [1] CRAN (R 4.0.0)                         
#>  tidyselect     1.1.0      2020-05-11 [1] CRAN (R 4.0.0)                         
#>  usethis        1.6.3      2020-09-17 [1] CRAN (R 4.0.3)                         
#>  vctrs          0.3.2      2020-07-15 [1] CRAN (R 4.0.2)                         
#>  vegan        * 2.5-6      2019-09-01 [1] CRAN (R 4.0.0)                         
#>  withr          2.3.0      2020-09-22 [1] CRAN (R 4.0.2)                         
#>  xfun           0.15       2020-06-21 [1] CRAN (R 4.0.2)                         
#>  XVector        0.28.0     2020-04-27 [1] Bioconductor                           
#>  yaml           2.2.1      2020-02-01 [1] CRAN (R 4.0.0)                         
#>  zlibbioc       1.34.0     2020-04-27 [1] Bioconductor                           
#> 
#> [1] C:/Program Files/R/R-4.0.1/library
```
