
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microViz <a href='https://david-barnett.github.io/microViz/index.html'><img src="man/figures/logo.png" align="right" height="180" width="156"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/david-barnett/microViz/workflows/R-CMD-check/badge.svg)](https://github.com/david-barnett/microViz/actions)
[![codecov](https://codecov.io/gh/david-barnett/microViz/branch/main/graph/badge.svg?token=C1EoVkhnxA)](https://codecov.io/gh/david-barnett/microViz)
![GitHub R package
version](https://img.shields.io/github/r-package/v/david-barnett/microViz?label=Latest)
![GitHub release (including
pre-releases)](https://img.shields.io/github/v/release/david-barnett/microViz?include_prereleases&label=Release)
![Docker Image Version (latest by
date)](https://img.shields.io/docker/v/barnettdavid/microviz-rocker-verse?color=blue&label=Docker)
[![r-universe microViz
status](https://david-barnett.r-universe.dev/badges/microViz)](https://david-barnett.r-universe.dev/ui#package:microViz)
[![JOSS
article](https://joss.theoj.org/papers/4547b492f224a26d96938ada81fee3fa/status.svg)](https://joss.theoj.org/papers/4547b492f224a26d96938ada81fee3fa)
[![Citations](https://img.shields.io/badge/Citations-~23-blueviolet)](https://scholar.google.com/scholar?oi=bibs&hl=en&cites=5439940108464463894)
[![Zenodo
DOI](https://zenodo.org/badge/307119750.svg)](https://zenodo.org/badge/latestdoi/307119750)
<!-- badges: end -->

## Overview

:package: `microViz` is an R package for analysis and visualization of
microbiome sequencing data.

:hammer: `microViz` functions are intended to be easy to use and
flexible.

:microscope: `microViz` extends and complements popular microbial
ecology packages like `phyloseq`, `vegan`, & `microbiome`.

## Upgrading microViz versions ≤ 0.9.7 to version 0.10.0 +

microViz version 0.10.0 introduces changes that may break some code
written with older versions of microViz. If you were already using
microViz prior to version 0.10.0 please read [this
document](articles/ps_extra-replaced.html) before upgrading to the
latest version.

## Learn more

:paperclip: This website is the best place for documentation and
examples: <https://david-barnett.github.io/microViz/>

- [**This ReadMe**](https://david-barnett.github.io/microViz/) shows a
  few example analyses

- **[The Getting Started
  guide](https://david-barnett.github.io/microViz/articles/shao19-analyses.html)**
  shows more example analyses and gives advice on using microViz with
  your own data

- **[The Reference
  page](https://david-barnett.github.io/microViz/reference/index.html)**
  lists all functions and links to help pages and examples

- **[The News
  page](https://david-barnett.github.io/microViz/news/index.html)**
  describes important changes in new microViz package versions

- **The Articles pages** give tutorials and further examples

  - [Working with phyloseq
    objects](https://david-barnett.github.io/microViz/articles/web-only/phyloseq.html)

  - [Fixing your taxa table with
    tax_fix](https://david-barnett.github.io/microViz/articles/web-only/tax-fixing.html)

  - [Creating ordination
    plots](https://david-barnett.github.io/microViz/articles/web-only/ordination.html)
    (e.g. PCA or PCoA)

  - [Interactive ordination plots with
    ord_explore](https://david-barnett.github.io/microViz/articles/web-only/ordination-interactive.html)

  - [Visualising taxonomic compositions with
    comp_barplot](https://david-barnett.github.io/microViz/articles/web-only/compositions.html)

  - [Heatmaps of microbiome composition and
    correlation](https://david-barnett.github.io/microViz/articles/web-only/heatmaps.html)

  - [Modelling and plotting individual taxon associations with
    taxatrees](https://david-barnett.github.io/microViz/articles/web-only/modelling-taxa.html)

  - More coming soon(ish)! Post on [GitHub
    discussions](https://github.com/david-barnett/microViz/discussions)
    if you have questions/requests

## Installation

microViz is not (yet) available from CRAN. You can install microViz from
R Universe, or from GitHub.

I recommend you first install the Bioconductor dependencies using the
code below.

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("phyloseq", "microbiome", "ComplexHeatmap"), update = FALSE)
```

### Installation of microViz from R Universe

``` r
install.packages(
  "microViz",
  repos = c(davidbarnett = "https://david-barnett.r-universe.dev", getOption("repos"))
)
```

I also highly recommend you install the following suggested CRAN
packages.

``` r
install.packages("ggraph") # for taxatree_plots()
install.packages("DT") # for tax_fix_interactive()
install.packages("corncob") # for example datasets and beta binomial models
```

### Installation of microViz from GitHub

``` r
# Installing from github requires the devtools package
install.packages("devtools")
# Windows users will also need to have RTools installed! http://jtleek.com/modules/01_DataScientistToolbox/02_10_rtools/

# To install the very latest version:
devtools::install_github("david-barnett/microViz")

# To install a specific "release" version of this package
devtools::install_github("david-barnett/microViz@0.10.1") 
```

### Installation notes

**:apple: macOS** **users** - might need to install
[xquartz](https://www.xquartz.org/) to make the heatmaps work (to do
this with homebrew, run the following command in your mac’s Terminal:
`brew install --cask xquartz`

:package: I highly recommend using
[renv](https://rstudio.github.io/renv/index.html) for managing your R
package installations across multiple projects.

:whale: For Docker users an image with the main branch installed is
available at:
<https://hub.docker.com/r/barnettdavid/microviz-rocker-verse>

:date: microViz is tested to work with R version 4 on Windows, MacOS,
and Ubuntu 18 and 20. R version 3.6.\* should probably work, but I don’t
formally test this.

## Interactive ordination exploration

``` r
library(microViz)
#> microViz version 0.10.5 - Copyright (C) 2022 David Barnett
#> ! Website: https://david-barnett.github.io/microViz
#> ✔ Useful?  For citation details, run: `citation("microViz")`
#> ✖ Silence? `suppressPackageStartupMessages(library(microViz))`
```

microViz provides a Shiny app for an easy way to start exploring your
microbiome data: all you need is a phyloseq object.

``` r
# example data from corncob package
pseq <- corncob::ibd_phylo %>%
  tax_fix() %>%
  phyloseq_validate()
```

``` r
ord_explore(pseq) # gif generated with microViz version 0.7.4 (plays at 1.75x speed)
```

![](vignettes/web-only/images/20210429_ord_explore_x175.gif)

## Example analyses

``` r
library(phyloseq)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following object is masked from 'package:testthat':
#> 
#>     matches
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

``` r
# get some example data
data("dietswap", package = "microbiome")

# create a couple of numerical variables to use as constraints or conditions
dietswap <- dietswap %>%
  ps_mutate(
    weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
    female = if_else(sex == "female", true = 1, false = 0),
    african = if_else(nationality == "AFR", true = 1, false = 0)
  )
# add a couple of missing values to show how microViz handles missing data
sample_data(dietswap)$african[c(3, 4)] <- NA
```

### Looking at your data

You have quite a few samples in your phyloseq object, and would like to
visualize their compositions. Perhaps these example data differ by
participant nationality?

``` r
dietswap %>%
  comp_barplot(
    tax_level = "Genus", n_taxa = 15, other_name = "Other",
    taxon_renamer = function(x) stringr::str_remove(x, " [ae]t rel."),
    palette = distinct_palette(n = 15, add = "grey90"),
    merge_other = FALSE, bar_outline_colour = "darkgrey"
  ) +
  coord_flip() +
  facet_wrap("nationality", nrow = 1, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
#> Registered S3 method overwritten by 'seriation':
#>   method         from 
#>   reorder.hclust vegan
```

<img src="man/figures/README-bars-1.png" width="100%" />

``` r
htmp <- dietswap %>%
  ps_mutate(nationality = as.character(nationality)) %>%
  tax_transform("log2", add = 1, chain = TRUE) %>%
  comp_heatmap(
    taxa = tax_top(dietswap, n = 30), grid_col = NA, name = "Log2p",
    taxon_renamer = function(x) stringr::str_remove(x, " [ae]t rel."),
    colors = heat_palette(palette = viridis::turbo(11)),
    row_names_side = "left", row_dend_side = "right", sample_side = "bottom",
    sample_anno = sampleAnnotation(
      Nationality = anno_sample_cat(
        var = "nationality", col = c(AAM = "grey35", AFR = "grey85"),
        box_col = NA, legend_title = "Nationality", size = grid::unit(4, "mm")
      )
    )
  )

ComplexHeatmap::draw(
  object = htmp, annotation_legend_list = attr(htmp, "AnnoLegends"),
  merge_legends = TRUE
)
```

<img src="man/figures/README-compheatmap-1.png" width="100%" />

### Example ordination plot workflow

Ordination methods can also help you to visualize if overall microbial
ecosystem composition differs markedly between groups, e.g. BMI.

Here is one option as an example:

1.  Filter out rare taxa (e.g. remove Genera not present in at least 10%
    of samples) - use `tax_filter()`
2.  Aggregate the taxa into bacterial families (for example) - use
    `tax_agg()`
3.  Transform the microbial data with the centered-log-ratio
    transformation - use `tax_transform()`
4.  Perform PCA with the clr-transformed features (equivalent to
    Aitchison distance PCoA) - use `ord_calc()`
5.  Plot the first 2 axes of this PCA ordination, colouring samples by
    group and adding taxon loading arrows to visualize which taxa
    generally differ across your samples - use `ord_plot()`
6.  Customise the theme of the ggplot as you like and/or add features
    like ellipses or annotations

``` r
# perform ordination
unconstrained_aitchison_pca <- dietswap %>%
  tax_filter(min_prevalence = 0.1, tax_level = "Genus") %>%
  tax_agg("Family") %>%
  tax_transform("clr") %>%
  ord_calc()
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.
# ord_calc will automatically infer you want a "PCA" here
# specify explicitly with method = "PCA", or you can pick another method

# create plot
pca_plot <- unconstrained_aitchison_pca %>%
  ord_plot(
    plot_taxa = 1:6, colour = "bmi_group", size = 1.5,
    tax_vec_length = 0.325,
    tax_lab_style = tax_lab_style(max_angle = 90, aspect_ratio = 0.5),
    auto_caption = 8
  )

# customise plot
customised_plot <- pca_plot +
  stat_ellipse(aes(linetype = bmi_group, colour = bmi_group), linewidth = 0.3) + # linewidth not size, since ggplot 3.4.0
  scale_colour_brewer(palette = "Set1") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 0.5, clip = "off") # makes rotated labels align correctly

# show plot
customised_plot
```

<img src="man/figures/README-ordination-plot-1.png" width="100%" />

### PERMANOVA

You visualised your ordinated data in the plot above. Below you can see
how to perform a PERMANOVA to test the significance of BMI’s association
with overall microbial composition. This example uses the Family-level
aitchison distance to correspond with the plot above.

``` r
# calculate distances
aitchison_dists <- dietswap %>%
  tax_filter(min_prevalence = 0.1) %>%
  tax_transform("identity", rank = "Family") %>%
  dist_calc("aitchison")
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.

# the more permutations you request, the longer it takes
# but also the more stable and precise your p-values become
aitchison_perm <- aitchison_dists %>%
  dist_permanova(
    seed = 1234, # for set.seed to ensure reproducibility of random process
    n_processes = 1, n_perms = 99, # you should use at least 999!
    variables = "bmi_group"
  )
#> 2023-01-15 16:35:31 - Starting PERMANOVA with 99 perms with 1 processes
#> 2023-01-15 16:35:31 - Finished PERMANOVA

# view the permanova results
perm_get(aitchison_perm) %>% as.data.frame()
#>            Df  SumOfSqs         R2        F Pr(>F)
#> bmi_group   2  104.0678 0.04177157 4.773379   0.01
#> Residual  219 2387.2862 0.95822843       NA     NA
#> Total     221 2491.3540 1.00000000       NA     NA

# view the info stored about the distance calculation
info_get(aitchison_perm)
#> psExtra info:
#> tax_agg = "Family" tax_trans = "identity" dist_method = "aitchison"
```

### Constrained partial ordination

You could visualise the effect of the (numeric/logical) variables in
your permanova directly using the `ord_plot` function with constraints
(and conditions).

``` r
perm2 <- aitchison_dists %>%
  dist_permanova(variables = c("weight", "african", "sex"), seed = 321)
#> Dropping samples with missings: 2
#> 2023-01-15 16:35:31 - Starting PERMANOVA with 999 perms with 1 processes
#> 2023-01-15 16:35:33 - Finished PERMANOVA
```

We’ll visualise the effect of nationality and bodyweight on sample
composition, after first removing the effect of sex.

``` r
perm2 %>%
  ord_calc(constraints = c("weight", "african"), conditions = "female") %>%
  ord_plot(
    colour = "nationality", size = 2.5, alpha = 0.35,
    auto_caption = 7,
    constraint_vec_length = 1,
    constraint_vec_style = vec_constraint(1.5, colour = "grey15"),
    constraint_lab_style = constraint_lab_style(
      max_angle = 90, size = 3, aspect_ratio = 0.35, colour = "black"
    )
  ) +
  stat_ellipse(aes(colour = nationality), linewidth = 0.2) + # linewidth not size since ggplot 3.4.0
  scale_color_brewer(palette = "Set1") +
  coord_fixed(ratio = 0.35, clip = "off") +
  theme(legend.position = c(0.9, 0.1), legend.background = element_rect())
#> 
#> Centering (mean) and scaling (sd) the constraints and/or conditions:
#>  weight
#>  african
#>  female
```

<img src="man/figures/README-constrained-ord-plot-1.png" width="100%" />

### Correlation Heatmaps

microViz heatmaps are powered by `ComplexHeatmap` and annotated with
taxa prevalence and/or abundance.

``` r
# set up the data with numerical variables and filter to top taxa
psq <- dietswap %>%
  ps_mutate(
    weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
    female = if_else(sex == "female", true = 1, false = 0),
    african = if_else(nationality == "AFR", true = 1, false = 0)
  ) %>%
  tax_filter(
    tax_level = "Genus", min_prevalence = 1 / 10, min_sample_abundance = 1 / 10
  ) %>%
  tax_transform("identity", rank = "Genus")
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.

# randomly select 30 taxa from the 50 most abundant taxa (just for an example)
set.seed(123)
taxa <- sample(tax_top(psq, n = 50), size = 30)
# actually draw the heatmap
cor_heatmap(
  data = psq, taxa = taxa,
  taxon_renamer = function(x) stringr::str_remove(x, " [ae]t rel."),
  tax_anno = taxAnnotation(
    Prev. = anno_tax_prev(undetected = 50),
    Log2 = anno_tax_box(undetected = 50, trans = "log2", zero_replace = 1)
  )
)
```

<img src="man/figures/README-corheatmap-1.png" width="100%" />

## Citation

:innocent: If you find any part of microViz useful to your work, please
consider citing the JOSS article:

Barnett et al., (2021). microViz: an R package for microbiome data
visualization and statistics. Journal of Open Source Software, 6(63),
3201, <https://doi.org/10.21105/joss.03201>

## Contributing

Bug reports, questions, suggestions for new features, and other
contributions are all welcome. Feel free to create a [GitHub
Issue](https://github.com/david-barnett/microViz/issues) or write on the
[Discussions](https://github.com/david-barnett/microViz/discussions)
page. Alternatively you could also contact me (David) on Twitter
[@\_david_barnett\_](https://twitter.com/_david_barnett_) .

This project is released with a [Contributor Code of
Conduct](https://david-barnett.github.io/microViz/CODE_OF_CONDUCT.html)
and by participating in this project you agree to abide by its terms.

## Session info

``` r
sessionInfo()
#> R version 4.2.1 (2022-06-23)
#> Platform: aarch64-apple-darwin20 (64-bit)
#> Running under: macOS Ventura 13.1
#> 
#> Matrix products: default
#> BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
#> LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] ggplot2_3.4.0   dplyr_1.0.10    phyloseq_1.40.0 microViz_0.10.5
#> [5] testthat_3.1.5  devtools_2.4.5  usethis_2.1.6  
#> 
#> loaded via a namespace (and not attached):
#>   [1] circlize_0.4.15        plyr_1.8.8             igraph_1.3.5          
#>   [4] selectr_0.4-2          splines_4.2.1          GenomeInfoDb_1.32.4   
#>   [7] digest_0.6.30          foreach_1.5.2          ca_0.71.1             
#>  [10] htmltools_0.5.3        magick_2.7.3           viridis_0.6.2         
#>  [13] fansi_1.0.3            magrittr_2.0.3         memoise_2.0.1         
#>  [16] cluster_2.1.4          doParallel_1.0.17      remotes_2.4.2         
#>  [19] ComplexHeatmap_2.12.1  Biostrings_2.64.1      matrixStats_0.63.0    
#>  [22] prettyunits_1.1.1      colorspace_2.0-3       corncob_0.3.0         
#>  [25] rvest_1.0.3            xfun_0.35              callr_3.7.3           
#>  [28] crayon_1.5.2           RCurl_1.98-1.9         jsonlite_1.8.3        
#>  [31] survival_3.4-0         iterators_1.0.14       ape_5.6-2             
#>  [34] glue_1.6.2             registry_0.5-1         gtable_0.3.1          
#>  [37] zlibbioc_1.42.0        XVector_0.36.0         GetoptLong_1.0.5      
#>  [40] pkgbuild_1.4.0         Rhdf5lib_1.18.2        shape_1.4.6           
#>  [43] BiocGenerics_0.42.0    scales_1.2.1           DBI_1.1.3             
#>  [46] miniUI_0.1.1.1         Rcpp_1.0.9             gridtext_0.1.5        
#>  [49] viridisLite_0.4.1      xtable_1.8-4           clue_0.3-63           
#>  [52] stats4_4.2.1           profvis_0.3.7          htmlwidgets_1.5.4     
#>  [55] httr_1.4.4             RColorBrewer_1.1-3     ellipsis_0.3.2        
#>  [58] urlchecker_1.0.1       pkgconfig_2.0.3        farver_2.1.1          
#>  [61] utf8_1.2.2             tidyselect_1.2.0       labeling_0.4.2        
#>  [64] rlang_1.0.6            reshape2_1.4.4         later_1.3.0           
#>  [67] munsell_0.5.0          tools_4.2.1            cachem_1.0.6          
#>  [70] cli_3.4.1              generics_0.1.3         ade4_1.7-20           
#>  [73] evaluate_0.18          biomformat_1.24.0      stringr_1.4.1         
#>  [76] fastmap_1.1.0          yaml_2.3.6             processx_3.8.0        
#>  [79] knitr_1.41             fs_1.5.2               purrr_0.3.5           
#>  [82] nlme_3.1-160           mime_0.12              xml2_1.3.3            
#>  [85] brio_1.1.3             compiler_4.2.1         rstudioapi_0.14       
#>  [88] curl_4.3.3             png_0.1-7              tibble_3.1.8          
#>  [91] stringi_1.7.8          highr_0.9              ps_1.7.2              
#>  [94] lattice_0.20-45        Matrix_1.5-3           commonmark_1.8.1      
#>  [97] markdown_1.4           vegan_2.6-4            microbiome_1.20.0     
#> [100] permute_0.9-7          multtest_2.52.0        vctrs_0.5.1           
#> [103] pillar_1.8.1           lifecycle_1.0.3        rhdf5filters_1.8.0    
#> [106] GlobalOptions_0.1.2    data.table_1.14.6      bitops_1.0-7          
#> [109] seriation_1.4.0        httpuv_1.6.6           R6_2.5.1              
#> [112] promises_1.2.0.1       TSP_1.2-1              gridExtra_2.3         
#> [115] IRanges_2.30.1         sessioninfo_1.2.2      codetools_0.2-18      
#> [118] MASS_7.3-58.1          assertthat_0.2.1       pkgload_1.3.2         
#> [121] rhdf5_2.40.0           rjson_0.2.21           withr_2.5.0           
#> [124] S4Vectors_0.34.0       GenomeInfoDbData_1.2.8 ggtext_0.1.2          
#> [127] mgcv_1.8-41            parallel_4.2.1         grid_4.2.1            
#> [130] tidyr_1.2.1            rmarkdown_2.18         Cairo_1.6-0           
#> [133] Rtsne_0.16             Biobase_2.56.0         shiny_1.7.3
```
