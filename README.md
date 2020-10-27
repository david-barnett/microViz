
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microViz

<!-- badges: start -->

<!-- badges: end -->

The goal of microViz is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("david-barnett/microViz")
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

## Example ordination plot workflow

PCA with clr-transformed features (equivalent to aitchison distance).

``` r
# compute and plot ordinations for demonstration of conditioning
unconstrained_aitchison_pca <- dietswap %>%
  tax_agg("Family") %>%
  tax_transform('clr') %>%
  ordin8(method = 'RDA')

unconstrained_aitchison_pca %>%
plot_ordin8(plot_taxa = 1:5, colour = 'bmi_group', auto_title = TRUE) +
stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
```

<img src="man/figures/README-ordination-plot-1.png" width="100%" />
