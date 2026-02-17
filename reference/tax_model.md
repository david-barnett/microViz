# Statistical modelling for individual taxa in a phyloseq

`tax_model` provides a simple framework to statistically model the
abundance of individual taxa in your data. You can choose which type of
statistical model you want to fit, and you can choose at which rank and
(optionally) which specific taxa to fit statistical models for.
`tax_model` takes a phyloseq and returns a list of statistical models,
one model for each taxon. The same independent variables are used for
all models, as specified in `variables` or `formula` argument (latter
takes precedence).

`taxatree_models` runs `tax_model` on every taxon at multiple taxonomic
ranks (you choose which ranks with the plural `ranks` argument), and
returns the results as a named nested list designed for use with
`taxatree_plots`. One list per rank, one model per taxon at each rank.

`type = "bbdml"` will run beta binomial regression model(s) using the
`corncob` package. For bbdml the same formula/variables is/are used for
modelling both the abundance and dispersion parameters.

## Usage

``` r
tax_model(
  ps,
  rank,
  type = "lm",
  variables = NULL,
  formula = NULL,
  taxa = NULL,
  use_future = FALSE,
  return_psx = TRUE,
  checkVars = TRUE,
  checkNA = "warning",
  verbose = TRUE,
  trans = "identity",
  trans_args = list(),
  ...
)
```

## Arguments

- ps:

  phyloseq object

- rank:

  name of taxonomic rank to aggregate to and model taxa at

- type:

  name of modelling function to use, or the function itself

- variables:

  vector of variable names, to be used as model formula right hand side.
  If variables is a list, not a vector, a model is fit for each entry in
  list.

- formula:

  Right hand side of a formula, as a formula object or character string.
  Or a list of these. (alternative to variables argument, do not provide
  both)

- taxa:

  taxa to model (named, numbered, logical selection, or defaulting to
  all if NULL)

- use_future:

  if TRUE parallel processing with future is possible, see details.

- return_psx:

  if TRUE, list of results will be returned attached to psExtra object

- checkVars:

  should the predictor variables be checked for zero variance?

- checkNA:

  One of "stop", "warning", "message", or "allow", which indicates
  whether to check predictor variables for NAs, and how to report any
  NAs if present?

- verbose:

  message about progress and any taxa name modifications

- trans:

  name of tax_transform transformation to apply to aggregated taxa
  before fitting statistical models

- trans_args:

  named list of any additional arguments to tax_transform e.g.
  list(zero_replace = "halfmin")

- ...:

  extra args passed directly to modelling function

## Value

psExtra with named list of model objects attached. Or a list of lists of
models (if multiple models per taxon). Or just a list (of lists), if
return_psx is FALSE.

## Details

`tax_model` and `taxatree_models` can use parallel processing with the
`future` package. This can speed up analysis if you have many taxa to
model. Set use_future = TRUE and run a line like this before doing your
modelling: `future::plan(future::multisession, workers = 3)` (This
requires the future and future.apply packages to be installed.)

## See also

[`tax_models_get`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
for the accessor to retrieve model results from psExtra

[`taxatree_models`](https://david-barnett.github.io/microViz/reference/taxatree_models.md)
for more details on the underlying approach

## Examples

``` r
library(corncob)
library(dplyr)

data(dietswap, package = "microbiome")
ps <- dietswap

# create some binary variables for easy visualization
ps <- ps %>% ps_mutate(
  female = if_else(sex == "female", 1, 0, NaN),
  overweight = if_else(bmi_group == "overweight", 1, 0, NaN),
  obese = if_else(bmi_group == "obese", 1, 0, NaN)
)

# This example HITChip dataset has some taxa with the same name for phylum and family...
# We can fix problems like this with the tax_prepend_ranks function
ps <- tax_prepend_ranks(ps)

# filter out rare taxa (it is often difficult to fit multivariable models to rare taxa)
ps <- ps %>% tax_filter(min_prevalence = 0.1, min_total_abundance = 10000)
#> Proportional min_prevalence given: 0.1 --> min 23/222 samples.

# specify variables used for modelling
VARS <- c("female", "overweight", "obese")

# Model first 3 genera using all VARS as predictors (just for a quick test)
models <- tax_model(ps, type = "bbdml", rank = "Genus", taxa = 1:3, variables = VARS)
#> Modelling: G: Akkermansia
#> Modelling: G: Allistipes et rel.
#> Modelling: G: Anaerostipes caccae et rel.

# Alternative method using formula arg instead of variables to produce identical results
models2 <- tax_model(
  ps = ps, rank = "Genus", type = "bbdml",
  taxa = 1:3, formula = ~ female + overweight + obese, return_psx = FALSE
)
#> Modelling: G: Akkermansia
#> Modelling: G: Allistipes et rel.
#> Modelling: G: Anaerostipes caccae et rel.
all.equal(models, models2) # should be TRUE
#> [1] "Modes: S4, list"                                                  
#> [2] "Lengths: 1, 3"                                                    
#> [3] "names for current but not for target"                             
#> [4] "Attributes: < names for target but not for current >"             
#> [5] "Attributes: < Length mismatch: comparison on first 0 components >"

# Model only one genus, NOTE the modified name,
# which was returned by tax_prepend_ranks defaults
models3 <- ps %>%
  tax_model(
    rank = "Genus", type = "bbdml",
    taxa = "G: Bacteroides fragilis et rel.", variables = VARS
  )
#> Modelling: G: Bacteroides fragilis et rel.

# Model all taxa at multiple taxonomic ranks (ranks 1 and 2)
# using only female variable as predictor
models4 <- taxatree_models(
  ps = ps, type = "bbdml", ranks = 1:2, formula = ~female, verbose = FALSE
)

# modelling proportions with simple linear regression is also possible via type = lm
# and transforming the taxa to compositional first
models_lm <- ps %>%
  tax_transform("compositional") %>%
  tax_model(rank = "Genus", taxa = 1:3, variables = VARS, type = "lm")
#> Modelling: G: Akkermansia
#> Modelling: G: Allistipes et rel.
#> Modelling: G: Anaerostipes caccae et rel.
```
