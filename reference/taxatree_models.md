# Statistical modelling for individual taxa across multiple ranks

`taxatree_models` runs `tax_model` on every taxon at multiple taxonomic
ranks (you choose which ranks with the plural `ranks` argument). It
returns the results as a named nested list of models attached to a
psExtra object. One list per rank, one model per taxon at each rank.

The result can then be used with `taxatree_models2stats` to extract a
dataframe of statistics for use with `taxatree_plots`.

## Usage

``` r
taxatree_models(
  ps,
  ranks = NULL,
  type = "lm",
  variables = NULL,
  formula = NULL,
  use_future = FALSE,
  checkVars = TRUE,
  checkNA = "warning",
  verbose = "rank",
  trans = "identity",
  trans_args = list(),
  ...
)
```

## Arguments

- ps:

  phyloseq object or psExtra

- ranks:

  vector of rank names at which to aggregate taxa for modelling

- type:

  name of regression modelling function, or the function itself

- variables:

  vector of variable names, to be used as model formula right hand side.
  If variables is a list, not a vector, a model is fit for each entry in
  list.

- formula:

  Right hand side of a formula, as a formula object or character string.
  Or a list of these. (alternative to variables argument, do not provide
  both)

- use_future:

  if TRUE parallel processing with future is possible, see details of
  ?tax_model.

- checkVars:

  check variance of variables?

- checkNA:

  check variables for NAs?

- verbose:

  message about progress: "rank" only notifies which rank is being
  processed; TRUE notifies you about each taxon being processed; FALSE
  for no messages.

- trans:

  name of tax_transform transformation to apply to aggregated taxa
  before fitting statistical models

- trans_args:

  named list of any additional arguments to tax_transform e.g.
  list(zero_replace = "halfmin")

- ...:

  extra arguments are passed directly to modelling function

## See also

[`tax_model`](https://david-barnett.github.io/microViz/reference/tax_model.md)
for more details and examples

[`taxatree_plots`](https://david-barnett.github.io/microViz/reference/taxatree_plots.md)
for how to plot the output of `taxatree_models`
