# Create node and edge dataframes for taxatree_plots

Mostly you will not have to use these functions directly: instead call
`taxatree_plots` with the output of `taxatree_stats`

- `taxatree_nodes` creates taxon nodes and calculates a summary
  statistic about each taxon (given by `fun`). Takes a psExtra or
  phyloseq object.

- `taxatree_edges` uses the output of `taxatree_nodes` to create a
  dataframe of edges.

## Usage

``` r
taxatree_nodes(
  ps,
  fun = list(sum = sum),
  ranks = "all",
  .sort = NULL,
  .use_counts = TRUE
)

taxatree_edges(nodes_df)
```

## Arguments

- ps:

  phyloseq object or psExtra

- fun:

  function to calculate for each taxon/node

- ranks:

  selection of taxonomic ranks to make nodes for ("all", or names)

- .sort:

  sort nodes by "increasing" or "decreasing" values of fun function
  result

- .use_counts:

  use count data if available (instead of transformed data)

- nodes_df:

  dataframe output from taxatree_nodes

## Details

`taxatree_nodes` makes nodes for taxa at all ranks or for a list of
consecutive ranks (plus a root rank if tree is not rooted).
