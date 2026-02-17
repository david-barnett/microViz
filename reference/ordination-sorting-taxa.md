# Order taxa in phyloseq by their loading vectors

`tax_sort_ord` reorders taxa in a phyloseq object based on the relative
length of their taxa scores / "loading" vector lengths on 1 or 2
ordination axes.

`ord_order_taxa` gets the taxa names in order from the ordination
contained in a psExtra object. This is used internally by
`tax_sort_ord`.

## Usage

``` r
tax_sort_ord(ps, ord, axes = 1:2, scaling = 2)

ord_order_taxa(ord, axes = 1:2, scaling = 2)
```

## Arguments

- ps:

  phyloseq object to be sorted

- ord:

  psExtra with ordination object

- axes:

  which axes to use for sorting? numerical vector of length 1 or 2

- scaling:

  Type 2, or type 1 scaling. For more info, see
  <https://sites.google.com/site/mb3gustame/constrained-analyses/redundancy-analysis>.
  Either "species" or "site" scores are scaled by (proportional)
  eigenvalues, and the other set of scores is left unscaled (from
  ?vegan::scores.cca)

## See also

- These functions were created to support ordering of taxa bars on
  `ord_plot_iris`

- `ps_sort_ord` for ordering samples in phyloseq by ordination
