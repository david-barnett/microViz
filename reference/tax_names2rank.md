# Add taxa_names as last column in phyloseq tax_table

The taxa names in your phyloseq may specify a further unique
classification of your taxa, e.g. ASVs, that is not otherwise
represented in the tax_table itself. This function fixes that, and
allows you to include this level in taxatree_plots for example.

## Usage

``` r
tax_names2rank(data, colname = "unique")
```

## Arguments

- data:

  phyloseq object, or psExtra or tax_table (taxonomyTable)

- colname:

  name of new rank to add at right side of tax_table

## Value

same class object as passed in to data
