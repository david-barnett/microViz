# microViz 0.0.6

# microViz 0.0.5

## Features

* `ps_dedupe` bug fix method = "readcount" now correctly keeps all samples that "first" or "last" methods do

# microViz 0.0.4

## Features

* `ps_arrange` allows reordering of `phyloseq` samples by values of variables in `sample_data` or `otu_table` (wrapper around `dplyr`'s `arrange` function)
* `ps_otu2samdat` add taxon abundance variables from `phyloseq` `otu_table` to `sample_data` for use in plotting etc

# microViz 0.0.3

## Features

* `ps_mutate` allowing easy and piped modification of `phyloseq` `sample_data` (wrapper around `dplyr`'s `mutate` function)
* `ps_join` allows easy and piped addition of a dataframe of data to `phyloseq` `sample_data` (wrapper around `dplyr`'s `*_join` functions)

# microViz 0.0.2

## Breaking changes

* `plot_ordin8` arguments changed to allow easier sizing and styling of all vectors and labels
* `plot_ordin8` default styling of taxon and constraint vectors and labels is changed: background vectors are now semi-transparent and dashed lines are not used any more by default (but can be set)

## Other

* `ordin8` and `plot_ordin8` get basic support for CCA and NMDS methods finally
* `plot_comp_bar` gets `order_samples_with_all_taxa` and `tax_transform_for_ordering`
* Documentation of manual sample ordering across `plot_comp_bar` groups added to Visualising Compositions article on website.
* Documentation of experimental polar coordinates and PCA_angle sorting added in new article called PCA-sorted polar composition plots, on website.

# microViz 0.0.1

## Main changes

* `phyloseq_validate` now fixes otu_tables stored as integers and messages user about suspicious or NA tax_table entries and this happens as part of `plot_comp_bar` and `plot_ordin8`
* `plot_comp_bar` and `plot_ordin8` gain `taxon_renamer` argument to allow you to customise the taxon names on these plots
* `plot_comp_bar` can now handle missing values in the grouping variable by converting NAs to "NA"s

* Some functions renamed for naming consistency: 
    - `prepend_ranks` -> `tax_prepend_ranks` 
    - `tax_model_corncob` -> `model_tax_corncob`
    - `corncob_models_to_var_stats` -> `modelsmodels2stats_corncob`
    - `tax_tree_` `nodes`/`edges`/`plot` -> `taxatree_` ...

## Other

* Added a `NEWS.md` file to track changes to the package.

