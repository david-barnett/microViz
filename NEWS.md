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

