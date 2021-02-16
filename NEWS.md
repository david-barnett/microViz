# microViz (development version)

## Bug fixes
* `tax_fill_unknowns` gives informative error on data with only one taxonomic rank
* `tax_filter` ps argument default removed

# microViz 0.4.1

## Bug fixes:
* `ord_calc` now correctly returns subsetted phyloseq in ps_extra obj after constrained ordination
* `taxatree_plots` and `taxatree_plotkey` now have reactive automatic **minimum** node and edge sizes that depend on the set maxes

# microViz 0.4.0

## Breaking changes
* `dist_permanova` replaces `permanova` for naming consistency and guiding user
* `comp_barplot` replaces `plot_comp_bar` in anticipation of (a) future heatmap function(s) named comp_heatmap or similar

## Features

New **"ps_extra"** class (S3) conveniently stores phyloseq object alongside any calculated distance matrix, ordination and permanova models, as well as records of `tax_agg` and `tax_transform` calls. "ps_extra" class objects have a pretty and compact print method, a simple list structure, and convenient accessor functions to return each component: `ps_get`, `info_get`, `dist_get`, `ord_get`, `perm_get`, `bdisp_get`.

# microViz 0.3.2

## Features
* Taxon modelling updates: 
    * `tax_model` and `taxatree_models` can handle linear modeling e.g. on compositional (TS-Scaled) data
    * `taxatree_plots` has more sensible defaults (automatic symmetrical colour limits and variable selection based on model type)

# microViz 0.3.1

## Breaking changes
* `ord_plot_iris` the `data` arg is replaced with `ord` and conditionally optional `ps` arg for when data in `ord` have been transformed
* `permanova` always uses adonis2 now, so that arg is **removed**, and replaced with `by` argument to set sums of squares choice

## Features
* `ord_plot` gets a `center` argument to center expand the plot limits to center around zero (useful when pairing with `ord_plot_iris`)
* `ord_explore` can now also display ordinations that don't use distances like PCA and RDA (as well as PCoA of course)
* `ord_explore` gains a `ps` arg (for untransformed version) and other tweaks to facilitate using transformed data in `ord`

## Other fixes
* `ord_plot_iris` annotation args are now NULL by default. 
* `microbiome::aggregate_top_taxa` is copied directly into `microViz` to avoid its coming deprecation and the current warning.

# microViz 0.2.0

## Features

* `taxatree_models` and `taxatree_plots` bring tree-based visualisations of statistical models

## Breaking changes

* `tax_model` replaces `tax_model_corncob` as a function that will eventually be generalised to use other model types 
* `taxatree_plots` and `taxatree_plotkey` replaces the rudimentary `taxatree_plot`

# microViz 0.1.2

## Features

* `ord_explore` allows interactive exploration of the composition of ordinated samples using `Shiny`

# microViz 0.1.1

## Features

* `ord_plot_iris` for pca-ordered circular compositional barplots to pair with `ord_plot` output
* `ps_select` for easily selecting variables within phyloseq sample data
* `ord_plot` exposes scaling argument

# microViz 0.1.0

## Major renaming

* `calc_dist` --> `dist_calc`
* `beta_disper` --> `dist_bdisp`
* `ordin8` --> `ord_calc`
* `plot_ordin8` --> `ord_plot`
* `model_tax_corncob` --> `tax_model_corncob`

## Other breaking changes

* `plot_comp_bar` 
    * groups argument renamed to group_by for consistency with new facet_by argument
    * drop_unused_vars replaced with keep_all_vars which defaults to TRUE

## Features

* `ps_seriate` for ordering phyloseq samples by similarity


# microViz 0.0.6

## Features

* `ps_filter` allows filtering of `phyloseq` samples by values of variables in `sample_data`  (wrapper around `dplyr`'s `filter` function)

## Breaking changes

* `ps_mutate` no longer needs `.across` argument to use `dplyr::across()`

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

