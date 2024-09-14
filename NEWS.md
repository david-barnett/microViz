# microViz 0.12.5

- Fix: tax_fix no longer allows min_length of 0, as empty strings are (and were) always replaced
- Fix/Feat: comp_barplot and tax_sort checks/warnings about unavailable counts can be disabled with counts_warn = FALSE

# microViz 0.12.4

- Fix: no more cowplot legend extraction warnings in `ord_explore` 

# microViz 0.12.3

- Fix: allow hex code colours in `ord_plot`

# microViz 0.12.2

- Fix: `comp_heatmap` helper `sampleAnnotation` will now work with R 4.4

# microViz 0.12.1

- corncob (a suggested dependency) back on CRAN - so no longer installed from GitHub
- `ibd` phyloseq dataset added to microViz, as no longer included in corncob.

# microViz 0.12.0

- Breaking change: va-wunifrac distance calculation is no longer available, as it is not available in GUniFrac 1.8
- Note: If needed, `corncob` can now only be installed from GitHub, as is currently not available from CRAN. `corncob` is a suggested dependency of microViz from which data is borrowed for use in several examples, and which is needed is bbdml models are requested in tax_model.

# microViz 0.11.0

- `taxatree_models` now accepts transformation arguments to allow transformation after aggregation at each rank

# microViz 0.10.11

- Fix: `taxatree_stats_p_adjust` can accept multiple grouping variables without specifying new_var.

# microViz 0.10.10

- Fix: `comp_barplot` taxon ordering is now correct when tax_level is set to "Taxon"

# microViz 0.10.9

- Fix: Small internal fix to avoid renderggiraph deprecation warning in ord_explore.
- Fix: Internal fix to report NAs in OTU table more gracefully. (ps_counts issue #109)
- Fix: `tax_select()` tax_list argument can now actually take a list (Issue #100)

# microViz 0.10.8

- Fix: small internal fix for compatibility with next R version (relating to loss of stringsAsFactors global option)

# microViz 0.10.7

- Fix for dplyr version 1.1.0 warnings e.g. about across() wanting explicit .cols use

# microViz 0.10.6

- Fix shao19 example data: family rank in tax table now correctly shows families

# microViz 0.10.5

- Bug fix: manual direct labelling of `taxatree_plots` with `taxatree_labels` 
and `taxatree_plot_labels` works properly also with phyloseq object that lack a rooting rank (e.g. kingdom)
- Bug fix: `comp_barplot` bars are now in the correct order again when using tax_level = "unique"
- New Getting Started article

# microViz 0.10.4

- Bug fix: manual direct labelling of `taxatree_plots` with `taxatree_labels` 
and `taxatree_plot_labels` works properly (without losing stats info for any rank) 

# microViz 0.10.3

- `taxatree_plotkey` IMPORTANT BUG FIX: layout now matches taxatree_plots again
- Fixed broken ReadMe links

# microViz 0.10.2

## Features
- `ps_get` can return phyloseq with taxon counts when counts = TRUE and counts are available

## Bug fixes
- `taxatree_plots` and keys more robust #43 and no deprecation warnings #80
- `tax_transform` add argument accepts "halfmin" again #77
- `tax_top` can use_counts #78
- Compatibility with old R versions 3.6.* improved (explicit stringsAsFactors = FALSE)

# microViz 0.10.1

- Updates for compatibility with ggplot2 3.4.0
- Move DT, ggraph and tidygraph to suggested dependencies

# microViz 0.10.0

Major internal changes from S3 ps_extra to S4 psExtra objects. See [article about this change](../articles/ps_extra-replaced.html)

This version also introduces changes around `tax_model` and `taxatree_models` functionality.

## Breaking changes

- Using the `$` operator to access parts of the old "ps_extra" S3 class will no longer work and must be replaced with accessor functions, e.g. `ps_get`
- "ps_extra" objects generated with older versions will not be compatible with version 0.10.0 and can be converted to the new "psExtra" S4 objects with `upgrade_ps_extra_to_psExtra`
- `tax_model` and `taxatree_model` parallel processing with future and future.apply is off by default, and needs to be enabled with argument use_future = TRUE.
- `tax_model` attaches results to psExtra object by default, but you can restore old behaviour with return_psx = FALSE
- `comp_barplot` "other" taxa category renamed to "Other" by default

## Features

- `tax_model` and `taxatree_model` can now run multiple models per taxon, by accepting **lists** for formula or variables arguments. 
This allows running e.g. three simple regression models per taxon with predictors a, b, and c with variables = list("a", "b", "c")
- `cor_test` is a new simple wrapper around `stats::cor.test` with a y~x formula interface, allowing its use in `tax_model`

## Fixes

- No more deprecation warnings in `taxatree_plot_labels`


# microViz 0.9.7

## Features

-   `comp_barplot` gains x argument, to change default x aesthetic to something other than SAMPLE, 
which is useful for (faceted) plotting of repeated samples from the same site/individual 
over time or in different conditions when some samples are missing (see compositions tutorial article)
-   `ps_seriate` can now aggregate taxa before seriation (returns unaggregated still)

# microViz 0.9.6

## Features

-   Custom reordering of taxa on `comp_barplot` is now easier, just name the taxa you want to see first in the tax_order argument, and they will be brought to the front
-   `tax_reorder` is exported, and now accepts a subset of all available taxa names (powering the improvement in comp_barplot)

## Fixes

-   Bump rlang dependency version to 1.0.0 fixing <https://github.com/david-barnett/microViz/issues/69>

# microViz 0.9.5

## Fixes

-   `tax_rename` accepts integerish values for pad_digits argument

# microViz 0.9.4

## Fixes

-   `otu_get` correctly subsets samples or taxa if requested
-   heatmaps with one row or column can be plotted without explicitly suppressing seriation

# microViz 0.9.3

## Features

Robust clr and robust aitchison distance are now supported following their inclusion in vegan - `tax_transform` supports "rclr" (robust centered log ratio) - `dist_calc` supports "robust.aitchison" distance (as rclr & euclidean distance)

# microViz 0.9.2

## Features

-   `ps_dedupe` now allows keeping a specified number of samples per group, not just one, which could be useful for e.g. keeping the first 2 samples per participant or site.

-   shao19 dataset now included in microViz for use in the "Learn microbiome analysis with microViz" tutorial

## Fixes

-   `ps_dedupe` now deduplicates samples correctly when duplicates are specified with multiple variables, and provides a labelling-only mode to check duplicate identification result before removal.

# microViz 0.9.1

## Features

-   New function `tax_palette` allows easy creation of a fixed colour palette for taxa, to use with e.g. comp_barplot
-   New convenience functions `ps_calc_diversity` and `ps_calc_richness` to calculate a diversity or richness index (using microbiome package functions) and add to phyloseq sample data
-   New function `tax_rename` allows renaming of taxa using unique combination of classification at any chosen rank and abundance of that taxon (e.g. ASV145 becomes Bifidobacterium-002, which can be interpreted as the second most abundant Bifidobacterium ASV)
-   `tax_sort` can now sort taxa "asis" (i.e. do nothing), which now permits upfront custom sorting of taxa for use in `comp_barplot`
-   `tax_sort` can now transform taxa temporarily for sorting e.g. to sort by mean compositional (%) relative abundance

# microViz 0.9.0

## Heatmaps major changes

**`comp_heatmap` and `cor_heatmap` and their helpers are largely rewritten.** For guidance, see the new website article on heatmaps.

-   `taxAnnotation` and `varAnnotation` annotation helper functions replace the deprecated `tax_anno` and `var_anno` functions
-   `sampleAnnotation` is added for coordinating sample annotations on `comp_heatmap`
-   various `anno_*` helpers for each of the annotation coordinating functions above

# microViz 0.8.2

## Features

-   `taxatree_plots` can now show symbols indicating multiple levels of statistical significance
-   `taxatree_plots`: it is now easier to use other colour palettes with their default luminance

# microViz 0.8.1

## Features

-   `ord_explore` can perform "binary" transformations, unlocking interactive use of Binary Jaccard etc.
-   `tax_transform` gains `add` argument to simply add a constant value to all otu_table values before transformation (as an alternative to `zero_replace`)
-   `tax_scale` gains `keep_counts` argument for consistency with `tax_transform`

## Fixes

-   `tax_model` now only requires `corncob` to be installed when actually used
-   `ord_explore` barplot numerical inputs are now debounced to prevent lag from repeated redrawing

# microViz 0.8.0 - "autumn leaves"

## Breaking changes

### Trees

**The taxatree\_\* family of functions are largely rewritten.** For guidance, see the new website article on statistical modelling of taxa.

-   `taxatree_models` now attaches resulting list to ps_extra
-   `taxatree_models2stats` must be run on the output of `taxatree_models` before using `taxatree_plots`
-   `taxatree_plots` has different arguments and can now be directly labelled with `taxatree_plot_labels` when `taxatree_label` is run first to identify which taxa to label.
-   `taxatree_plotkey` has different arguments, with more flexible labelling conditions and a smarter label positioning approach.
-   `tax_model` and `taxatree_models` now use "lm" type by default, instead of "bbdml", as `corncob` is only a suggested dependency.

### Ordination

-   `ord_plot` default labels now have `alpha` = 1 for both taxa and constraints (previously 0.8)
-   `ord_plot` "auto"matic loading/constraint vector length scalar adjustment improvement: now uses both axes

### Barplots

-   `comp_barplot` now uses bray-curtis by default for sample ordering (instead of aitchison) as this generally looks better
-   `comp_barplot` now expects palette argument colours in first-to-last order, which is more intuitive than the previous reverse order
-   `distinct_palette` now adds "lightgrey" to end by default

### Other

-   `tax_filter`'s `is_counts` argument replaced by `use_counts`, allowing it to filter ps_extra objects using stored count data (i.e. after `tax_transform`).
-   `comp_heatmap` can no longer transform data internally, but accepts data already transformed with `tax_transform` and uses stored count data in the ps_extra for any taxa annotations
-   `tax_anno` heatmap annotation default style slightly changed.
-   `tax_names2rank` replaces deprecated `tax_names2tt`

## Features

-   `ord_plot` arrow labels can now be rotated with the help of `tax_lab_style()` and `constraint_lab_style()`
-   `ps_calc_dominant` function added, for conveniently identifying the dominant taxon in each phyloseq sample
-   `distinct_palette` gains "kelly" and "greenArmytage" palettes and helpfully adds "lightgrey" to the end by default for convenient use as the palette argument to `comp_barplot`
-   `tax_transform` can now chain multiple transformations together and records these transformations in the ps_extra output
-   `tax_mutate` function added, for modifying the tax_table rank variables with `dplyr` `mutate` syntax
-   `tax_sort` can now sort ps_extra objects
-   heatmap annotation helper `tax_anno` no longer requires 'column' or 'row' to be specified in advance
-   `prev`, a low level helper function for calculating taxon prevalence now exported
-   Various phyloseq accessor functions now work with ps_extra objects e.g. taxa_names, sample_names

## Fixes

-   `cor_heatmap` and `comp_heatmap` now respect column seriation arguments when different to row seriation
-   `comp_barplot` now actually orders grouped samples by similarity AFTER splitting into groups, as documented

# microViz 0.7.10

Updated citation information for JOSS publication. No other changes.

# microViz 0.7.9

Release accompanying JOSS manuscript acceptance.

-   Includes a fix (hopefully temporary) for incorrect barplot legend in ord_explore caused by bug introduced by ggplot2 version 3.3.4 noted at <https://github.com/tidyverse/ggplot2/issues/4511>

# microViz 0.7.8

-   `ord_plot` gains `vec_*` helper functions for generating lists for styling taxa and constraint vectors/arrows (`vec_constraint`, `vec_tax_all` and `vec_tax_sel`)

# microViz 0.7.7

-   `ord_explore` shapes selection bug fixed by limiting to 5 shapes returned by new `scale_shape_girafe_filled` function

# microViz 0.7.6

## Features

-   `stat_chulls` added, for drawing convex hulls on ord_plot and ord_explore ordinations
-   `add_paths` added, for drawing geom_paths connecting a subset of samples (over time) on an ordination plot

## Fixes

-   `phyloseq_validate` no longer checks "unique" tax_table rank column for NAs or nchar\<4 (avoiding warnings in ord_explore caused by short taxa_names)

# microViz 0.7.5

-   `ord_explore` can now draw stat_ellipse or taxa loading vectors
-   `tax_agg` error messages now include personalised suggested tax_fix code

# microViz 0.7.4

-   `ord_explore` Shiny app GUI can now also be used to interactively generate ordination plots, and to generate `ord_plot` code
-   `ord_plot` bug fix - can now plot any dimension
-   Removed deprecated `tax_fill_unknowns` function

# microViz 0.7.3

-   `phyloseq_validate` verbose = FALSE is actually silent now.

# microViz 0.7.2

-   `ord_explore` now compatible with Shiny version \>=1.5.0
-   `tax_fix` now sends messages about fixing completely anonymous rows, instead of warnings

# microViz 0.7.1

Allows `ps_seriate`, `ps_arrange`, `ps_reorder`, `ps_mutate`, and `ps_select` to work directly with `ps_extra` objects, as this can be helpful when quickly exploring / printing aggregated data, as in the new "Working with phyloseq objects" tutorial.

# microViz 0.7.0 - "fickle fixes"

## Breaking changes

-   `tax_fix` replaces the deprecated `tax_fill_unknowns`, `tax_fix` has all the same arguments except the 'levels' argument, which was removed

## Features

-   `tax_fix_interactive` Shiny app will help you clean up your taxonomy table with `tax_fix`

# microViz 0.6.1

## Breaking changes

-   `ord_plot_iris` and `ord_explore` no longer take ps argument of untransformed counts, because (by default) `tax_transform` now keeps the untransformed counts otu_table in the ps_extra object

## Features

-   `ord_explore` now allows much better control over selection of points (using `ggiraph` functionality)
-   `ord_plot` now has interactive option with `ggiraph` package
-   `ord_plot_iris` gains ord_plot argument, allowing a simple pairing of iris plot and ordination to be made more easily
-   `comp_barplot` (and by extension `ord_plot_iris`) can now be made interactive in a simple fashion, using ggiraph for hover/tooltip interaction with taxa

# microViz 0.6.0 - "open sesame"

This is the first public release version of microViz. It is still under active development, so pay attention to the following:

-   Minor version changes (e.g. 0.5.\* to 0.6.0) will signal that breaking changes have been made, i.e. installing the new version may break some previously working code. Breaking changes will be listed in this document.
-   Patch versions (e.g. 0.5.1 to 0.5.2) will be used to release new features and bug fixes, which should not break existing code. Please let me know if it does anyway!

## Breaking changes

-   `tax_agg` argument agg_level renamed to rank. `tax_agg` returns taxa in different order than before (and now different order from, but same aggregation as, `microbiome::aggregate_taxa()`). tax_agg now checks if taxa cannot be uniquely identified at the specified rank level. (now also about twice as fast)
-   `tax_fill_unknowns` x arg renamed to ps. Also now stops when unknown values are detected to the left of known values in the tax_table, as this should always be wrong/need fixing. Also, by default it now searches a larger list of probably unknown/uninformative tax_table values e.g. "k\_\_NA", "p\_\_Unknown" will now be replaced

## Features

-   `comp_barplot` gets merge_other argument, which, when FALSE, shows full diversity of samples by outlining individual taxa within the (grey) "other" category!
-   `tax_sort` for sorting taxa in tax_table and otu_table by several name or abundance options (deletes phy_tree if present!)
-   `tax_transform` can take a rank argument, to perform aggregation (internally using tax_agg) and transformation with one function, and record the results. **This is now the recommended usage!**
-   `tax_transform` new transformation = "binary" can convert to presence/absence data (used by `tax_sort` for by = "prev")
-   `tax_top` for flexibly returning top n taxa, at chosen rank, with ordering by `tax_sort`

# microViz 0.5.0 - "hot maps"

## Breaking changes

-   `cor_heatmap` and `comp_heatmap` argument changed: 'taxa_which' replaced with 'taxa_side' for easier control over where taxa annotations are placed (default behaviour stays the same)

## Features

-   Optionally annotate `cor_heatmap` with variable distributions using `var_anno` and its helpers: `anno_var_box` and `anno_var_hist`
-   `cor_heatmap` gets 'var_fun' argument for transforming variables before correlating
-   `phyloseq_validate` checks for zero taxa, which can happen after filtering samples
-   `tax_filter` gets undetected arg (greater than), as optional alternative to prev_detection_threshold (greater than or equal to)

## Bug fixes

-   heatmaps should handle NAs better (`viz_heatmap` internal function fix)
-   `heat_palette` can set range arg manually now without errors

## Other

-   minor versions from 0.5 will now get a memorable name, probably referring to features added since the last minor version

# microViz 0.4.3

## Features

-   `cor_heatmap` for microbe-metadata correlation heatmaps
-   `comp_heatmap` for visualising taxonomic composition across samples (ordered/clustered)
-   `ord_calc` can now guess which method the user wants by default (by checking for presence of distance matrix and constraints)
-   `ord_plot` auto_caption size can now be set, and it also now exposes the `coord_*` args: expand and clip
-   `ord_plot_iris` now handles multiple rings of anno_binary annotations, and anno_binary position is now closer when no anno_colour is set
-   `ord_explore` shiny app menu styling is a little cleaner (but still needs some love)
-   `tax_scale` for applying `base::scale()` to phyloseq otu_table
-   `tax_name` for easily setting informative unique phyloseq taxa_names

## Bug fixes

-   `dist_permanova`'s obsolete return arg removed
-   `tax_filter` gets explicit handling of compositional input
-   `tax_fill_unknowns` gets better handling of fully unclassified taxa
-   `taxatree_nodes` now checks for cross-rank name duplications, which would mess up the taxatree graph structure

# microViz 0.4.2

## Bug fixes

-   `ord_explore` now works with ps_extra classes
-   `tax_fill_unknowns` gives informative error on data with only one taxonomic rank
-   `tax_filter` ps argument default removed

# microViz 0.4.1

## Bug fixes:

-   `ord_calc` now correctly returns subsetted phyloseq in ps_extra obj after constrained ordination
-   `taxatree_plots` and `taxatree_plotkey` now have reactive automatic **minimum** node and edge sizes that depend on the set maxes

# microViz 0.4.0

## Breaking changes

-   `dist_permanova` replaces `permanova` for naming consistency and guiding user
-   `comp_barplot` replaces `plot_comp_bar` in anticipation of (a) future heatmap function(s) named comp_heatmap or similar

## Features

New **"ps_extra"** class (S3) conveniently stores phyloseq object alongside any calculated distance matrix, ordination and permanova models, as well as records of `tax_agg` and `tax_transform` calls. "ps_extra" class objects have a pretty and compact print method, a simple list structure, and convenient accessor functions to return each component: `ps_get`, `info_get`, `dist_get`, `ord_get`, `perm_get`, `bdisp_get`.

# microViz 0.3.2

## Features

-   Taxon modelling updates:
    -   `tax_model` and `taxatree_models` can handle linear modeling e.g. on compositional (TS-Scaled) data
    -   `taxatree_plots` has more sensible defaults (automatic symmetrical colour limits and variable selection based on model type)

# microViz 0.3.1

## Breaking changes

-   `ord_plot_iris` the `data` arg is replaced with `ord` and conditionally optional `ps` arg for when data in `ord` have been transformed
-   `permanova` always uses adonis2 now, so that arg is **removed**, and replaced with `by` argument to set sums of squares choice

## Features

-   `ord_plot` gets a `center` argument to center expand the plot limits to center around zero (useful when pairing with `ord_plot_iris`)
-   `ord_explore` can now also display ordinations that don't use distances like PCA and RDA (as well as PCoA of course)
-   `ord_explore` gains a `ps` arg (for untransformed version) and other tweaks to facilitate using transformed data in `ord`

## Other fixes

-   `ord_plot_iris` annotation args are now NULL by default.
-   `microbiome::aggregate_top_taxa` is copied directly into `microViz` to avoid its coming deprecation and the current warning.

# microViz 0.2.0

## Features

-   `taxatree_models` and `taxatree_plots` bring tree-based visualisations of statistical models

## Breaking changes

-   `tax_model` replaces `tax_model_corncob` as a function that will eventually be generalised to use other model types
-   `taxatree_plots` and `taxatree_plotkey` replaces the rudimentary `taxatree_plot`

# microViz 0.1.2

## Features

-   `ord_explore` allows interactive exploration of the composition of ordinated samples using `Shiny`

# microViz 0.1.1

## Features

-   `ord_plot_iris` for pca-ordered circular compositional barplots to pair with `ord_plot` output
-   `ps_select` for easily selecting variables within phyloseq sample data
-   `ord_plot` exposes scaling argument

# microViz 0.1.0

## Major renaming

-   `calc_dist` --\> `dist_calc`
-   `beta_disper` --\> `dist_bdisp`
-   `ordin8` --\> `ord_calc`
-   `plot_ordin8` --\> `ord_plot`
-   `model_tax_corncob` --\> `tax_model_corncob`

## Other breaking changes

-   `plot_comp_bar`
    -   groups argument renamed to group_by for consistency with new facet_by argument
    -   drop_unused_vars replaced with keep_all_vars which defaults to TRUE

## Features

-   `ps_seriate` for ordering phyloseq samples by similarity

# microViz 0.0.6

## Features

-   `ps_filter` allows filtering of `phyloseq` samples by values of variables in `sample_data` (wrapper around `dplyr`'s `filter` function)

## Breaking changes

-   `ps_mutate` no longer needs `.across` argument to use `dplyr::across()`

# microViz 0.0.5

## Features

-   `ps_dedupe` bug fix method = "readcount" now correctly keeps all samples that "first" or "last" methods do

# microViz 0.0.4

## Features

-   `ps_arrange` allows reordering of `phyloseq` samples by values of variables in `sample_data` or `otu_table` (wrapper around `dplyr`'s `arrange` function)
-   `ps_otu2samdat` add taxon abundance variables from `phyloseq` `otu_table` to `sample_data` for use in plotting etc

# microViz 0.0.3

## Features

-   `ps_mutate` allowing easy and piped modification of `phyloseq` `sample_data` (wrapper around `dplyr`'s `mutate` function)
-   `ps_join` allows easy and piped addition of a dataframe of data to `phyloseq` `sample_data` (wrapper around `dplyr`'s `*_join` functions)

# microViz 0.0.2

## Breaking changes

-   `plot_ordin8` arguments changed to allow easier sizing and styling of all vectors and labels
-   `plot_ordin8` default styling of taxon and constraint vectors and labels is changed: background vectors are now semi-transparent and dashed lines are not used any more by default (but can be set)

## Other

-   `ordin8` and `plot_ordin8` get basic support for CCA and NMDS methods finally
-   `plot_comp_bar` gets `order_samples_with_all_taxa` and `tax_transform_for_ordering`
-   Documentation of manual sample ordering across `plot_comp_bar` groups added to Visualising Compositions article on website.
-   Documentation of experimental polar coordinates and PCA_angle sorting added in new article called PCA-sorted polar composition plots, on website.

# microViz 0.0.1

## Main changes

-   `phyloseq_validate` now fixes otu_tables stored as integers and messages user about suspicious or NA tax_table entries and this happens as part of `plot_comp_bar` and `plot_ordin8`

-   `plot_comp_bar` and `plot_ordin8` gain `taxon_renamer` argument to allow you to customise the taxon names on these plots

-   `plot_comp_bar` can now handle missing values in the grouping variable by converting NAs to "NA"s

-   Some functions renamed for naming consistency:

    -   `prepend_ranks` -\> `tax_prepend_ranks`
    -   `tax_model_corncob` -\> `model_tax_corncob`
    -   `corncob_models_to_var_stats` -\> `modelsmodels2stats_corncob`
    -   `tax_tree_` `nodes`/`edges`/`plot` -\> `taxatree_` ...

## Other

-   Added a `NEWS.md` file to track changes to the package.
