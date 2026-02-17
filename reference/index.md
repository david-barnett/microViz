# Package index

## Subsetting or sorting phyloseq samples

Functions for filtering or reording samples in a phyloseq object. These
characteristically start with “ps\_”, which is short for phyloseq.

- [`ps_filter()`](https://david-barnett.github.io/microViz/reference/ps_filter.md)
  : Filter phyloseq samples by sample_data variables
- [`ps_arrange()`](https://david-barnett.github.io/microViz/reference/ps_arrange.md)
  : Arrange samples in phyloseq by sample_data variables or taxon
  abundance
- [`ps_seriate()`](https://david-barnett.github.io/microViz/reference/ps_seriate.md)
  : Arrange samples in a phyloseq by microbiome similarity
- [`ps_sort_ord()`](https://david-barnett.github.io/microViz/reference/ordination-sorting-samples.md)
  [`ord_order_samples()`](https://david-barnett.github.io/microViz/reference/ordination-sorting-samples.md)
  : Sort phyloseq samples by ordination axes scores
- [`ps_reorder()`](https://david-barnett.github.io/microViz/reference/ps_reorder.md)
  : Set order of samples in phyloseq object
- [`ps_dedupe()`](https://david-barnett.github.io/microViz/reference/ps_dedupe.md)
  : De-duplicate phyloseq samples
- [`ps_drop_incomplete()`](https://david-barnett.github.io/microViz/reference/ps_drop_incomplete.md)
  : Deselect phyloseq samples with sample_data missings

## Manipulating phyloseq sample_data

Functions for modifying and augmenting the sample data within phyloseq
objects. These also start with “ps\_”, which is short for phyloseq.

- [`ps_select()`](https://david-barnett.github.io/microViz/reference/ps_select.md)
  : Select phyloseq sample_data using dplyr::select syntax
- [`ps_mutate()`](https://david-barnett.github.io/microViz/reference/ps_mutate.md)
  : Modify or compute new sample_data in phyloseq object
- [`ps_join()`](https://david-barnett.github.io/microViz/reference/ps_join.md)
  : Join a dataframe to phyloseq sample data
- [`ps_otu2samdat()`](https://david-barnett.github.io/microViz/reference/ps_otu2samdat.md)
  : Copy phyloseq otu_table data to sample_data
- [`ps_melt()`](https://david-barnett.github.io/microViz/reference/ps_melt.md)
  : Melt phyloseq data object into large data.frame (tibble)

## Manipulating taxa

Functions for modifying the taxonomic information contained within
phyloseq objects. These characteristically start with “tax\_”, which is
short for taxa.

- [`tax_fix()`](https://david-barnett.github.io/microViz/reference/tax_fix.md)
  : Replace unknown, NA, or short tax_table values
- [`tax_fix_interactive()`](https://david-barnett.github.io/microViz/reference/tax_fix_interactive.md)
  : Shiny app to help you use tax_fix
- [`tax_prepend_ranks()`](https://david-barnett.github.io/microViz/reference/tax_prepend_ranks.md)
  : Add rank prefixes to phyloseq tax_table values
- [`tax_filter()`](https://david-barnett.github.io/microViz/reference/tax_filter.md)
  : Filter rare and/or low abundance taxa from a phyloseq object
- [`tax_agg()`](https://david-barnett.github.io/microViz/reference/tax_agg.md)
  : Aggregate taxa and track aggregation in psExtra
- [`tax_transform()`](https://david-barnett.github.io/microViz/reference/tax_transform.md)
  : Transform taxa in phyloseq object and record transformation
- [`tax_scale()`](https://david-barnett.github.io/microViz/reference/tax_scale.md)
  : Mean-center and SD-scale taxa in phyloseq
- [`tax_select()`](https://david-barnett.github.io/microViz/reference/tax_select.md)
  : Subset phyloseq object by (partial) taxa names
- [`tax_mutate()`](https://david-barnett.github.io/microViz/reference/tax_mutate.md)
  : Modify or compute new taxonomic ranks in phyloseq
- [`tax_rename()`](https://david-barnett.github.io/microViz/reference/tax_rename.md)
  : Make new phyloseq taxa names from classification and taxon abundance
  info
- [`tax_name()`](https://david-barnett.github.io/microViz/reference/tax_name.md)
  : Simple way to set unique taxa_names for phyloseq object
- [`tax_sort()`](https://david-barnett.github.io/microViz/reference/tax_sort.md)
  : Sort taxa in phyloseq otu_table and tax_table
- [`tax_sort_ord()`](https://david-barnett.github.io/microViz/reference/ordination-sorting-taxa.md)
  [`ord_order_taxa()`](https://david-barnett.github.io/microViz/reference/ordination-sorting-taxa.md)
  : Order taxa in phyloseq by their loading vectors
- [`tax_reorder()`](https://david-barnett.github.io/microViz/reference/tax_reorder.md)
  : Reorder taxa in phyloseq object using vector of names
- [`tax_top()`](https://david-barnett.github.io/microViz/reference/tax_top.md)
  : Get names of "top" n taxa
- [`tax_names2rank()`](https://david-barnett.github.io/microViz/reference/tax_names2rank.md)
  : Add taxa_names as last column in phyloseq tax_table

## Ordination and related methods

Functions for assessing and visualising overall microbiota composition
and its associations with phyloseq sample data variables. dist\_\*
functions calculate or use sample-sample distances/dissimilarities.
ord\_\* functions calculate or use sample ordinations.

- [`dist_calc()`](https://david-barnett.github.io/microViz/reference/dist_calc.md)
  : Calculate distances between pairs of samples in phyloseq object
- [`dist_permanova()`](https://david-barnett.github.io/microViz/reference/dist_permanova.md)
  : Calculate PERMANOVA after dist_calc()
- [`dist_bdisp()`](https://david-barnett.github.io/microViz/reference/dist_bdisp.md)
  : Wrapper for vegan::betadisper()
- [`ord_calc()`](https://david-barnett.github.io/microViz/reference/ord_calc.md)
  : Ordinate samples (arrange by similarity in multiple dimensions)
- [`ord_plot()`](https://david-barnett.github.io/microViz/reference/ord_plot.md)
  : Customisable ggplot2 ordination plot
- [`stat_chull()`](https://david-barnett.github.io/microViz/reference/stat_chull.md)
  : Draw convex hull for a set of points on a ggplot
- [`add_paths()`](https://david-barnett.github.io/microViz/reference/add_paths.md)
  : Add paths connecting points on a ggplot scatterplot
- [`tax_lab_style()`](https://david-barnett.github.io/microViz/reference/Ordination-labels.md)
  [`constraint_lab_style()`](https://david-barnett.github.io/microViz/reference/Ordination-labels.md)
  : Create list for ord_plot() \*\_lab_style arguments
- [`vec_constraint()`](https://david-barnett.github.io/microViz/reference/ord_arrows.md)
  [`vec_tax_sel()`](https://david-barnett.github.io/microViz/reference/ord_arrows.md)
  [`vec_tax_all()`](https://david-barnett.github.io/microViz/reference/ord_arrows.md)
  : Create ordination plot vector styling lists
- [`ord_plot_iris()`](https://david-barnett.github.io/microViz/reference/ord_plot_iris.md)
  : Circular compositional barplot sorted by ordination angle
- [`ord_explore()`](https://david-barnett.github.io/microViz/reference/ord_explore.md)
  : Interactively explore microbial compositions of ordinated samples
- [`scale_shape_girafe_filled()`](https://david-barnett.github.io/microViz/reference/scale_shape_girafe_filled.md)
  : Filled shapes for ggiraph interactive plots
- [`ps_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`dist_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`ord_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`info_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`perm_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`bdisp_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`tax_models_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`tax_stats_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`taxatree_models_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`taxatree_stats_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`otu_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`tt_get()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  [`samdat_tbl()`](https://david-barnett.github.io/microViz/reference/psExtra-accessors.md)
  : Extract elements from psExtra class

## Taxon models and tree visualisations

Functions for modelling and visualising associations between individual
microbes and phyloseq sample data variables.

- [`tax_model()`](https://david-barnett.github.io/microViz/reference/tax_model.md)
  : Statistical modelling for individual taxa in a phyloseq
- [`taxatree_models()`](https://david-barnett.github.io/microViz/reference/taxatree_models.md)
  : Statistical modelling for individual taxa across multiple ranks
- [`taxatree_models2stats()`](https://david-barnett.github.io/microViz/reference/models2stats.md)
  [`tax_models2stats()`](https://david-barnett.github.io/microViz/reference/models2stats.md)
  : Extract statistics from taxatree_models or tax_model output
- [`taxatree_stats_p_adjust()`](https://david-barnett.github.io/microViz/reference/taxatree_stats_p_adjust.md)
  : Adjust p values in taxatree_stats dataframe
- [`taxatree_plots()`](https://david-barnett.github.io/microViz/reference/taxatree_plots.md)
  : Plot statistical model results for all taxa on a taxonomic tree
- [`taxatree_plotkey()`](https://david-barnett.github.io/microViz/reference/taxatree_plotkey.md)
  : Draw labelled key to accompany taxatree_plots
- [`taxatree_label()`](https://david-barnett.github.io/microViz/reference/taxatree_label.md)
  : Add logical label column to taxatree_stats dataframe
- [`taxatree_plot_labels()`](https://david-barnett.github.io/microViz/reference/taxatree_plot_labels.md)
  : Add labels to taxatree plots/key
- [`taxatree_nodes()`](https://david-barnett.github.io/microViz/reference/taxatree_funs.md)
  [`taxatree_edges()`](https://david-barnett.github.io/microViz/reference/taxatree_funs.md)
  : Create node and edge dataframes for taxatree_plots
- [`cor_test()`](https://david-barnett.github.io/microViz/reference/cor_test.md)
  : Simple wrapper around cor.test for y ~ x style formula input

## Heatmap plotting functions

Functions for drawing and annotating heatmaps

- [`comp_heatmap()`](https://david-barnett.github.io/microViz/reference/comp_heatmap.md)
  : Draw heatmap of microbiome composition across samples
- [`cor_heatmap()`](https://david-barnett.github.io/microViz/reference/cor_heatmap.md)
  : Microbe-to-sample-data correlation heatmap
- [`taxAnnotation()`](https://david-barnett.github.io/microViz/reference/taxAnnotation.md)
  : Helper to specify a HeatmapAnnotation for taxa
- [`anno_tax_box()`](https://david-barnett.github.io/microViz/reference/anno_tax_box.md)
  : Helper to specify heatmap annotation for showing taxa abundance on
  boxplot
- [`anno_tax_prev()`](https://david-barnett.github.io/microViz/reference/anno_tax_prev.md)
  : Helper to specify heatmap annotation for showing taxa prevalence as
  barplot
- [`anno_tax_density()`](https://david-barnett.github.io/microViz/reference/anno_tax_density.md)
  : Helper to specify heatmap annotation for showing taxa abundance
  density plot
- [`sampleAnnotation()`](https://david-barnett.github.io/microViz/reference/sampleAnnotation.md)
  : Helper to specify a HeatmapAnnotation for samples in comp_heatmap
- [`anno_sample()`](https://david-barnett.github.io/microViz/reference/anno_sample.md)
  : Helper to specify simple comp_heatmap annotation for other sample
  data
- [`anno_sample_cat()`](https://david-barnett.github.io/microViz/reference/anno_sample_cat.md)
  : Helper to specify comp_heatmap annotation for categorical sample
  data
- [`anno_cat()`](https://david-barnett.github.io/microViz/reference/anno_cat.md)
  : Create colored rectangle annotations for categorical data
- [`anno_cat_legend()`](https://david-barnett.github.io/microViz/reference/anno_cat_legend.md)
  : Convenience function for generating a legend for anno_cat
  annotations.
- [`varAnnotation()`](https://david-barnett.github.io/microViz/reference/varAnnotation.md)
  : Helper to specify a HeatmapAnnotation for variables in cor_heatmap
- [`anno_var_box()`](https://david-barnett.github.io/microViz/reference/anno_var_box.md)
  : Helper to specify heatmap annotation for variable distribution
  boxplots
- [`anno_var_hist()`](https://david-barnett.github.io/microViz/reference/anno_var_hist.md)
  : Helper to specify heatmap annotation for variable distribution
  histograms
- [`anno_var_density()`](https://david-barnett.github.io/microViz/reference/anno_var_density.md)
  : Helper to specify heatmap annotation for variable distribution
  density plot
- [`heat_palette()`](https://david-barnett.github.io/microViz/reference/heat_palette.md)
  : Easy palettes for ComplexHeatmap
- [`heat_numbers()`](https://david-barnett.github.io/microViz/reference/heat_numbers.md)
  : Aesthetic settings for drawing numbers on heatmap tiles
- [`heat_grid()`](https://david-barnett.github.io/microViz/reference/heat_grid.md)
  : set options for drawing gridlines on heatmaps
- [`adjacent_side()`](https://david-barnett.github.io/microViz/reference/adjacent_side.md)
  : Simple heatmap helper to get a default adjacent side for another
  annotation

## Other functions

Other assorted functions

- [`comp_barplot()`](https://david-barnett.github.io/microViz/reference/comp_barplot.md)
  : Plot (grouped and ordered) compositional barplots
- [`distinct_palette()`](https://david-barnett.github.io/microViz/reference/distinct_palette.md)
  : Colour palettes suitable for 20+ categories
- [`tax_palette()`](https://david-barnett.github.io/microViz/reference/tax_palette.md)
  : Make a fixed taxa colour palette e.g. for comp_barplot
- [`tax_palette_plot()`](https://david-barnett.github.io/microViz/reference/tax_palette_plot.md)
  : tax_palette plotting helper function
- [`ps_calc_dominant()`](https://david-barnett.github.io/microViz/reference/ps_calc_dominant.md)
  : Calculate dominant taxon in each phyloseq sample
- [`ps_calc_diversity()`](https://david-barnett.github.io/microViz/reference/ps_calc_diversity.md)
  : Calculate diversity index and add to phyloseq sample data
- [`ps_calc_richness()`](https://david-barnett.github.io/microViz/reference/ps_calc_richness.md)
  : Calculate richness estimate and add to phyloseq sample data
- [`phyloseq_validate()`](https://david-barnett.github.io/microViz/reference/phyloseq_validate.md)
  : Check for (and fix) common problems with phyloseq objects
- [`prev()`](https://david-barnett.github.io/microViz/reference/prev.md)
  : Calculate prevalence from numeric vector

## Datasets

- [`shao19`](https://david-barnett.github.io/microViz/reference/shao19.md)
  : Gut microbiota relative abundance data from Shao et al. 2019
- [`ibd`](https://david-barnett.github.io/microViz/reference/ibd.md) :
  IBD study data in phyloseq object.

## Other

- [`microViz-package`](https://david-barnett.github.io/microViz/reference/microViz.md)
  [`microViz`](https://david-barnett.github.io/microViz/reference/microViz.md)
  : microViz: microbiome data analysis and visualization
- [`print(`*`<psExtraInfo>`*`)`](https://david-barnett.github.io/microViz/reference/psExtraInfo.md)
  : Print method for psExtraInfo object
- [`psExtra-class`](https://david-barnett.github.io/microViz/reference/psExtra-class.md)
  : Define psExtra class S4 object
- [`upgrade_ps_extra_to_psExtra()`](https://david-barnett.github.io/microViz/reference/upgrade_ps_extra_to_psExtra.md)
  : Convert old format "ps_extra" objects to new "psExtra" objects
- [`dist_calc_seq()`](https://david-barnett.github.io/microViz/reference/dist_calc_seq.md)
  : Calculate distances between sequential samples in ps_extra/phyloseq
  object
- [`tax_anno()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  [`anno_prev()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  [`anno_abund()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  [`var_anno()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  [`old_anno_var_hist()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  [`old_anno_var_box()`](https://david-barnett.github.io/microViz/reference/deprecated-heatmap-annotations.md)
  : DEPRECATED Heatmap annotations helpers
