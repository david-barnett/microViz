#' Plot (grouped and ordered) compositional barplots
#'
#' Stacked barplots showing composition of phyloseq samples for a specified number of coloured taxa. `plot_comp_bar` performs the compositional transformation for you, so your phyloseq object should contain counts!
#' - sample_order: Either specify a list of sample names to order manually, or the bars/samples can/will be sorted by similarity, according to a specified distance measure (default aitchison),
#' - seriate_method specifies a seriation/ordering algorithm (default Ward hierarchical clustering with optimal leaf ordering, see seriation::list_seriation_methods())
#' - group_by: You can group the samples on distinct plots by levels of a variable in the phyloseq object. The list of ggplots produced can be arranged flexibly with the patchwork package functions. If you want to group by several variables you can create an interaction variable with interaction(var1, var2) in the phyloseq sample_data BEFORE using plot_comp_bar.
#' - facet_by can allow faceting of your plot(s) by a grouping variable. Using this approach is less flexible than using group_by but means you don't have to arrange a list of plots yourself like with the group_by argument. Using facet_by is equivalent to adding a call to facet_wrap(facets = facet_by, scales = "free") to your plot(s). Calling facet_wrap() yourself is itself a more flexible option as you can add other arguments like the number of rows etc.
#' - bar_width: No gaps between bars, unless you want them (decrease width argument to add gaps between bars).
#' - bar_outline_colour: Bar outlines default to "black". Set to NA if you don't want outlines.
#' - palette: Default colouring is consistent across multiple plots if created with the group_by argument, and the defaults scheme retains the colouring of the most abundant taxa irrespective of n_taxa
#'
#' @param ps phyloseq object
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param n_taxa how many taxa to colour show distinct colours for (all other taxa grouped into "Other").
#' @param tax_order order of taxa within the bars, currently only "abundance" works, which puts the most abundant taxa at the bottom (or left).
#' @param taxon_renamer function that takes taxon names and returns modified names for legend
#' @param palette palette for taxa fill colours
#' @param sample_order vector of sample names, any distance measure in calc_dist that does not require a phylogenetic tree, or "default" for the order returned by phyloseq::sample_names(ps)
#' @param order_with_all_taxa if TRUE, this will use all taxa (not just the top n_taxa) to calculate distances for sample ordering
#' @param tax_transform_for_ordering transformation of taxa values used before ordering samples by similarity
#' @param label could also consider arbitrary annotation with extra info, like in complex heatmap
#' @param group_by splits dataset by this variable (must be categorical) - resulting in a list of plots, one for each level of the group_by variable.
#' @param facet_by facets plots by this variable (must be categorical) - if group_by is also set, the faceting with occur separately in the plot for each group.
#' @param bar_width default 1 avoids random gapping otherwise seen with many samples (set to something less than 1 to introduce gaps between fewer samples)
#' @param bar_outline_colour line colour separating taxa and samples (use NA for none)
#' @param drop_unused_vars speeds up ps_melt but might limit future plot customisation options
#' @param seriate_method name of any ordering method suitable for distance matrices (see ?seriation::seriate)
#'
#' @return ggplot or list of harmonised ggplots
#' @export
#'
#' @examples
#' library(microbiome)
#' data(dietswap)
#'
#' # illustrative simple customised example
#' dietswap %>%
#'   ps_filter(timepoint == 1) %>%
#'   plot_comp_bar(
#'     tax_level = "Family", n_taxa = 8,
#'     bar_outline_colour = NA,
#'     sample_order = "bray",
#'     bar_width = 0.7,
#'     taxon_renamer = toupper
#'   ) + coord_flip()
#'
#' # Order samples by the value of one of more sample_data variables.
#' # Use ps_arrange and set sample_order = "default" in plot_comp_bar.
#' # ps_mutate is also used here to create an informative variable for axis labelling
#' dietswap %>%
#'   ps_mutate(subject_timepoint = interaction(subject, timepoint)) %>%
#'   ps_filter(nationality == "AAM", group == "DI", sex == "female") %>%
#'   ps_arrange(desc(subject), desc(timepoint)) %>%
#'   plot_comp_bar(
#'     tax_level = "Genus", n_taxa = 12,
#'     bar_outline_colour = NA,
#'     sample_order = "default",
#'     bar_width = 0.7,
#'     label = "subject_timepoint"
#'   ) + coord_flip()
#'
#'
#' # Often to compare groups, average compositions are presented
#' p1 <- phyloseq::merge_samples(dietswap, group = "group") %>%
#'   plot_comp_bar(
#'     tax_level = "Genus", n_taxa = 12,
#'     sample_order = c("ED", "HE", "DI"),
#'     bar_width = 0.8
#'   ) +
#'   coord_flip() + labs(x = NULL, y = NULL)
#' p1
#'
#' # However that "group-averaging" approach hides a lot of within-group variation
#' p2 <- plot_comp_bar(dietswap,
#'   tax_level = "Genus", n_taxa = 12, group_by = "group",
#'   sample_order = "euclidean", bar_outline_colour = NA
#' ) %>%
#'   patchwork::wrap_plots(nrow = 3, guides = "collect") &
#'   coord_flip() & labs(x = NULL, y = NULL) &
#'   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
#' p2
#'
#' # Only from p2 you can see that the apparently higher average relative abundance
#' # of Oscillospira in group DI is probably driven largely by a subgroup
#' # of DI samples with relatively high Oscillospira.
#'
#' # make a list of 2 harmonised composition plots (grouped by sex)
#' p <- plot_comp_bar(dietswap,
#'   n_taxa = 15, tax_level = "Genus",
#'   bar_outline_colour = "black",
#'   sample_order = "aitchison", group_by = "sex"
#' )
#'
#' # plot them side by side with patchwork package
#' patch <- patchwork::wrap_plots(p, ncol = 2, guides = "collect")
#' patch & coord_flip() # make bars in all plots horizontal (note: use & instead of +)
#'
#' # beautifying tweak #
#' # modify one plot in place (flip the order of the samples in the 2nd plot)
#' # notice that the scaling is for the x-axis
#' # (that's because coord_flip is used afterwards when displaying the plots
#' patch[[2]] <- patch[[2]] + scale_x_discrete(limits = rev)
#' # Explainer: rev() function takes current limits and reverses them.
#' # You could also pass a completely arbitrary order, naming all samples
#'
#' # you can theme all plots with the & operator
#' patch & coord_flip() &
#'   theme(axis.text.y = element_text(size = 5), legend.text = element_text(size = 6))
#' # See https://patchwork.data-imaginist.com/index.html
plot_comp_bar <- function(
                          ps,
                          tax_level,
                          n_taxa = 8,
                          tax_order = "abundance",
                          taxon_renamer = function(x) identity(x),
                          palette = c("lightgrey", rev(distinct_palette(n_taxa))),
                          sample_order = "aitchison",
                          order_with_all_taxa = FALSE,
                          tax_transform_for_ordering = "identity",
                          label = "SAMPLE",
                          group_by = NA,
                          facet_by = NA,
                          bar_width = 1,
                          bar_outline_colour = "black",
                          drop_unused_vars = TRUE,
                          seriate_method = "OLO_ward") {

  # check phyloseq for common problems (and fix or message about this)
  ps <- phyloseq_validate(ps, verbose = TRUE)

  # save full unfiltered phyloseq for ordering
  ps_original <- ps

  # how many taxa to plot (otherwise group into other)
  ps <- microbiome::aggregate_top_taxa(ps, top = n_taxa, level = tax_level)

  # set taxa order
  if (tax_order == "abundance") {
    ordered_taxa <- rev(c(setdiff(microbiome::top_taxa(ps), "Other"), "Other"))
  } else {
    # e.g. should allow external taxa ordering from calling top_taxa on a larger superset phyloseq
    ordered_taxa <- tax_order
  }
  # fix taxa colour scheme
  if (isTRUE(is.null(names(palette)))) {
    names(palette) <- ordered_taxa
  }
  # determine sample ordering option
  samples_ordered_by_similarity <- FALSE # default (may be overwritten with true)
  if (identical(sample_order, "default")) {
    ordered_samples <- phyloseq::sample_names(ps)
  } else if (length(sample_order) == 1) {
    samples_ordered_by_similarity <- TRUE
  } else if (length(sample_order) > 1) {
    ordered_samples <- sample_order
  }

  # create a sample names variable if this will be used for labelling
  if (identical(label, "SAMPLE")) {
    phyloseq::sample_data(ps)$SAMPLE <- phyloseq::sample_names(ps)
  }

  # establish a labelling function (for the samples)
  meta <- data.frame(phyloseq::sample_data(ps))
  LABELLER <- function(SAMPLES) {
    lapply(SAMPLES, function(SAMPLE) {
      meta[rownames(meta) == SAMPLE, label]
    })
  }

  # drop unused variables
  if (identical(drop_unused_vars, TRUE)) {
    kept_vars <- label
    if (!identical(group_by, NA)) {
      kept_vars <- c(union(kept_vars, group_by))
    }
    if (!identical(facet_by, NA)) {
      kept_vars <- c(union(kept_vars, facet_by))
    }
    if (!samples_ordered_by_similarity && length(sample_order) == 1 && sample_order != "default") {
      kept_vars <- c(union(kept_vars, sample_order))
    }
    phyloseq::sample_data(ps) <- data.frame(phyloseq::sample_data(ps))[, kept_vars, drop = FALSE]
  }

  # define function to actually create the plot/plots
  plot_function <- function(ps) {

    # sample ordering
    if (samples_ordered_by_similarity) {
      if (isTRUE(order_with_all_taxa)){
        ps_ordered <- ps_original
      } else {
        ps_ordered <- ps
      }
      ps_ordered <- ps_seriate(
        ps_ordered, method = seriate_method, dist = sample_order, tax_transform = tax_transform_for_ordering
      )
      ordered_samples <- phyloseq::sample_names(ps_ordered)
    }
    # create long dataframe from compositional phyloseq
    # TODO consider replacing psmelt for speed: e.g. https://chuckpr.github.io/posts/melt/
    df <- ps %>%
      microbiome::transform(transform = "compositional") %>%
      phyloseq::psmelt()

    # set abundance order of taxa at chosen rank level
    df[[tax_level]] <- factor(df[[tax_level]], levels = ordered_taxa, ordered = TRUE)

    # set sample order
    df[["SAMPLE"]] <- factor(df[["Sample"]], levels = ordered_samples, ordered = TRUE)

    # build plot
    p <- ggplot2::ggplot(df, ggplot2::aes_string(x = "SAMPLE", y = "Abundance", fill = tax_level))

    if (is.na(bar_outline_colour)) {
      p <- p + ggplot2::geom_bar(position = "stack", stat = "identity", width = bar_width)
    } else {
      p <- p + ggplot2::geom_bar(position = "stack", stat = "identity", width = bar_width, colour = bar_outline_colour)
    }

    p <- p +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_discrete(
        breaks = ordered_samples,
        labels = LABELLER
      ) +
      ggplot2::scale_fill_manual(
        values = palette,
        labels = taxon_renamer,
        guide = ggplot2::guide_legend(reverse = TRUE)
      )

    if (!identical(facet_by, NA)) {
      p <- p + ggplot2::facet_wrap(facets = facet_by, scales = "free")
    }

    p
  }

  # group_by / splitting entire phyloseq by the group_by variable
  group_by_var <- phyloseq::sample_data(ps)[[group_by]]
  if (anyNA(group_by_var)) {
    message("Warning: replacing NAs with 'NA's in group_by variable: ", group_by)
    phyloseq::sample_data(ps)[[group_by]][is.na(group_by_var)] <- "NA"
  }
  LEVELS <- unique(phyloseq::sample_data(ps)[[group_by]])

  if (isTRUE(is.null(LEVELS))) {
    plot_function(ps)
  } else {
    plots_list <- lapply(LEVELS, function(level) {
      this_group <- phyloseq::sample_data(ps)[[group_by]] == level
      ps <- phyloseq::prune_samples(samples = this_group, x = ps)
      plot_function(ps)
    })
    # set y axis title to group level
    plots_list <- lapply(seq_along(LEVELS), function(level) {
      plots_list[[level]] + ggplot2::labs(title = LEVELS[[level]])
    })
    names(plots_list) <- LEVELS
    return(plots_list)
  }
}
