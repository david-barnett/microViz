#' Plot (grouped and ordered) compositional barplots
#'
#' @description
#' Stacked barplots showing composition of phyloseq samples for a specified number of coloured taxa.
#' Normally your phyloseq object should contain count data, as by default `comp_barplot()` performs the "compositional" taxa transformation for you, and requires count input for some sample_order methods!
#'
#' @details
#' - sample_order: Either specify a list of sample names to order manually, or the bars/samples can/will be sorted by similarity, according to a specified distance measure (default aitchison),
#' - seriate_method specifies a seriation/ordering algorithm (default Ward hierarchical clustering with optimal leaf ordering, see seriation::list_seriation_methods())
#' - group_by: You can group the samples on distinct plots by levels of a variable in the phyloseq object. The list of ggplots produced can be arranged flexibly with the patchwork package functions. If you want to group by several variables you can create an interaction variable with interaction(var1, var2) in the phyloseq sample_data BEFORE using comp_barplot.
#' - facet_by can allow faceting of your plot(s) by a grouping variable. Using this approach is less flexible than using group_by but means you don't have to arrange a list of plots yourself like with the group_by argument. Using facet_by is equivalent to adding a call to facet_wrap(facets = facet_by, scales = "free") to your plot(s). Calling facet_wrap() yourself is itself a more flexible option as you can add other arguments like the number of rows etc. However you must use keep_all_vars = TRUE if you will add faceting manually.
#' - bar_width: No gaps between bars, unless you want them (decrease width argument to add gaps between bars).
#' - bar_outline_colour: Bar outlines default to "grey5" for almost black outlines. Use NA if you don't want outlines.
#' - merge_other: controls whether bar outlines can be drawn around individual (lower abundance) taxa that are grouped in "other" category. If you want to see the diversity of taxa in "other" use merge_taxa = FALSE, or use TRUE if you prefer the cleaner merged look
#' - palette: Default colouring is consistent across multiple plots if created with the group_by argument, and the defaults scheme retains the colouring of the most abundant taxa irrespective of n_taxa
#'
#' @details
#'
#' @param ps phyloseq object
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param n_taxa
#' how many taxa to show distinct colours for (all others grouped into "Other")
#' @param tax_order
#' order of taxa within the bars, either a function for tax_sort (e.g. sum),
#' or a vector of (all) taxa names at tax_level to set order manually
#' @param merge_other
#' if FALSE, taxa coloured/filled as "other" remain distinct,
#'  and so can have bar outlines drawn around them
#' @param taxon_renamer
#' function that takes taxon names and returns modified names for legend
#' @param sample_order
#' vector of sample names;
#' or any distance measure in dist_calc that doesn't require phylogenetic tree;
#' or "default" for the order returned by phyloseq::sample_names(ps)
#' @param order_with_all_taxa
#' if TRUE, this will always use all taxa (not just the top n_taxa)
#' to calculate distances for sample ordering
#' @param label sample label variable name
#' @param group_by splits dataset by this variable (must be categorical)
#' - resulting in a list of plots, one for each level of the group_by variable.
#' @param facet_by
#' facets plots by this variable (must be categorical). If group_by is also set
#' the faceting will occur separately in the plot for each group.
#' @param bar_width
#' default 1 avoids random gapping otherwise seen with many samples
#' (set to something less than 1 to introduce gaps between fewer samples)
#' @param bar_outline_colour line colour separating taxa and samples
#' (use NA for none)
#' @param bar_outline_width width of line separating taxa and samples
#' (for no outlines set bar_outline_colour = NA)
#' @param palette palette for taxa fill colours
#' @param tax_transform_for_ordering
#' transformation of taxa values used before ordering samples by similarity
#' @param tax_transform_for_plot
#' default "compositional" draws proportions of total counts per sample,
#' but you could reasonably use another transformation,
#' e.g. "identity", if you have truly quantitative microbiome profiling data
#' @param seriate_method
#' name of any ordering method suitable for distance matrices
#' (see ?seriation::seriate)
#' @param keep_all_vars
#' FALSE may speed up internal melting with ps_melt for large phyloseq objects
#' but TRUE is required for some post-hoc plot customisation
#' @param interactive creates plot suitable for use with ggiraph
#' @param max_taxa maximum distinct taxa groups to show
#' (only really useful for limiting complexity of interactive plots
#' e.g. within ord_explore)
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
#'   comp_barplot(
#'     tax_level = "Family", n_taxa = 8,
#'     bar_outline_colour = NA,
#'     sample_order = "bray",
#'     bar_width = 0.7,
#'     taxon_renamer = toupper
#'   ) + coord_flip()
#'
#' # Order samples by the value of one of more sample_data variables.
#' # Use ps_arrange and set sample_order = "default" in comp_barplot.
#' # ps_mutate is also used here to create an informative variable for axis labelling
#' dietswap %>%
#'   ps_mutate(subject_timepoint = interaction(subject, timepoint)) %>%
#'   ps_filter(nationality == "AAM", group == "DI", sex == "female") %>%
#'   ps_arrange(desc(subject), desc(timepoint)) %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 12,
#'     sample_order = "default",
#'     bar_width = 0.7,
#'     bar_outline_colour = "black",
#'     order_with_all_taxa = TRUE,
#'     label = "subject_timepoint"
#'   ) + coord_flip()
#'
#' # how many taxa are in those light grey "other" bars?
#' # set merge_other, to find out (& remember to set a bar_outline_colour)
#' dietswap %>%
#'   ps_mutate(subject_timepoint = interaction(subject, timepoint)) %>%
#'   ps_filter(nationality == "AAM", group == "DI", sex == "female") %>%
#'   ps_arrange(desc(subject), desc(timepoint)) %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 12,
#'     sample_order = "default",
#'     merge_other = FALSE,
#'     bar_width = 0.7,
#'     bar_outline_colour = "black",
#'     order_with_all_taxa = TRUE,
#'     label = "subject_timepoint"
#'   ) + coord_flip()
#'
#'
#' # Often to compare groups, average compositions are presented
#' p1 <- phyloseq::merge_samples(dietswap, group = "group") %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 12,
#'     sample_order = c("ED", "HE", "DI"),
#'     bar_width = 0.8
#'   ) +
#'   coord_flip() + labs(x = NULL, y = NULL)
#' p1
#'
#' # However that "group-averaging" approach hides a lot of within-group variation
#' p2 <- comp_barplot(dietswap,
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
#' p <- comp_barplot(dietswap,
#'   n_taxa = 15, tax_level = "Genus",
#'   bar_outline_colour = "black", merge_other = TRUE,
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
comp_barplot <- function(
                         ps,
                         tax_level,
                         n_taxa = 8,
                         tax_order = sum,
                         merge_other = TRUE,
                         taxon_renamer = function(x) identity(x),
                         sample_order = "aitchison",
                         order_with_all_taxa = FALSE,
                         label = "SAMPLE",
                         group_by = NA,
                         facet_by = NA,
                         bar_width = 1,
                         bar_outline_colour = "grey5",
                         bar_outline_width = 0.1,
                         palette =
                           c("lightgrey", rev(distinct_palette(n_taxa))),
                         tax_transform_for_ordering = "identity",
                         tax_transform_for_plot = "compositional",
                         seriate_method = "OLO_ward",
                         keep_all_vars = TRUE,
                         interactive = FALSE,
                         max_taxa = 10000) {
  stopifnot(max_taxa > n_taxa)

  # check phyloseq for common problems (and fix or message about this)
  ps <- phyloseq_validate(ps, remove_undetected = FALSE, verbose = TRUE)

  # how many taxa to plot (otherwise group into other)
  ps <- tax_agg(ps, rank = tax_level)[["ps"]]

  # calculate tax_order if rule given or external ordering vec given
  if (inherits(tax_order, "function") || identical(tax_order, "name")) {
    ps <- tax_sort(ps, by = tax_order)
  } else if (length(tax_order) == phyloseq::ntaxa(ps)) {
    ps <- tax_reorder(ps, tax_order = tax_order, tree_warn = TRUE)
  }

  # save full (but rank-aggregated) phyloseq for ordering samples
  if (isTRUE(order_with_all_taxa)) ps_for_order <- ps

  # create "top" rank
  phyloseq::tax_table(ps) <-
    tt_add_topN_var(phyloseq::tax_table(ps), N = n_taxa, other = "other")

  # merge "top" rank's "other" category into one taxon,
  # to allow drawing bar outlines, but not within "other"
  if (isTRUE(merge_other)) {
    ps <- tax_agg(ps, rank = "top", force = TRUE)[["ps"]]
  }
  # aggregate the phyloseq at a high maximum number of displayable taxa
  phyloseq::tax_table(ps) <-
    tt_add_topN_var(
      phyloseq::tax_table(ps),
      N = max_taxa,
      other = "other", varname = "separate"
    )
  ps <- tax_agg(ps, rank = "separate", force = TRUE)[["ps"]]

  if (isFALSE(order_with_all_taxa)) ps_for_order <- ps

  # set taxa order
  ordered_taxa <- rev(unique(unclass(phyloseq::tax_table(ps))[, "unique"]))

  # fix taxa colour scheme (colours only applied to top taxa)
  ordered_top_taxa <- rev(unique(unclass(phyloseq::tax_table(ps))[, "top"]))
  if (identical(names(palette), NULL)) names(palette) <- ordered_top_taxa

  # determine sample ordering option
  # set default (which may then be overwritten with TRUE)
  samples_ordered_by_similarity <- FALSE
  if (identical(sample_order, "default")) {
    ordered_samples <- phyloseq::sample_names(ps)
  } else if (length(sample_order) == 1) {
    samples_ordered_by_similarity <- TRUE
  } else if (length(sample_order) > 1) {
    ordered_samples <- sample_order
  }

  # create a sample names variable if this will be used for labelling
  if (identical(label, "SAMPLE")) {
    phyloseq::sample_data(ps)[["SAMPLE"]] <- phyloseq::sample_names(ps)
  }

  # establish a labelling function (for the samples)
  meta <- data.frame(phyloseq::sample_data(ps))
  LABELLER <- function(SAMPLES) {
    lapply(SAMPLES, function(SAMPLE) {
      meta[rownames(meta) == SAMPLE, label]
    })
  }

  # drop unused variables if requested
  if (identical(keep_all_vars, FALSE)) {
    kept_vars <- label
    if (!identical(group_by, NA)) {
      kept_vars <- c(union(kept_vars, group_by))
    }
    if (!identical(facet_by, NA)) {
      kept_vars <- c(union(kept_vars, facet_by))
    }
    if (
      !samples_ordered_by_similarity &&
        length(sample_order) == 1 &&
        !identical(sample_order, "default")
    ) {
      kept_vars <- c(union(kept_vars, sample_order))
    }
    phyloseq::sample_data(ps) <- data.frame(
      phyloseq::sample_data(ps),
      check.names = FALSE
    )[, kept_vars, drop = FALSE]
  }

  # define function to actually create the plot/plots
  barplot_function <- function(ps) {

    # sample ordering
    if (samples_ordered_by_similarity) {
      ps_ordered <- ps_seriate(
        ps_for_order, # ps_ordered,
        method = seriate_method, dist = sample_order,
        tax_transform = tax_transform_for_ordering
      )
      ordered_samples <- phyloseq::sample_names(ps_ordered)
    }
    ps <- tax_transform(ps, transformation = tax_transform_for_plot)[["ps"]]
    # create long dataframe from compositional phyloseq
    df <- ps_melt(ps)

    # set fixed order of stacked taxa bars by creating ordered factor
    df[["unique"]] <-
      factor(df[["unique"]], levels = ordered_taxa, ordered = TRUE)
    # set fixed order of fill colours (for LEGEND ordering!)
    df[["top"]] <-
      factor(df[["top"]], levels = ordered_top_taxa, ordered = TRUE)

    # set sample order
    df[["SAMPLE"]] <-
      factor(df[["Sample"]], levels = ordered_samples, ordered = TRUE)

    # build plot
    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes_string(
        x = "SAMPLE", y = "Abundance", fill = "top", group = "unique"
      )
    ) +
      ggplot2::xlab(NULL)

    if (isTRUE(interactive)) {
      p <- p + ggiraph::geom_col_interactive(
        position = "stack", width = bar_width,
        colour = bar_outline_colour, size = bar_outline_width,
        ggplot2::aes_string(
          data_id = "unique",
          tooltip = "unique"
        )
      )
    } else {
      p <- p + ggplot2::geom_col(
        position = "stack", width = bar_width,
        colour = bar_outline_colour, size = bar_outline_width
      )
    }
    # theme plot
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
        guide = ggplot2::guide_legend(title = tax_level, reverse = TRUE)
      )

    if (!identical(facet_by, NA)) {
      p <- p + ggplot2::facet_wrap(facets = facet_by, scales = "free")
    }

    p
  }

  # group_by / splitting entire phyloseq by the group_by variable
  group_by_var <- phyloseq::sample_data(ps)[[group_by]]
  if (anyNA(group_by_var)) {
    message(
      "Warning: replacing NAs with 'NA's in group_by variable: ", group_by
    )
    phyloseq::sample_data(ps)[[group_by]][is.na(group_by_var)] <- "NA"
  }
  LEVELS <- unique(phyloseq::sample_data(ps)[[group_by]])

  if (isTRUE(is.null(LEVELS))) {
    # NULL when group_by = NA
    barplot_function(ps)
  } else {
    plots_list <- lapply(LEVELS, function(level) {
      this_group <- phyloseq::sample_data(ps)[[group_by]] == level
      ps <- phyloseq::prune_samples(samples = this_group, x = ps)
      barplot_function(ps)
    })
    # set y axis title to group level
    plots_list <- lapply(seq_along(LEVELS), function(level) {
      plots_list[[level]] + ggplot2::labs(title = LEVELS[[level]])
    })
    names(plots_list) <- LEVELS
    return(plots_list)
  }
}

#' Create a new tax_table rank called top
#'
#' Same as taxa_names for first N taxa, "other" otherwise.
#' Used in tax_agg and comp_barplot
#' (for when external sorted tax vec given, e.g. for ord_explore)
#'
#' @param tt tax_table ALREADY SORTED
#'
#' @return tax_table with new rank column: "top"
#' @examples
#' data("dietswap", package = "microbiome")
#' taxtab <- phyloseq::tax_table(dietswap)
#' tt_add_topN_var(taxtab, N = 5, other = "whatever")
#' @noRd
tt_add_topN_var <- function(tt, N, other = "other", varname = "top") {
  max_tax <- min(N, phyloseq::ntaxa(tt)) # in case N > number of taxa at rank
  top_taxons <- c(
    phyloseq::taxa_names(tt)[seq_len(max_tax)],
    rep_len(other, length.out = phyloseq::ntaxa(tt) - max_tax)
  )
  top_taxons <- matrix(top_taxons, ncol = 1)
  colnames(top_taxons) <- varname
  ranks <- phyloseq::rank_names(tt)
  # handle case of only rank being "unique"
  # (e.g. if started without tax_table)
  if (identical(ranks, "unique")) {
    tt_out <- cbind(top_taxons, tt)
  } else {
    tt_out <- cbind(
      # new tt except unique col
      tt[, phyloseq::rank_names(tt) != "unique"],
      top_taxons # new top col
    )
    if ("unique" %in% colnames(tt)) {
      # add unique col back on at end if present
      tt_out <- cbind(tt_out, tt[, "unique"])
    }
  }
  tt_out <- phyloseq::tax_table(tt_out)
  return(tt_out)
}
