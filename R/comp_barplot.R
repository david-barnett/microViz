#' Plot (grouped and ordered) compositional barplots
#'
#' @description
#' Stacked barplots showing composition of phyloseq samples for a specified number of coloured taxa.
#' Normally your phyloseq object should contain counts data,
#' as by default `comp_barplot()` performs the "compositional" taxa transformation for you,
#' and requires count input for some sample_order methods!
#'
#' @details
#' - sample_order: Either specify a list of sample names to order manually, or the bars/samples can/will be sorted by similarity, according to a specified distance measure (default 'bray'-curtis),
#' - seriate_method specifies a seriation/ordering algorithm (default Ward hierarchical clustering with optimal leaf ordering, see seriation::list_seriation_methods())
#' - group_by: You can group the samples on distinct plots by levels of a variable in the phyloseq object. The list of ggplots produced can be arranged flexibly with the patchwork package functions. If you want to group by several variables you can create an interaction variable with interaction(var1, var2) in the phyloseq sample_data BEFORE using comp_barplot.
#' - facet_by can allow faceting of your plot(s) by a grouping variable. Using this approach is less flexible than using group_by but means you don't have to arrange a list of plots yourself like with the group_by argument. Using facet_by is equivalent to adding a call to facet_wrap(facets = facet_by, scales = "free") to your plot(s). Calling facet_wrap() yourself is itself a more flexible option as you can add other arguments like the number of rows etc. However you must use keep_all_vars = TRUE if you will add faceting manually.
#' - bar_width: No gaps between bars, unless you want them (decrease width argument to add gaps between bars).
#' - bar_outline_colour: Bar outlines default to "grey5" for almost black outlines. Use NA if you don't want outlines.
#' - merge_other: controls whether bar outlines can be drawn around individual (lower abundance) taxa that are grouped in "other" category. If you want to see the diversity of taxa in "other" use merge_taxa = FALSE, or use TRUE if you prefer the cleaner merged look
#' - palette: Default colouring is consistent across multiple plots if created with the group_by argument, and the defaults scheme retains the colouring of the most abundant taxa irrespective of n_taxa
#'
#' @param ps
#' phyloseq object
#' @param tax_level
#' taxonomic aggregation level (from rank_names(ps))
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
#' or "asis" for the current order as is returned by phyloseq::sample_names(ps)
#' @param order_with_all_taxa
#' if TRUE, this will always use all taxa (not just the top n_taxa)
#' to calculate any distances needed for sample ordering
#' @param label
#' name of variable to use for labelling samples, or "SAMPLE" for sample names
#' @param group_by splits dataset by this variable (must be categorical)
#' - resulting in a list of plots, one for each level of the group_by variable.
#' @param facet_by
#' facets plots by this variable (must be categorical). If group_by is also set
#' the faceting will occur separately in the plot for each group.
#' @param bar_width
#' default 1 avoids random gapping otherwise seen with many samples
#' (set to less than 1 to introduce gaps between samples)
#' @param bar_outline_colour
#' line colour separating taxa and samples (use NA for no outlines)
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
#' @param interactive
#' creates plot suitable for use with ggiraph
#' @param max_taxa
#' maximum distinct taxa groups to show
#' (only really useful for limiting complexity of interactive plots
#' e.g. within ord_explore)
#' @param other_name
#' name for other taxa after N
#' @param ...
#' extra arguments passed to facet_wrap() (if facet_by is not NA)
#' @param x
#' name of variable to use as x aesthetic:
#' it probably only makes sense to change this when also using facets
#' (check only one sample is represented per bar!)
#' @param counts_warn
#' should a warning be issued if counts are unavailable?
#' TRUE, FALSE, or "error" (passed to ps_get)
#'
#' @return ggplot or list of harmonised ggplots
#' @export
#'
#' @examples
#' library(ggplot2)
#' data(dietswap, package = "microbiome")
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
#' # change colour palette with the distinct_palette() function
#' # remember to set the number of colours to the same as n_taxa argument!
#' dietswap %>%
#'   ps_filter(timepoint == 1) %>%
#'   comp_barplot(
#'     tax_level = "Family", n_taxa = 8,
#'     bar_outline_colour = NA,
#'     sample_order = "bray",
#'     bar_width = 0.7,
#'     palette = distinct_palette(8, pal = "kelly"),
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
#' # Order taxa differently:
#' # By default, taxa are ordered by total sum across all samples
#' # You can set a different function for calculating the order, e.g. median
#' dietswap %>%
#'   ps_filter(timepoint == 1) %>%
#'   comp_barplot(tax_level = "Genus", tax_order = median) +
#'   coord_flip()
#'
#' # Or you can set the taxa order up front, with tax_sort() and use it as is
#' dietswap %>%
#'   ps_filter(timepoint == 1) %>%
#'   tax_sort(at = "Genus", by = sum) %>%
#'   comp_barplot(tax_level = "Genus", tax_order = "asis") +
#'   coord_flip()
#'
#' # how many taxa are in those light grey "other" bars?
#' # set merge_other to find out (& remember to set a bar_outline_colour)
#' dietswap %>%
#'   ps_filter(timepoint == 1) %>%
#'   comp_barplot(
#'     tax_level = "Genus", n_taxa = 12, merge_other = FALSE, bar_outline_colour = "grey50",
#'   ) +
#'   coord_flip()
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
comp_barplot <- function(ps,
                         tax_level,
                         n_taxa = 8,
                         tax_order = sum,
                         merge_other = TRUE,
                         taxon_renamer = function(x) identity(x),
                         sample_order = "bray",
                         order_with_all_taxa = FALSE,
                         label = "SAMPLE",
                         group_by = NA,
                         facet_by = NA,
                         bar_width = 1,
                         bar_outline_colour = "grey5",
                         bar_outline_width = 0.1,
                         palette = distinct_palette(n_taxa),
                         tax_transform_for_ordering = "identity",
                         tax_transform_for_plot = "compositional",
                         seriate_method = "OLO_ward",
                         keep_all_vars = TRUE,
                         interactive = FALSE,
                         max_taxa = 10000,
                         other_name = "Other",
                         x = "SAMPLE",
                         counts_warn = TRUE,
                         ...) {
  ps <- ps_get(ps)

  if (identical(tax_level, ".Taxon")) {
    stop("'.Taxon' cannot be used as a rank name! You must rename that rank.")
  }
  if (!rlang::is_scalar_integerish(n_taxa) || n_taxa < 1) {
    stop("n_taxa must be a positive integer")
  }
  if (!rlang::is_scalar_integerish(max_taxa) || max_taxa < n_taxa) {
    stop("max_taxa must be a positive integer, and not lower than n_taxa")
  }
  if (!rlang::is_character(sample_order)) {
    stop("sample_order must be character")
  }

  # check phyloseq for common problems (and fix or message about this)
  ps <- psCheckSamdat(ps, verbose = TRUE)
  ps <- psCheckTaxTable(ps, verbose = TRUE, min_tax_length = 3)

  # create a sample names variable if this will be used for labelling
  if (identical(label, "SAMPLE") || identical(x, "SAMPLE")) {
    phyloseq::sample_data(ps)[["SAMPLE"]] <- phyloseq::sample_names(ps)
  }
  if (!is.null(label) && !label %in% phyloseq::sample_variables(ps)) {
    stop('label must be "SAMPLE" or a sample variable name, or NULL for none')
  }
  if (!rlang::is_string(x) || !x %in% phyloseq::sample_variables(ps)) {
    stop('x must be "SAMPLE" or a sample variable name')
  }

  # taxa: aggregate and order for bar ordering and plotting ------------------

  # include "unique" rank when aggregating
  ps <- tax_agg(ps, rank = tax_level, add_unique = TRUE)
  ps <- ps_get(ps)

  if (length(tax_order) == 1) {
    # reorder taxa if tax_order given as a length one rule
    ps <- tax_sort(ps, by = tax_order, use_counts = TRUE, counts_warn = counts_warn)
  } else {
    # reorder taxa if tax_order given as presumably a vector of taxa names
    ps <- tax_reorder(ps, tax_order = tax_order, tree_warn = FALSE, ignore = other_name)
  }

  # create "top" rank ---------------------------------------------------------
  phyloseq::tax_table(ps) <- phyloseq::tax_table(ps) %>%
    tt_add_topN_var(N = n_taxa, other = other_name, varname = ".top")

  # ordering samples up front? -----------------------------------------------
  if (identical(sample_order, "default") || identical(sample_order, "asis")) {
    sample_order <- phyloseq::sample_names(ps) # overwrite so not used as dist
  } else if (length(sample_order) > 1) {
    # sample_order given as vector of names? check all exist as sample names
    checkFixedSampleOrder(ps = ps, sample_order = sample_order)
    ps <- ps_reorder(ps, sample_order = sample_order)
  }

  # drop unused variables if requested ---------------------------------------
  if (identical(keep_all_vars, FALSE)) {
    keptVars <- c(x, label, group_by, facet_by)
    keptVars <- unique(keptVars[!is.na(keptVars)])
    ps <- ps_select(ps, dplyr::all_of(keptVars))
  }

  # create a list of phyloseqs, one per group level in group_by, or just one
  if (is.null(group_by) || rlang::is_na(group_by)) {
    ps_list <- list(ps) # no names
  } else {
    # check grouping variable for NAs and replace with "NA"s
    group_by_var <- phyloseq::sample_data(ps)[[group_by]]
    if (anyNA(group_by_var)) {
      message(
        "Warning: replacing NAs with 'NA's in group_by variable: ", group_by
      )
      phyloseq::sample_data(ps)[[group_by]][is.na(group_by_var)] <- "NA"
    }
    LEVELS <- unique(phyloseq::sample_data(ps)[[group_by]])
    # split phyloseq into multiple phyloseq objects by grouping variable
    ps_list <- lapply(LEVELS, function(level) {
      ps_filter(ps, .data[[group_by]] == level, .keep_all_taxa = TRUE)
      # it is NECESSARY for uniform taxa ordering and colour to keep all taxa.
    })
    names(ps_list) <- LEVELS
  }

  # modify each phyloseq, sort and merge top taxa if appropriate
  ps_list <- lapply(ps_list, function(ps) {
    # possibly calculate sample_order before any merging of "other" category
    if (isTRUE(order_with_all_taxa) && length(sample_order) == 1) {
      ps <- ps_seriate(
        ps = ps, method = seriate_method, dist = sample_order,
        tax_transform = tax_transform_for_ordering
      )
    }

    # merge ".top" rank's `other_name` category into one taxon to allow drawing
    # bar outlines everywhere except within `other_name` category bars
    if (isTRUE(merge_other)) {
      ps <- tax_agg(ps, rank = ".top", force = TRUE, add_unique = TRUE)
      ps <- ps_get(ps)
    } else {
      ps <- taxMaxEnforce(ps = ps, maxTaxa = max_taxa, otherName = other_name)
    }

    # possibly calculate sample_order AFTER any merging of "other" category
    if (isFALSE(order_with_all_taxa) && length(sample_order) == 1) {
      ps <- ps_seriate(
        ps = ps, method = seriate_method, dist = sample_order,
        tax_transform = tax_transform_for_ordering
      )
    }
    return(ps)
  })

  # draw a plot per phyloseq in ps_list
  plots <- lapply(ps_list, function(ps) {
    comp_barplotFixed(
      ps = ps, interactive = interactive,
      tax_transform_for_plot = tax_transform_for_plot,
      palette = palette, taxon_renamer = taxon_renamer,
      bar_width = bar_width, bar_outline_colour = bar_outline_colour,
      bar_outline_width = bar_outline_width, label = label,
      x = x, facet_by = facet_by, tax_level = tax_level, ...
    )
  })

  # return early if no group_by was provided
  if (is.null(group_by) || rlang::is_na(group_by)) {
    return(plots[[1]])
  }

  # otherwise add titles for each group
  plots <- lapply(seq_along(LEVELS), function(level) {
    plots[[level]] + ggplot2::labs(title = LEVELS[[level]])
  })
  names(plots) <- LEVELS
  return(plots)
}

# function to actually create one barplot with fixed taxa levels and palette
# possibly fixed sample order
comp_barplotFixed <- function(ps, interactive,
                              tax_transform_for_plot, palette, taxon_renamer,
                              bar_width, bar_outline_colour, bar_outline_width,
                              label, x, facet_by, tax_level, ...) {
  # sample order was already fixed, so infer it from phyloseq
  sample_order <- unique(phyloseq::sample_names(ps))
  uniqueTaxaOrdered <- unique(phyloseq::tax_table(ps)[, "unique"])
  topTaxaOrdered <- unique(phyloseq::tax_table(ps)[, ".top"])
  # fix taxa colour scheme (colours only applied to top taxa)
  if (identical(names(palette), NULL)) names(palette) <- topTaxaOrdered

  # setup labelling samples -------------------------------------------------
  # establish a SAMPLE labelling function (NULL is none)
  if (!is.null(label) && x == "SAMPLE") {
    labels <- samdatAsDataframe(ps)[[label]]
    names(labels) <- phyloseq::sample_names(ps)
    LABELLER <- function(SAMPLES) {
      lapply(SAMPLES, function(SAMPLE) labels[[SAMPLE]])
    }
  }
  if (is.null(label)) LABELLER <- NULL # will remove labels

  # transform taxa values for display on plot
  ps <- tax_transform(ps, trans = tax_transform_for_plot, keep_counts = FALSE)
  ps <- ps_get(ps, warn = FALSE)

  # prepare dataframe for plot ----------------------------------------------
  # create long dataframe from (compositional) phyloseq
  df <- ps_melt(ps)

  # set fixed order of stacked taxa bars by creating ordered factor
  df[[".Taxon"]] <- factor(df[["unique"]], levels = rev(uniqueTaxaOrdered))

  # # set fixed order of fill colours (for LEGEND ordering!)
  df[[tax_level]] <- factor(df[[".top"]], levels = topTaxaOrdered)
  df[[".top"]] <- NULL

  # set sample order
  df[["SAMPLE"]] <- factor(df[["Sample"]], levels = sample_order)

  # build plot
  p <- ggplot2::ggplot(data = df, mapping = ggplot2::aes(
    x = .data[[x]], y = .data[["Abundance"]],
    fill = .data[[tax_level]], group = .data[[".Taxon"]]
  ))
  if (identical(x, "SAMPLE")) p <- p + ggplot2::xlab(NULL)

  p <- addGeomStackedBars(
    p = p, interactive = interactive, width = bar_width,
    colour = bar_outline_colour, linewidth = bar_outline_width
  )

  # theme plot
  p <- p +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_manual(
      values = palette, labels = taxon_renamer, limits = force
    )
  # limits = force is a fix (hopefully temporary) for new ggplot2 behaviour
  # all named values appear in legend even if they don't exist in the data
  # this is a known bug introduced in ggplot2 3.3.4
  # this only matters if named values given for palette, e.g. in ord_explore
  # see https://github.com/tidyverse/ggplot2/issues/4511#issuecomment-866185530
  if (x == "SAMPLE" || is.null(label)) {
    p <- p + ggplot2::scale_x_discrete(labels = LABELLER)
  }

  if (!identical(facet_by, NA)) {
    p <- p + ggplot2::facet_wrap(facets = facet_by, scales = "free", ...)
  }

  return(p)
}

# helper to add geom_col or geom_col_interactive to comp_barplot ggplot
addGeomStackedBars <- function(p, interactive, width, colour, linewidth) {
  args <- list(position = "stack", width = width, colour = colour)
  if (utils::packageVersion("ggplot2") < "3.4.0") {
    args[["size"]] <- linewidth
  } else {
    args[["linewidth"]] <- linewidth
  }
  if (isTRUE(interactive)) {
    fun <- ggiraph::geom_col_interactive
    args[["mapping"]] <- ggplot2::aes(data_id = .data[["unique"]], tooltip = .data[["unique"]])
  } else {
    fun <- ggplot2::geom_col
  }
  return(p + do.call(fun, args))
}

#' Create a new tax_table rank, lumping taxa after first N
#'
#' Same as taxa_names for first N taxa, "other" otherwise.
#' Used in tax_agg and comp_barplot
#' (for when external sorted tax vec given, e.g. for ord_explore)
#'
#' @param tt tax_table, must be ALREADY SORTED
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

# if sample_order is given as vector of names,
# check they all exist as sample names in phyloseq
checkFixedSampleOrder <- function(ps, sample_order) {
  # check sample_order argument values are all in phyloseq sample names
  if (!all(sample_order %in% phyloseq::sample_names(ps))) {
    notFound <- sample_order[!sample_order %in% phyloseq::sample_names(ps)]
    stop(
      "1 or more of the sample_order values are not phyloseq sample_names:",
      "\n-> '", paste(notFound, collapse = "', '"), "'"
    )
  }
  # check sample_order same length as nsamples
  if (length(sample_order) != phyloseq::nsamples(ps)) {
    stop(
      "Length of sample_order must be 1 or same as number of samples!",
      "\n- phyloseq::nsamples(ps) is: ", phyloseq::nsamples(ps),
      "\n- length(sample_order) is: ", length(sample_order)
    )
  }
}

# enforce max_taxa limit (avoids v.expensive plots in ord_explore)
taxMaxEnforce <- function(ps, maxTaxa, otherName) {
  # aggregate the phyloseq at a high maximum number of displayable taxa
  phyloseq::tax_table(ps) <- tt_add_topN_var(
    tt = phyloseq::tax_table(ps), N = maxTaxa - 1,
    other = otherName, varname = "..sep" # temp variable, can't be "unique"
  )
  ps <- tax_agg(ps, rank = "..sep", force = TRUE, add_unique = TRUE)
  ps <- ps_get(ps)
  ps <- tax_mutate(ps = ps, ..sep = NULL)
  return(ps)
}
