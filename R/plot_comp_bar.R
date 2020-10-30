#' Pretty & flexible barplots of sample composition
#'
#' No gaps, unless you want them. Grouped bars and bar outlines if you want them.
#' Consistent, distinct, fairly pleasant colouring.
#'
#' @param ps phyloseq object
#' @param n_taxa how many distinct taxa to colour in the plot (otherwise "other")
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param tax_order or what? taxonomy grouping?
#' @param palette palette for taxa fill colours
#' @param sample_order any distance measure in calc_dist that does not require a phylogenetic tree (or a variable?)
#' @param label could also consider arbitrary annotation with extra info, like in complex heatmap
#' @param groups splits dataset by this variable (must be categorical) and uses patchwork to assemble multiple plots?
#' @param horizontal typically you have more samples than taxa and more vertical space than horizontal, so TRUE makes sense as default
#' @param bar_width  default 1 avoids random gapping otherwise seen with many samples (set to something less than 1 to introduce gaps between fewer samples)
#' @param bar_outline_colour line colour separating taxa and samples
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
#' # make a list of 2 harmonised composition plots (grouped by sex)
#' p <- plot_comp_bar(dietswap, n_taxa = 15, bar_outline_colour = "black",
#'                    sample_order = 'aitchison', groups = 'sex')
#'
#' # plot them side by side with patchwork package
#' patch <- patchwork::wrap_plots(p, ncol = 2, guides = 'collect')
#' patch
#'
#' # beautifying tweak #
#' # modify one plot in place (flip the order of the samples in the 2nd plot)
#' # notice that the scaling is for the x-axis
#' # (that's because coord_flip is used inside plot_comp_bar)
#' patch[[2]] <- patch[[2]] + scale_x_discrete(limits = rev)
#' # Explainer: rev() function takes current limits and reverses them.
#' # You could also pass a completely arbitrary order, naming all samples
#'
#' # you can theme all plots with the & operator
#' patch & theme(axis.text.y = element_text(size = 5), legend.text = element_text(size = 6))
#' # See https://patchwork.data-imaginist.com/index.html
#'
#'
plot_comp_bar <- function(
  ps,
  n_taxa = 10,
  tax_level = "Genus",
  tax_order = "abundance",
  palette = c("lightgrey", rev(distinct_palette(n_taxa))),
  sample_order = "aitchison",
  label = "SAMPLE",
  groups = NA,
  horizontal = TRUE,
  bar_width = 1,
  bar_outline_colour = 'black',
  drop_unused_vars = TRUE,
  seriate_method = "OLO_ward"
){

  # how many taxa to plot (otherwise group into other)
  ps <- microbiome::aggregate_top_taxa(ps, top = n_taxa, level = tax_level)

  # set colour scheme
  ordered_taxa <- rev(c(setdiff(microbiome::top_taxa(ps), "Other"), "Other"))
  names(palette) <- ordered_taxa

  # determine sample ordering option
  if(length(sample_order) == 1 && !sample_order %in% phyloseq::sample_variables(ps)){
    samples_ordered_by_similarity <- TRUE
  }

  # create a sample names variable if this will be used for labelling
  if (label == "SAMPLE") {
    phyloseq::sample_data(ps)$SAMPLE <- phyloseq::sample_names(ps)
  }

  # establish a labelling function (for the samples)
  meta <- microbiome::meta(ps)
  LABELLER <- function(SAMPLES) {
    lapply(SAMPLES, function(SAMPLE) {
      meta[rownames(meta) == SAMPLE, label]
    })
  }

  # drop unused variables
  if (isTRUE(drop_unused_vars)) {
    kept_vars <- label
    if (isFALSE(is.na(groups))) {
      kept_vars <- c(union(kept_vars, groups))
    }
    if (!samples_ordered_by_similarity) {
      kept_vars <- c(union(kept_vars, sample_order))
    }
    phyloseq::sample_data(ps) <- microbiome::meta(ps)[, kept_vars, drop = FALSE]
  }


  # define function to actually create the plot/plots
  plot_function <- function(ps){

    # sample ordering
    if (samples_ordered_by_similarity) {
      # calculate distance between samples for pretty ordering
      distMat <- ps %>%
        # microbiome::transform(transform = "compositional") %>%
        calc_dist(dist = sample_order) %>% .[["distMat"]]
      ser <- seriation::seriate(x = distMat, method = seriate_method)
      s_order <- seriation::get_order(ser)
      ordered_samples <- attr(distMat, which = "Labels")[s_order]
    }

    # create long dataframe from compositional phyloseq
    df <- ps %>%
      microbiome::transform(transform = "compositional") %>%
      phyloseq::psmelt()

    # set abundance order of taxa at chosen rank level
    df[[tax_level]] <- factor(df[[tax_level]], levels = ordered_taxa, ordered = TRUE)

    # set sample order
    df[["SAMPLE"]] <- factor(df[["SAMPLE"]], levels = ordered_samples, ordered = TRUE)


    p <- ggplot(df, aes_string(x = "SAMPLE", y = "Abundance", fill = tax_level))

    if (is.na(bar_outline_colour)) {
      p <- p + geom_bar(position = "stack", stat = "identity", width = bar_width)
    } else {
      p <- p + geom_bar(position = "stack", stat = "identity", width = bar_width, colour = bar_outline_colour)
    }

    p <- p +
      theme(panel.background = element_blank(), panel.grid = element_blank()) +
      scale_x_discrete(
        # limits = ordered_samples,
        breaks = ordered_samples,
        labels = LABELLER) +
      scale_fill_manual(values = palette, guide = guide_legend(reverse = TRUE))

    if (horizontal) {
      p <- p + coord_flip()
    }

    if(label == "SAMPLE"){
      p <- p + xlab(NULL)
    }

    p
  }
  # grouping / splitting entire phyloseq (multiple susbset_sample calls)
  LEVELS <- unique(phyloseq::sample_data(ps)[[groups]])

  if(isTRUE(is.null(LEVELS))){
    plot_function(ps)
  } else {
    plots_list <- lapply(LEVELS, function(level){
      keepers <- phyloseq::sample_data(ps)[[groups]] == level
      ps <- phyloseq::prune_samples(samples = keepers, x = ps)
      plot_function(ps)
    })
    names(plots_list) <- LEVELS
    return(plots_list)
  }
}
