# ## plotting individual taxa distributions
#
# library(ggplot2)
# library(tidyr)
# library(phyloseq)
# load_all()
# ps <- microbiomeutilities::hmp2
# ps <- tax_filter(ps = ps, min_prevalence = 1/10)
# ps <- tax_agg(ps, "Family") %>% ps_get()
#
#
# taxa = microbiome::top_taxa(ps)
# prevalence = TRUE
# abundance = TRUE
# undetected = 0
#
# # get selected taxa names
# all_taxa <- phyloseq::taxa_names(ps)
# names(all_taxa) <- all_taxa
# if(identical(taxa, NA)){
#   taxa <- all_taxa
# } else {
#   taxa <- all_taxa[taxa]
# }
#
# # create prevalence plots
# if (prevalence) p_prev <- plot_prev(ps = ps, taxa = rev(taxa), undetected = 0, horizontal = FALSE) +
#   colorspace::scale_colour_continuous_sequential(palette = "Grays", begin = 0.2, aesthetics = c("fill", "colour"))
#
# # create abundance plots
# if (abundance) p_abund <- plot_abund(ps = ps, taxa = rev(taxa), undetected = 0, horizontal = FALSE, trans = "sqrt", dist_scale = "width")
#
# legend <- cowplot::get_legend(p_prev)
# plots <- cowplot::align_plots(p_prev + theme(legend.position = "none", axis.text.y = element_blank()), p_abund + theme(legend.position = "none", axis.text.y = element_blank()), align = "h", axis = "b")
#
# cowplot::plot_grid(plots[[1]], plots[[2]])

plot_abund <- function(ps, taxa, undetected = 0, horizontal = FALSE, trans = "identity", dist_scale = "width", colour = "prevalence") {
  otu <- otu_get(ps)
  otu <- otu[, taxa, drop = FALSE]
  prevalence <- prev_calc(ps, taxa = taxa, undetected = undetected)
  prev_df <- data.frame(prev. = prevalence, name = names(prevalence))
  sample_total <- phyloseq::sample_sums(ps)

  df <- as.data.frame.matrix(otu)
  df <- tibble::rownames_to_column(df, var = "...sample...")
  df <- cbind(df, sample_total)
  df[["undetected_prop"]] <- undetected / df[["sample_total"]]
  df <- tidyr::pivot_longer(df, cols = -dplyr::all_of(c("...sample...", "sample_total", "undetected_prop")))
  df <- dplyr::left_join(df, prev_df, by = "name")
  df[["name"]] <- factor(df[["name"]], levels = taxa)
  df[["value"]] <- df[["value"]] / df[["sample_total"]]

  p <- df %>%
    dplyr::filter(.data[["name"]] %in% taxa, .data[["value"]] > .data[["undetected_prop"]]) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = "name", y = "value"))

  if (identical(colour, "prevalence")) {
    p <- p +
      ggplot2::geom_violin(ggplot2::aes(colour = .data[["prev."]]), fill = NA, scale = dist_scale, trim = TRUE, width = 0.7) +
      ggplot2::geom_point(ggplot2::aes(fill = .data[["prev."]], colour = .data[["prev."]]), size = 0.1) +
      colorspace::scale_color_continuous_sequential(palette = "Grays", aesthetics = c("colour", "fill"), begin = 0.2)
  } else {
    p <- p + ggplot2::geom_violin(colour = colour, fill = NA, scale = dist_scale, trim = TRUE, width = 0.7) +
      ggforce::geom_sina(fill = colour, colour = colour, size = 0.3, scale = "area")
  }
  p +
    ggplot2::scale_y_continuous(trans = trans, n.breaks = 3) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() + ggplot2::theme(panel.grid = ggplot2::element_line(size = 0.1), axis.title = ggplot2::element_blank()) +
    NULL
}

# if plot will be vertical (and not faceted) then taxa should be rev(taxa) to give top-down ordering
# p <- ps %>% plot_prev(taxa)
# p + facet_wrap(facets = "name", ncol = 1, scales = "free_y")
plot_prev <- function(ps, taxa, undetected = 0, horizontal = FALSE, colour = "prevalence") {
  # compute data
  prevalence <- prev_calc(data = ps, taxa = taxa, undetected = undetected)
  df <- data.frame(name = factor(names(prevalence), levels = taxa), prev = prevalence)

  # plot data
  p <- ggplot2::ggplot(df, mapping = ggplot2::aes(y = .data[["name"]], x = .data[["prev"]])) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title = ggplot2::element_blank(), panel.grid = ggplot2::element_line(size = 0.1)) +
    ggplot2::coord_cartesian(xlim = 0:1, default = TRUE) +
    ggplot2::ggtitle("Prev.") +
    ggplot2::scale_x_continuous(breaks = c(0, 1))

  if (!identical(colour, "prevalence")) {
    p <- p + ggplot2::geom_col(fill = colour, width = 0.7)
  } else {
    p <- p + ggplot2::geom_col(ggplot2::aes(fill = .data[["prev"]]), width = 0.7)
  }

  if (isTRUE(horizontal)) p <- p + ggplot2::coord_flip()

  return(p)
}

