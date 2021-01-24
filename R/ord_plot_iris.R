#' Circular compositional barplot sorted by ordination angle
#'
#'
#'
#' @param data list output of ord_calc
#' @param axes which 2 axes of ordination to use for ordering
#' @param scaling ordination scaling option: "species" or "sites" scaled?
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param n_taxa how many taxa to colour show distinct colours for (all other taxa grouped into "Other").
#' @param keep_all_vars slows down processing but is required for any post-hoc plot customisation options
#' @param anno_colour name of sample_data variable to use for colouring geom_segment annotation ring
#' @param anno_colour_style list of further arguments passed to geom_segment e.g. size
#' @param anno_binary name of binary sample_data variable (levels T/F or 1/0) to use for filtered geom_point annotation ring (annotates at TRUE values)
#' @param anno_binary_style list of further arguments passed to geom_point e.g. colour, size, etc.
#'
#' @return ggplot
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#' data("dietswap", package = "microbiome")
#'
#' ps <- dietswap %>%
#' ps_filter(timepoint %in% c(1,2)) %>%
#'   ps_otu2samdat("Prevotella melaninogenica et rel.") %>%
#'   ps_mutate(
#'     female = sex == "female",
#'     log_P.melaninogenica = log10(Prevotella.melaninogenica.et.rel. + 1)
#'   )
#'
#' ord <- ps %>%
#'   dist_calc("aitchison") %>%
#'   ord_calc("PCoA")
#'
#' # ordination plot for comparison
#' ord_plot(ord, color = "log_P.melaninogenica", size = 3)
#'
#' ord_plot_iris(
#'   data = ord,
#'   tax_level = "Genus",
#'   n_taxa = 12,
#'   anno_colour = "nationality",
#'   anno_colour_style = list(size = 3),
#'   anno_binary = "female",
#'   anno_binary_style = list(shape = "F", size = 3)
#' ) +
#' ggplot2::scale_colour_brewer(palette = "Dark2")
#'
ord_plot_iris <- function(
                          data,
                          axes = 1:2,
                          scaling = "species",
                          tax_level,
                          n_taxa = 10,
                          keep_all_vars = FALSE,
                          anno_colour,
                          anno_colour_style = list(),
                          anno_binary,
                          anno_binary_style = list()) {
  ps <- data$ps
  # get axes
  df <- as.data.frame.matrix(
    vegan::scores(
      data$ordination,
      choices = axes, display = "sites", scaling = scaling
    )
  )

  df[["ID"]] <- rownames(df)

  # find anticlockwise angle from right x-axis
  df[["angle"]] <- dplyr::if_else(
    condition = df[[2]] > 0,
    true = atan2(x = df[[1]], y = df[[2]]),
    false = (2 * pi) + (atan2(x = df[[1]], y = df[[2]]))
  )

  df <- dplyr::arrange(df, .data$angle)
  sorted_samples <- rownames(df)
  ps <- ps_reorder(ps, sample_order = sorted_samples)

  if (isFALSE(keep_all_vars)) {
    ps <- ps_select(ps, dplyr::all_of(c(anno_binary, anno_colour)))
  }

  iris <- microViz::plot_comp_bar(
    ps = ps,
    tax_level = tax_level,
    n_taxa = n_taxa,
    sample_order = "default",
    bar_outline_colour = NA,
    keep_all_vars = TRUE
  ) +
    ggplot2::coord_polar(
      direction = -1,
      start = 3 * pi / 2,
      clip = "off"
    ) + ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = c(0.5, -0.1))
    )

  if (!identical(anno_colour, NULL)) {
    # set default args
    ac_args <- list(
      y = 1.05, yend = 1.10, size = 1,
      mapping = ggplot2::aes_string(xend = "SAMPLE", colour = anno_colour)
    )
    # overwrite defaults and add any other args
    ac_args[names(anno_colour_style)] <- anno_colour_style

    iris <- iris + do.call(ggplot2::geom_segment, args = ac_args)
  }

  if (!identical(anno_binary, NULL)) {
    # set default args
    ab_args <- list(
      data = ~ .[.[[anno_binary]] == TRUE, ],
      y = 1.175, shape = "circle", size = 1, colour = "black", show.legend = FALSE
    )
    # overwrite defaults and add any other args
    ab_args[names(anno_binary_style)] <- anno_binary_style

    iris <- iris + do.call(ggplot2::geom_point, args = ab_args)
  }
  iris
}

