#' Circular compositional barplot sorted by ordination angle
#'
#' - Order of samples extracted from ordination axes in `ord`. (`ord_calc` output)
#' - `ord_plot_iris` Uses the phyloseq from `ord` for plotting compositions if no problematic transform mentioned in `ord$info`
#' - Otherwise, ps must be a phyloseq object containing untransformed counts.
#' - (`ps` must be otherwise identical to the ps used to make `ord`!)
#'
#'
#' @param ord list output of ord_calc
#' @param ps phyloseq object containing untransformed counts if needed (must otherwise be identical to ps used to make ord!)
#' @param axes which 2 axes of ordination to use for ordering
#' @param scaling ordination scaling option: "species" or "sites" scaled?
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param n_taxa how many taxa to colour show distinct colours for (all other taxa grouped into "Other").
#' @param taxon_renamer function to rename taxa in the legend
#' @param palette colour palette
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
#' library(ggplot2)
#' data("dietswap", package = "microbiome")
#'
#' ps <- dietswap %>%
#'   ps_filter(timepoint %in% c(1, 2)) %>%
#'   ps_otu2samdat("Prevotella melaninogenica et rel.") %>%
#'   ps_mutate(
#'     female = sex == "female",
#'     log_P.melaninogenica = log10(Prevotella.melaninogenica.et.rel. + 1)
#'   )
#'
#' # define a function for taking end off the long genus names in this dataset
#' tax_renamer <- function(tax) {
#'   stringr::str_remove(tax, " [ae]t rel.")
#' }
#'
#' ord <- ps %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("aitchison") %>%
#'   ord_calc("PCoA")
#'
#' # ordination plot for comparison
#' ord %>% ord_plot(
#'   color = "log_P.melaninogenica", size = 3
#' )
#'
#' ord_plot_iris(
#'   ord = ord,
#'   tax_level = "Genus",
#'   n_taxa = 10,
#'   anno_colour = "nationality",
#'   anno_colour_style = list(size = 3),
#'   anno_binary = "female",
#'   anno_binary_style = list(shape = "F", size = 3),
#'   taxon_renamer = tax_renamer
#' ) +
#'   scale_colour_brewer(palette = "Dark2")
#'
#' # Using PCA for ordination after transformations (e.g. clr) means the untransformed taxonomic
#' # data are not available for plotting as compositions.
#' # This means you also need to provide the untransformed phyloseq object e.g.
#'
#' clr_pca <- ps %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc("PCA")
#'
#' plot1 <- clr_pca %>% ord_plot(
#'   plot_taxa = 1:6, tax_vec_length = 0.7,
#'   color = "grey", auto_caption = FALSE,
#'   taxon_renamer = tax_renamer
#' ) +
#'   # avoid clipping long labels
#'   coord_cartesian(clip = "off")
#'
#' iris <- ord_plot_iris(
#'   ord = clr_pca,
#'   ps = ps, n_taxa = 10,
#'   tax_level = "Genus",
#'   taxon_renamer = tax_renamer
#' ) +
#'   # shrink legend text size
#'   theme(legend.text = element_text(size = 7))
#'
#' patchwork::wrap_plots(plot1, iris, nrow = 1)
ord_plot_iris <- function(
                          ord,
                          ps = NULL,
                          axes = 1:2,
                          scaling = "species",
                          tax_level,
                          n_taxa = 10,
                          taxon_renamer = function(x) identity(x),
                          palette = c("lightgrey", rev(distinct_palette(n_taxa))),
                          keep_all_vars = FALSE,
                          anno_colour = NULL,
                          anno_colour_style = list(),
                          anno_binary = NULL,
                          anno_binary_style = list()) {
  if (identical(ps, NULL)) {
    transf <- ord[["info"]][["tax_transform"]]
    # check for mention of transformed taxonomic data in ord$info$transform
    if (!identical(transf, NULL) && !transf %in% c("none specified", "identity")) {
      stop(
        "The ord argument object info specifies a ", transf, " transformation has been applied, so compositions cannot be plotted.\n",
        "To resolve this, please also supply the untransformed phyloseq data to the ps argument."
      )
    } else {
      # use the phyloseq from ord if no problematic transform mentioned
      ps <- ord[["ps"]]
    }
  }

  # get axes
  df <- as.data.frame.matrix(
    vegan::scores(
      ord$ordination,
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

  # drop to only annotation vars, if any given
  annos <- c(anno_binary, anno_colour)
  if (isFALSE(keep_all_vars) && !identical(annos, NULL)) {
    ps <- ps_select(ps, dplyr::all_of(annos))
  }

  iris <- microViz::plot_comp_bar(
    ps = ps,
    tax_level = tax_level,
    taxon_renamer = taxon_renamer,
    palette = palette,
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