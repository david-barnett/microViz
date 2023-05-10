#' Circular compositional barplot sorted by ordination angle
#'
#' @description
#' Use with `ord_calc` output as data argument.
#' Order of samples extracted from ordination axes in data.
#' Best paired with ordination plot made from same `ord_calc` output.
#'
#' @details
#' data must also contain counts table if taxa were transformed (e.g. for clr PCA ordination)
#' (i.e. you must have used `tax_transform` with keep_counts = TRUE, if transformation was not "identity")
#'
#' You cannot set a variable fill aesthetic (only fixed) for the annotation points,
#' as the fill is used for the taxonomic composition bars
#'
#' @param data psExtra output of ord_calc
#' @param tax_level taxonomic aggregation level (from rank_names(ps))
#' @param axes which 2 axes of ordination to use for ordering bars
#' @param n_taxa how many taxa to colour show distinct colours for (all other taxa grouped into "other").
#' @param taxon_renamer function to rename taxa in the legend
#' @param ord_plot add a matching ordination plot to your iris plot
#' ('list' returns separate plots in a list, 'above'/'below' uses patchwork to pair plots together into one)
#' @param palette colour palette
#' @param anno_colour name of sample_data variable to use for colouring geom_segment annotation ring
#' @param anno_colour_style list of further arguments passed to geom_segment e.g. size
#' @param anno_binary name(s) of binary sample_data variable(s) (levels T/F or 1/0) to use for filtered geom_point annotation ring(s) (annotates at TRUE values)
#' @param anno_binary_style list of further arguments passed to geom_point e.g. colour, size, y, etc.
#' @param keep_all_vars slows down processing but is required for any post-hoc plot customisation options
#' @inheritParams ord_plot
#' @param count_warn warn if count data are not available? i.e. phyloseq otu_table is not positive integers and psExtra counts slot is NULL
#' @inheritDotParams comp_barplot
#' merge_other other_name bar_width bar_outline_colour bar_outline_width
#' tax_order tax_transform_for_plot interactive max_taxa
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
#' # although these iris plots are great for 100s of samples
#' # we'll take a subset of the data (for speed in this example)
#' ps <- dietswap %>%
#'   ps_filter(timepoint %in% c(1, 2)) %>%
#'   # copy an otu to the sample data
#'   ps_otu2samdat("Prevotella melaninogenica et rel.") %>%
#'   # create a couple of useful variables
#'   ps_mutate(
#'     female = sex == "female",
#'     african = nationality == "AFR",
#'     log_P.melaninogenica = log10(`Prevotella melaninogenica et rel.` + 1)
#'   )
#'
#' # define a function for taking the end off the long genus names in this dataset
#' tax_renamer <- function(tax) {
#'   stringr::str_remove(tax, " [ae]t rel.")
#' }
#'
#' ord <- ps %>%
#'   tax_agg("Genus") %>%
#'   dist_calc("aitchison") %>%
#'   ord_calc(method = "PCoA")
#'
#' # ordination plot for comparison
#' ord %>% ord_plot(color = "log_P.melaninogenica", size = 3)
#'
#' ord_plot_iris(
#'   data = ord,
#'   tax_level = "Genus",
#'   n_taxa = 10,
#'   anno_colour = "nationality",
#'   anno_colour_style = list(size = 3),
#'   anno_binary = "female",
#'   anno_binary_style = list(shape = "F", size = 2.5),
#'   taxon_renamer = tax_renamer
#' ) +
#'   scale_colour_brewer(palette = "Dark2")
#'
#' # It is also possible to use comp_barplot customisation arguments
#' # like bar_width and bar_outline_colour, and to make interactive iris plots
#' # using ggiraph:
#'
#' if (interactive()) {
#'   hover_over_me <- ord_plot_iris(
#'     data = ord,
#'     tax_level = "Genus",
#'     n_taxa = 10,
#'     anno_colour = "nationality",
#'     anno_colour_style = list(size = 3),
#'     anno_binary = "female",
#'     anno_binary_style = list(shape = "F", size = 2.5),
#'     taxon_renamer = tax_renamer,
#'     interactive = TRUE,
#'     bar_width = 0.8, bar_outline_colour = "black"
#'   ) +
#'     scale_colour_brewer(palette = "Dark2")
#'
#'   ggiraph::girafe(ggobj = hover_over_me)
#' }
#'
#' # Using PCA for ordination after transformations (e.g. clr) means the untransformed taxonomic
#' # data are only available for plotting as compositions if you transformed with
#' # tax_transform(keep_counts = TRUE) and your original data were in fact counts.
#' # Compositional data will also work, and you can set count_warn to FALSE to avoid the warning
#'
#' clr_pca <- ps %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(method = "PCA")
#'
#' # you can generate a simple paired layout of ord_plot and iris plot
#' # or separately create and pair the plots yourself, for more control
#'
#' # simple pairing
#' ord_plot_iris(
#'   data = clr_pca, n_taxa = 12,
#'   tax_level = "Genus",
#'   taxon_renamer = tax_renamer,
#'   ord_plot = "below",
#'   bar_width = 0.8, bar_outline_colour = "black",
#'   anno_binary = "african",
#'   anno_binary_style = list(
#'     y = 1.08, colour = "gray50", shape = "circle open", size = 1, stroke = 1.5
#'   )
#' )
#'
#' # manual pairing
#' plot1 <- clr_pca %>% ord_plot(
#'   plot_taxa = 6:1, tax_vec_length = 0.6,
#'   colour = "gray50", shape = "nationality",
#'   taxon_renamer = tax_renamer,
#'   auto_caption = NA, center = TRUE,
#' ) +
#'   scale_shape_manual(values = c(AFR = "circle", AAM = "circle open"))
#'
#' iris <- ord_plot_iris(
#'   data = clr_pca, n_taxa = 15,
#'   tax_level = "Genus",
#'   taxon_renamer = tax_renamer,
#'   anno_binary = "african",
#'   anno_binary_style = list(y = 1.05, colour = "gray50", shape = "circle", size = 1)
#' ) +
#'   # shrink legend text size
#'   theme(legend.text = element_text(size = 7))
#'
#' cowplot::plot_grid(plot1, iris, nrow = 1, align = "h", axis = "b", rel_widths = 3:4)
#'
#' # you can add multiple rings of binary annotations
#' ord_plot_iris(
#'   data = clr_pca, n_taxa = 15,
#'   tax_level = "Genus",
#'   taxon_renamer = tax_renamer,
#'   anno_binary = c("african", "female"),
#'   anno_binary_style = list(
#'     colour = c("gray50", "coral"),
#'     shape = c("circle", "F"), size = c(0.5, 2)
#'   )
#' ) +
#'   theme(legend.text = element_text(size = 7))
ord_plot_iris <- function(data,
                          tax_level,
                          axes = 1:2,
                          n_taxa = 10,
                          ord_plot = "none", # list/above/below (left, right?)
                          taxon_renamer = function(x) identity(x),
                          palette = distinct_palette(n_taxa),
                          anno_colour = NULL,
                          anno_colour_style = list(),
                          anno_binary = NULL,
                          anno_binary_style = list(),
                          keep_all_vars = FALSE,
                          scaling = 2,
                          count_warn = TRUE,
                          ...) {
  check_is_phyloseq(data, argName = "data")
  rlang::arg_match(ord_plot, values = c("none", "list", "below", "above"))
  if (!identical(ord_plot, "none")) {
    ord_p <- ord_plot(
      data = data, axes = axes, taxon_renamer = taxon_renamer, center = TRUE,
      colour = c(anno_colour, "darkgrey")[[1]] # darkgrey if anno_colour NULL
    )
  }

  # get count data, warns if not count
  ps <- ps_counts(data, warn = count_warn)

  # sort samples by ordination order
  ps <- ps_sort_ord(ps, ord = data, axes = axes, scaling = scaling)

  # drop to only annotation vars, if any given
  annos <- c(anno_binary, anno_colour)
  if (isFALSE(keep_all_vars) && !identical(annos, NULL)) {
    ps <- ps_select(ps, dplyr::all_of(annos))
  }

  # fixed barplot args
  fixed_args <- list(
    ps = ps,
    tax_level = tax_level,
    taxon_renamer = taxon_renamer,
    palette = palette,
    n_taxa = n_taxa,
    keep_all_vars = TRUE, # this is already handle by ord_plot_iris
    sample_order = "asis" # uses ps order already set by ps_sort_ord!
  )

  # default extra args to barplot still modifiable and extendible
  modifiable_args <- list(bar_outline_colour = NA)
  dots <- list(...)
  modifiable_args[names(dots)] <- dots

  # all args for barplot, bargs...
  bargs <- c(fixed_args, modifiable_args)

  iris <- do.call(comp_barplot, args = bargs)
  # set up polar coordinates and style
  iris <- iris +
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
    # this expansion to set inner and outer radius is fairly hackish
    # TODO improve and maybe make flexible for user choice?
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(add = c(0.5, -0.1))
    )

  if (!identical(anno_colour, NULL)) {
    # set default args
    ac_args <- list(y = 1.05, yend = 1.10, mapping = ggplot2::aes(
      xend = .data[["SAMPLE"]], colour = .data[[anno_colour]]
    ))
    # set appropriate linewidth/size arg, dependent on ggplot2 version
    oldGG <- isTRUE(utils::packageVersion("ggplot2") < "3.4.0")
    if (oldGG) ac_args[["size"]] <- 1 else ac_args[["linewidth"]] <- 1

    # overwrite defaults and add any other args
    ac_args[names(anno_colour_style)] <- anno_colour_style
    # add to plot
    iris <- iris + do.call(ggplot2::geom_segment, args = ac_args)
  } else {
    # for default positioning of anno_binary
    ac_args <- list(yend = 1)
  }

  if (!identical(anno_binary, NULL)) {
    y <- ac_args[["yend"]]
    for (i in seq_along(anno_binary)) {
      y <- y + i * 0.05
      # set default args
      ab_args <- list(
        # paste formula together to ensure anno_binary[i] evaluates
        # otherwise all do.calls use formula with last i value
        data = stats::as.formula(paste0("~ .[.[['", anno_binary[i], "']] == TRUE, ]")),
        y = y, shape = "circle", size = 1, colour = "black", show.legend = FALSE
      )
      # overwrite defaults and add any other args
      ab_args[names(anno_binary_style)] <- purrr::map(anno_binary_style, .f = i)
      # add to plot
      iris <- iris + do.call(ggplot2::geom_point, args = ab_args)
      if (!identical(ord_plot, "none")) {
        ab_args_ord <- ab_args[!names(ab_args) %in% c("y", "shape", "size")]
        ab_args_ord[["shape"]] <- "circle open"
        ord_p <- ord_p + do.call(ggplot2::geom_point, args = ab_args_ord)
      }
    }
  }

  if (identical(ord_plot, "none")) {
    return(iris)
  }

  plots <- list(iris = iris, ord_plot = ord_p)
  if (ord_plot == "list") {
    return(plots)
  } else {
    rlang::check_installed(
      pkg = "patchwork", reason = "to place `ord_plot` 'above' or 'below'"
    )
    if (ord_plot == "above") plots <- plots[2:1] # flip order
    return(patchwork::wrap_plots(plots, ncol = 1))
  }
}
