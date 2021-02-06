#' Customisable ggplot ordination (using ord_calc result)
#'
#' Ordination visualisation. Utilises results of \code{ord_calc}.
#' - For interpretation see the the relevant pages on PCA, PCoA, RDA, or CCA on the "gusta me" website: \url{https://sites.google.com/site/mb3gustame/}
#' - Some other inspiration was from here: \url{https://ourcodingclub.github.io/2018/05/04/ordination.html}
#'
#' How to specify the plot_taxa argument:
#' - FALSE --> plot no taxa vectors or labels
#' - integer vector e.g. 1:3 --> plot labels for top 3 taxa (by longest line length)
#' - single numeric value e.g. 0.75 --> plot labels for taxa with line length > 0.75
#' - character vector e.g. c('g__Bacteroides', 'g__Veillonella') --> plot labels for the exactly named taxa
#'
#' @param data list object output from ord_calc
#' @param axes which axes to plot: numerical vector of length 2
#' @param scaling either "species" (2) or "site" (1) scores are scaled by eigenvalues, and the other set of scores is left unscaled (from ?vegan::scores.cca)
#' @param constraint_vec_length NA = auto-scaling for line segment drawn for any constraints. Alternatively provide a numeric length multiplier yourself.
#' @param constraint_vec_style list of aesthetics/arguments (colour, alpha etc) for the constraint vectors
#' @param constraint_lab_length relative length of label drawn for any constraints (relative to default position which is defined by correlation with each drawn axis)
#' @param constraint_lab_style list of aesthetics/arguments (colour, size etc) for the constraint labels
#' @param var_renamer function to rename constraining variables for plotting their labels
#' @param plot_taxa if ord_calc method was "PCA/RDA" draw the taxa loading vectors (see details)
#' @param tax_vec_length NA = auto-scaling for line segment drawn for any taxa. Alternatively provide a numeric length multiplier yourself.
#' @param tax_vec_style_all list of named aesthetic attributes for all (background) taxon vectors
#' @param tax_vec_style_sel list of named aesthetic attributes for taxon vectors for the taxa selected by plot_taxa
#' @param tax_lab_length multiplier for label distance/position for any selected taxa
#' @param tax_lab_style list of fixed aesthetics (colour, size etc) for the taxon labels
#' @param taxon_renamer function that takes any plotted taxon names and returns modified names for labels
#' @param plot_samples if TRUE, plot sample points with geom_point
#' @param auto_caption if TRUE, add a small font caption with info about the ordination
#' @param center expand plot limits to center around origin point (0,0)
#' @param ... pass aesthetics arguments for sample points, drawn with geom_point using aes_string
#'
#' @return ggplot
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' library(vegan)
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use as constraints or conditions
#' dietswap <- dietswap %>%
#'   ps_mutate(
#'     weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'     female = if_else(sex == "female", true = 1, false = 0)
#'   )
#'
#' # compute and plot ordinations for demonstration of conditioning
#' unconstrained_aitchison_pca <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(method = "RDA")
#'
#' unconstrained_aitchison_pca %>%
#'   ord_plot(colour = "bmi_group", plot_taxa = 1:5) +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # remove effect of weight with conditions arg
#' dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(method = "RDA", conditions = "weight") %>%
#'   ord_plot(colour = "bmi_group") +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # or instead constrain on weight and female
#' constrained_aitchison_rda <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ord_calc(method = "RDA", constraints = c("weight", "female"))
#'
#' constrained_aitchison_rda %>%
#'   ord_plot(colour = "bmi_group", constraint_vec_length = 2) +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # ggplot allows additional customisation of the resulting plot
#' p <- constrained_aitchison_rda %>%
#'   ord_plot(colour = "bmi_group", plot_taxa = 1:3) +
#'   lims(x = c(-5, 6), y = c(-5, 5)) +
#'   scale_colour_brewer(palette = "Set1")
#'
#' p + stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#' p + stat_density2d(aes(colour = bmi_group))
#'
#' # you can rename the taxa on the labels with any function that takes and modifies a character vector
#' constrained_aitchison_rda %>%
#'   ord_plot(
#'     colour = "bmi_group",
#'     plot_taxa = 1:3,
#'     taxon_renamer = function(x) stringr::str_extract(x, "^.")
#'   ) +
#'   lims(x = c(-5, 6), y = c(-5, 5)) +
#'   scale_colour_brewer(palette = "Set1")
#'
#' # it is possible to facet only unconstrained ordination plots (with plot_taxa = FALSE)
#' unconstrained_aitchison_pca %>%
#'   ord_plot(color = "sex") +
#'   facet_wrap("sex") +
#'   stat_density2d(aes(colour = sex)) +
#'   guides(colour = FALSE)
#'
#' unconstrained_aitchison_pca %>%
#'   ord_plot(color = "bmi_group", plot_samples = FALSE) +
#'   facet_wrap("sex") +
#'   stat_density2d_filled(show.legend = FALSE)
ord_plot <-
  function(data,
           axes = 1:2,
           scaling = "species",
           constraint_vec_length = NA,
           constraint_vec_style = list(),
           constraint_lab_length = constraint_vec_length * 1.1,
           constraint_lab_style = list(),
           var_renamer = function(x) identity(x),
           plot_taxa = FALSE,
           tax_vec_length = NA,
           tax_vec_style_all = list(),
           tax_vec_style_sel = list(),
           tax_lab_length = tax_vec_length * 1.1,
           tax_lab_style = list(),
           taxon_renamer = function(x) identity(x),
           plot_samples = TRUE,
           auto_caption = TRUE,
           center = FALSE,
           ...) {
    data_arg_reminder <- "data argument should be a list, specifically the output from ord_calc"

    # check input data object class and extract the most used objects to function env
    if (inherits(data, "list")) {
      ps <- data[["ps"]]
      info <- data[["info"]]
      ordination <- data[["ordination"]]
      if (is.null(ordination)) {
        stop(data_arg_reminder)
      }
    } else {
      stop(data_arg_reminder)
    }

    # get ellipses optional arguments (aesthetics for geom_point)
    ellipses <- list(...)
    # properly delete any ellipses arguments set to NULL
    if (length(ellipses) > 0) {
      ellipses[sapply(ellipses, is.null)] <- NULL
    }

    # check there are still ellipses args left after removing nulls
    if (length(ellipses) > 0) {
      # check aesthetics colour, shape, size and alpha are all in dataset (or numeric-esque)
      variables <- phyloseq::sample_variables(ps)
      for (v in ellipses) {
        if (
          !is.null(v) && !(class(v) %in% c("logical", "numeric", "integer")) &&
            !(v %in% c(variables, grDevices::colors()))
        ) {
          stop(v, " is not a variable in the sample metadata")
        }
      }
    }

    # double-check ordination size against phyloseq (should never fail if ord_calc used)
    stopifnot(stats::nobs(ordination) == phyloseq::nsamples(ps))

    # get and transform aesthetic metadata ------------------------------------
    meta <- data.frame(phyloseq::sample_data(ps))

    # set variable and fixed ggplot aesthetics based on metadata names check
    aestheticArgs <- ellipses[ellipses %in% colnames(meta)]
    fixed_aesthetics <- ellipses[!ellipses %in% colnames(meta)]

    # set colour variables to factors, if they're not null or numeric-like
    if (!is.null(aestheticArgs$colour)) {
      if (class(meta[[aestheticArgs$colour]]) %in% c("numeric", "difftime")) {
        meta[[aestheticArgs$colour]] <- as.numeric(meta[[aestheticArgs$colour]])
      } else {
        meta[[aestheticArgs$colour]] <- as.factor(meta[[aestheticArgs$colour]])
      }
    }
    # and coerce shape variable to factor if it is a non-fixed variable
    if (!is.null(aestheticArgs$shape)) {
      meta[[aestheticArgs$shape]] <- as.factor(meta[[aestheticArgs$shape]])
    }

    # get data point positions ------------------------------------------------

    # NMDS and DCA ordinations needs alternative handling
    if (inherits(ordination, c("decorana", "metaMDS"))) {
      siteScoresDf <- as.data.frame(vegan::scores(ordination, display = "sites", choices = axes))
      axeslabels <- axesNames <- colnames(siteScoresDf)
    } else {

      # compute summary of ordination object to ensure consistent scaling of components
      ordsum <- summary(ordination, scaling = scaling)

      # retrieve scores from model object
      siteScoresDf <- as.data.frame(ordsum[["sites"]][, axes, drop = FALSE])

      # if RDA/PCA method: get species scores (aka feature loadings)
      if (info[["method"]] %in% c("RDA", "CCA")) {
        speciesScoresDf <- as.data.frame(ordsum[["species"]][, axes, drop = FALSE])
      }

      # if constrained model: get constraints coordinates for plotting
      if (!identical(info[["constraints"]], 1)) {
        constraintDf <- as.data.frame(ordsum[["biplot"]][, axes, drop = FALSE])
      }

      # extract "explained variation" for labelling axes
      explainedVar <- vegan::eigenvals(ordination)[axes] / sum(vegan::eigenvals(ordination))
      axesNames <- colnames(siteScoresDf)
      axeslabels <- paste0(axesNames, " [", sprintf("%.1f", 100 * explainedVar), "%]")
    }
    # bind ordination axes vectors to metadata subset for plotting
    df <- dplyr::bind_cols(siteScoresDf, meta)


    # build ggplot ------------------------------------------------------------

    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes_string(x = axesNames[1], y = axesNames[2])
    ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = axeslabels[1], y = axeslabels[2]) +
      ggplot2::coord_cartesian(clip = "off", default = TRUE)

    # set geom_point variable aesthetics
    aesthetics <- do.call(what = ggplot2::aes_string, args = aestheticArgs)

    # gather all args for use in geom_point (sample data)
    geompointArgs <- c(list(mapping = aesthetics), fixed_aesthetics)

    # add sample/site points, sized dynamically or fixed size
    if (plot_samples) {
      p <- p + do.call(ggplot2::geom_point, args = geompointArgs)
    }

    # add loadings/ species-scores arrows for RDA/PCA methods
    if (info[["method"]] == "RDA") {

      # calculate line length for taxa vectors
      speciesLineLength <- sqrt(speciesScoresDf[, 1]^2 + speciesScoresDf[, 2]^2)

      # return subselection of taxa for which to draw labels on plot
      selectSpeciesScoresDf <-
        switch(class(plot_taxa[[1]]),
          # default plot_taxa == TRUE --> line length > 1
          "logical" = {
            if (isTRUE(plot_taxa)) {
              speciesScoresDf[speciesLineLength > 1, , drop = FALSE]
            } else {
              NULL
            }
          },
          # integer e.g. 1:3 --> plot labels for top 3 taxa (by line length)
          "integer" = {
            speciesScoresDf[rev(order(speciesLineLength)), ][plot_taxa, , drop = FALSE]
          },
          # numeric e.g. 0.75 --> plot labels for taxa with line length > 0.75
          "numeric" = {
            speciesScoresDf[speciesLineLength > plot_taxa[[1]], , drop = FALSE]
          },
          # character e.g. c('g__Bacteroides', 'g__Veillonella') --> plot labels for exactly named taxa
          "character" = {
            speciesScoresDf[rownames(speciesScoresDf) %in% plot_taxa, , drop = FALSE]
          }
        )

      # if a selection of species scores was calculated, add lines and labels to plot
      if (!identical(selectSpeciesScoresDf, NULL)) {
        # automatic taxa vector length setting
        if (identical(tax_vec_length, NA)) {
          x <- max(siteScoresDf[[1]]) / max(speciesScoresDf[[1]])
          tax_vec_length <- x * 0.85
        }

        # (semi-transparent) lines for all taxa
        tax_vec_all_args <- list(
          data = speciesScoresDf * tax_vec_length,
          mapping = ggplot2::aes_string(xend = axesNames[1], yend = axesNames[2], x = 0, y = 0),
          size = 0.5, alpha = 0.25
        )
        tax_vec_all_args[names(tax_vec_style_all)] <- tax_vec_style_all
        p <- p + do.call(what = ggplot2::geom_segment, args = tax_vec_all_args)

        # (opaque) lines for selected taxa
        tax_vec_sel_args <- list(
          data = selectSpeciesScoresDf * tax_vec_length,
          mapping = ggplot2::aes_string(xend = axesNames[1], yend = axesNames[2], x = 0, y = 0),
          lineend = "round", linejoin = "mitre",
          arrow = grid::arrow(length = grid::unit(0.005, "npc"), type = "closed"),
          size = 0.5
        )
        tax_vec_sel_args[names(tax_vec_style_sel)] <- tax_vec_style_sel
        p <- p + do.call(what = ggplot2::geom_segment, args = tax_vec_sel_args)

        # selected taxa labels
        tax_lab_args <- list(
          label = taxon_renamer(rownames(selectSpeciesScoresDf)),
          data = selectSpeciesScoresDf * tax_lab_length,
          size = 2, alpha = 0.8
        )
        tax_lab_args[names(tax_lab_style)] <- tax_lab_style
        p <- p + do.call(what = ggplot2::geom_label, args = tax_lab_args)
      }
    }

    # if constrained ordination, plot constraints
    if (!identical(info[["constraints"]], 1)) {
      # automatic constraint length setting
      if (identical(constraint_vec_length, NA)) {
        x <- max(abs(siteScoresDf[[1]])) / max(abs(constraintDf[[1]]))
        constraint_vec_length <- x * 0.45
      }
      # draw vector segments at length set by constraint_vec_length argument (proportion of automatic length)
      constraint_vec_args <- list(
        mapping = ggplot2::aes_string(xend = axesNames[1], yend = axesNames[2], x = 0, y = 0),
        alpha = 0.8, colour = "brown",
        lineend = "round", linejoin = "mitre",
        arrow = grid::arrow(length = grid::unit(0.005, "npc"), type = "closed"),
        data = constraintDf * constraint_vec_length
      )
      constraint_vec_args[names(constraint_vec_style)] <- constraint_vec_style
      p <- p + do.call(what = ggplot2::geom_segment, args = constraint_vec_args)

      # draw vector tip labels at length set by constraint_lab_length argument
      constraint_lab_args <- list(
        label = var_renamer(rownames(constraintDf)),
        data = constraintDf * constraint_lab_length,
        size = 2.5, colour = "brown", alpha = 0.8
      )
      constraint_lab_args[names(constraint_lab_style)] <- constraint_lab_style
      p <- p + do.call(what = ggplot2::geom_label, args = constraint_lab_args)
    }

    # add automated title if requested (default TRUE)
    if (auto_caption) {
      infoElements <- list(
        m = paste("method =", info[["method"]]),
        t = paste("tax_level =", info[["tax_level"]])
      )
      for (i in setdiff(names(info), c("method", "tax_level", "constraints"))) {
        if (!rlang::is_null(info[[i]])) {
          infoElements[[i]] <- paste(i, "=", info[[i]])
        }
      }
      if (!identical(info[["constraints"]], 1)) {
        infoElements[["cs"]] <- paste("constraints =", paste(info[["constraints"]], collapse = "+"))
      }

      caption <- paste(stats::nobs(df), "samples &", phyloseq::ntaxa(ps), "taxa.", scaling, "scaling.")
      caption <- paste(caption, paste(infoElements, collapse = ". "))

      p <- p + ggplot2::labs(caption = caption) +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = 6))
    }

    # center the plot if requested using helper function
    if (isTRUE(center)) p <- center_plot(p)

    return(p)
  }

# helper functions
center_plot <- function(plot) {
  lims <- get_plot_limits(plot)
  plot + ggplot2::coord_cartesian(
    xlim = c(-max(abs(lims$x)), max(abs(lims$x))),
    ylim = c(-max(abs(lims$y)), max(abs(lims$y))),
    default = TRUE
  )
}

get_plot_limits <- function(plot) {
  gb <- ggplot2::ggplot_build(plot)
  list(
    x = c(
      min = gb$layout$panel_params[[1]]$x.range[1],
      max = gb$layout$panel_params[[1]]$x.range[2]
    ),
    y = c(
      min = gb$layout$panel_params[[1]]$y.range[1],
      max = gb$layout$panel_params[[1]]$y.range[2]
    )
  )
}
