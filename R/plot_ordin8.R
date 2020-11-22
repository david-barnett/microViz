#' Customisable ggplot of ordin8 ordination result
#'
#' Ordination visualisation. Utilises results of \code{ordin8}.
#' Some inspiration was from here ref: \url{https://ourcodingclub.github.io/2018/05/04/ordination.html}
#'
#' How to specify the plot_taxa argument:
#' - integer vector e.g. 1:3 --> plot labels for top 3 taxa (by line length)
#' - single numeric value e.g. 0.75 --> plot labels for taxa with line length > 0.75
#' - character vector e.g. c('g__Bacteroides', 'g__Veillonella') --> plot labels for the exactly named taxa
#'
#' @param data list object output from ordin8
#' @param axes which axes to plot: numerical vector of length 2
#' @param constraint_label_length relative length of line segment drawn for any constraints (relative to default length which is defined by correlation with each drawn axis)
#' @param constraint_label_style list of fixed aesthetics (colour, size etc) for the constraint labels themselves
#' @param plot_taxa if ordin8 method was "RDA" draw the taxa loading vectors (see details)
#' @param taxon_label_length relative length of line segment drawn for any constraints
#' @param taxon_label_style list of fixed aesthetics (colour, size etc) for the taxon labels
#' @param taxon_renamer function that takes any plotted taxon names and returns modified names for labels
#' @param plot_samples if TRUE, plot sample points with geom_point
#' @param auto_title if TRUE, add a crude title and subtitle with info about the ordination
#' @param ... pass aesthetics arguments for sample points, drawn with geom_point using aes_string

#'
#' @return ggplot
#' @export
#' @importFrom ggplot2 ggplot aes aes_string geom_point geom_segment geom_label ggtitle theme_minimal xlab ylab
#'
#' @examples
#' library(phyloseq)
#' library(vegan)
#' library(microbiome)
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use as constraints or conditions
#' sample_data(dietswap)$female <-
#'   dplyr::if_else(sample_data(dietswap)$sex == "female", true = 1, false = 0)
#' sample_data(dietswap)$weight <-
#'   dplyr::recode(sample_data(dietswap)$bmi_group, obese = 3, overweight = 2, lean = 1)
#'
#' # compute and plot ordinations for demonstration of conditioning
#' unconstrained_aitchison_pca <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ordin8(method = "RDA")
#'
#' unconstrained_aitchison_pca %>%
#'   plot_ordin8(colour = "bmi_group") +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ordin8(method = "RDA", conditions = "weight") %>%
#'   plot_ordin8(colour = "bmi_group") +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' constrained_aitchison_rda <- dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("clr") %>%
#'   ordin8(method = "RDA", constraints = c("weight", "female"))
#'
#' constrained_aitchison_rda %>%
#'   plot_ordin8(colour = "bmi_group", constraint_label_length = 2) +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # ggplot allows additional customisation of the resulting plot
#' p <- constrained_aitchison_rda %>%
#'   plot_ordin8(
#'     colour = "bmi_group", constraint_label_length = 2,
#'     plot_taxa = 1:3, taxon_label_length = 5
#'   ) +
#'   lims(x = c(-5, 6), y = c(-5, 5)) +
#'   scale_colour_brewer(palette = "Set1")
#'
#' p + stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#' p + stat_density2d(aes(colour = bmi_group))
#'
#' # you can rename the taxa on the labels with any function that takes a modifies a character vector
#' constrained_aitchison_rda %>%
#'   plot_ordin8(
#'     colour = "bmi_group", constraint_label_length = 2,
#'     plot_taxa = 1:3, taxon_label_length = 5,
#'     taxon_renamer = function(x) stringr::str_extract(x, "^.")
#'   ) +
#'   lims(x = c(-5, 6), y = c(-5, 5)) +
#'   scale_colour_brewer(palette = "Set1")
#'
#' # it is possible to facet only unconstrained ordination plots (with plot_taxa = FALSE)
#' unconstrained_aitchison_pca %>%
#'   plot_ordin8(color = "sex") +
#'   facet_wrap("sex") +
#'   stat_density2d(aes(colour = sex)) +
#'   guides(colour = FALSE)
#'
#' unconstrained_aitchison_pca %>%
#'   plot_ordin8(color = "bmi_group", plot_samples = FALSE) +
#'   facet_wrap("sex") +
#'   stat_density2d_filled(show.legend = FALSE)
plot_ordin8 <-
  function(data,
           axes = 1:2,
           constraint_label_length = 1,
           constraint_label_style = list(size = 2.5, colour = "#D92027", alpha = 0.8), # red
           plot_taxa = FALSE,
           taxon_label_length = 1,
           taxon_label_style = list(size = 2.5, colour = "black", alpha = 0.8),
           taxon_renamer = function(x) identity(x),
           plot_samples = TRUE,
           auto_title = TRUE,
           ...) {
    data_arg_reminder <- "data argument should be a list, specifically the output from ordin8"

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
      variables <- colnames(microbiome::meta(ps))
      for (v in ellipses) {
        if (
          !is.null(v) && !(class(v) %in% c("logical", "numeric", "integer")) &&
            !(v %in% c(variables, grDevices::colors()))
        ) {
          stop(v, " is not a variable in the sample metadata")
        }
      }
    }

    # double-check ordination size against phyloseq (should never fail if ordin8 used)
    stopifnot(stats::nobs(ordination) == phyloseq::nsamples(ps))

    # get and transform aesthetic metadata ------------------------------------
    meta <- microbiome::meta(ps)

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

    # compute summary of ordination object to ensure consistent scaling of components
    ordsum <- summary(ordination, scaling = "species")

    # retrieve scores from model object
    siteScoresDf <- as.data.frame(ordsum[["sites"]][, axes, drop = FALSE])

    # if RDA/PCA method: get species scores (aka feature loadings)
    if (info[["method"]] == "RDA") {
      speciesScoresDf <- as.data.frame(ordsum[["species"]][, axes, drop = FALSE])
    }

    # if constrained model: get constraints coordinates for plotting
    if (info[["constraints"]][[1]] != 1) {
      constraintDf <- as.data.frame(ordsum[["biplot"]][, axes, drop = FALSE])
    }

    # extract "explained variation" for labelling axes
    explainedVar <- vegan::eigenvals(ordination)[axes] / sum(vegan::eigenvals(ordination))
    axesNames <- colnames(siteScoresDf)
    axeslabels <- paste0(axesNames, " [", sprintf("%.1f", 100 * explainedVar), "%]")

    # bind ordination axes vectors to metadata subset for plotting
    df <- dplyr::bind_cols(siteScoresDf, meta)


    # build ggplot ------------------------------------------------------------

    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes_string(x = axesNames[1], y = axesNames[2])
    ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = axeslabels[1], y = axeslabels[2])

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
      if (!is.null(selectSpeciesScoresDf[[1]])) {
        p <- p + ggplot2::geom_segment(
          mapping = ggplot2::aes_string(xend = 0, yend = 0),
          size = 0.5, linetype = "dashed",
          colour = taxon_label_style$colour,
          data = selectSpeciesScoresDf * taxon_label_length
        )

        if (taxon_label_length >= 1) {
          p <- p + ggplot2::geom_segment(
            mapping = ggplot2::aes_string(xend = axesNames[1], yend = axesNames[2], x = 0, y = 0),
            # TODO could colour taxa by phylum?
            colour = taxon_label_style$colour,
            lineend = "round", linejoin = "mitre", size = 1,
            arrow = grid::arrow(length = grid::unit(0.0025, "npc"), type = "closed"),
            data = speciesScoresDf
          )
        }

        p <- p + do.call(
          what = ggplot2::geom_label,
          args = c(
            list(
              label = taxon_renamer(rownames(selectSpeciesScoresDf)),
              data = selectSpeciesScoresDf * taxon_label_length
            ),
            taxon_label_style
          )
        )
      }
    }

    # if constrained ordination, plot constraints
    if (info[["constraints"]][[1]] != 1) {
      # draw narrow vector segments at length set by constraint_label_length argument (proportion of automatic length)
      p <- p + ggplot2::geom_segment(
        mapping = ggplot2::aes_string(xend = 0, yend = 0),
        colour = constraint_label_style$colour, size = 0.5,
        data = constraintDf * constraint_label_length
      )

      # draw thick vector segments at automatic length
      if (constraint_label_length >= 1) {
        p <- p + ggplot2::geom_segment(
          mapping = ggplot2::aes_string(xend = axesNames[1], yend = axesNames[2], x = 0, y = 0),
          size = 1.5, colour = constraint_label_style$colour,
          lineend = "round", linejoin = "mitre",
          arrow = grid::arrow(length = grid::unit(0.005, "npc"), type = "closed"),
          data = constraintDf
        )
      }
      # draw vector tip labels at length set by constraint_label_length argument
      p <- p + do.call(
        what = ggplot2::geom_label,
        args = c(
          list(
            label = rownames(constraintDf),
            data = constraintDf * constraint_label_length
          ),
          constraint_label_style
        )
      )
    }

    # add automated title if requested (default TRUE)
    if (auto_title) {
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

      MAIN <- paste(stats::nobs(df), "samples &", phyloseq::ntaxa(ps), "taxa")
      SUB <- paste(infoElements, collapse = ". ")

      p <- p + ggplot2::ggtitle(label = MAIN, subtitle = SUB)
    }

    return(p)
  }
