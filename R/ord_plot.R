#' Customisable ggplot ordination (using ord_calc result)
#'
#' Draw ordination plot. Utilises results of \code{\link{ord_calc}}.
#' - For an extensive tutorial see the ordination vignette: \url{https://david-barnett.github.io/microViz/articles/ordination.html}
#' - For interpretation see the the relevant pages on PCA, PCoA, RDA, or CCA on the "gusta me" website: \url{https://sites.google.com/site/mb3gustame/}
#'
#' How to specify the plot_taxa argument (when using PCA, CCA or RDA):
#' - FALSE --> plot no taxa vectors or labels
#' - integer vector e.g. 1:3 --> plot labels for top 3 taxa (by longest line length)
#' - single numeric value e.g. 0.75 --> plot labels for taxa with line length > 0.75
#' - character vector e.g. c('g__Bacteroides', 'g__Veillonella') --> plot labels for the exactly named taxa
#'
#' @param data list object output from ord_calc
#' @param axes which axes to plot: numerical vector of length 2
#' @param plot_taxa if ord_calc method was "PCA/RDA" draw the taxa loading vectors (see details)
#' @param tax_vec_length NA = auto-scaling for line segment drawn for any taxa.
#' Alternatively provide a numeric length multiplier yourself.
#' @param tax_vec_style_all list of named aesthetic attributes for all (background) taxon vectors
#' @param tax_vec_style_sel list of named aesthetic attributes for taxon vectors for the taxa selected by plot_taxa
#' @param tax_lab_length multiplier for label distance/position for any selected taxa
#' @param tax_lab_style list of fixed aesthetics (colour, size etc) for the taxon labels
#' @param taxon_renamer function that takes any plotted taxon names and returns modified names for labels
#' @param plot_samples if TRUE, plot sample points with geom_point
#' @param constraint_vec_length NA = auto-scaling for line segment drawn for any constraints.
#' Alternatively provide a numeric length multiplier yourself.
#' @param constraint_vec_style list of aesthetics/arguments (colour, alpha etc) for the constraint vectors
#' @param constraint_lab_length relative length of label drawn for any constraints
#' (relative to default position which is defined by correlation with each drawn axis)
#' @param constraint_lab_style list of aesthetics/arguments (colour, size etc) for the constraint labels
#' @param var_renamer function to rename constraining variables for plotting their labels
#' @param scaling
#' Relevant for constrained ordinations: Type 2, or type 1 scaling. See \url{https://sites.google.com/site/mb3gustame/constrained-analyses/rda}
#' Either "species" or "site" scores are scaled by eigenvalues, and the other set of scores is left unscaled (from ?vegan::scores.cca)
#' @param auto_caption size of caption with info about the ordination, NA for none
#' @param center expand plot limits to center around origin point (0,0)
#' @param clip clipping of labels that extend outside plot limits?
#' @param expand expand plot limits a little bit further than data range?
#' @param interactive creates plot suitable for use with ggiraph
#' @param ...
#' pass aesthetics arguments for sample points,
#' drawn with geom_point using aes_string
#'
#' @return ggplot
#' @export
#'
#' @examples
#' library(ggplot2)
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use as constraints or conditions
#' dietswap <- dietswap %>%
#'   ps_mutate(
#'     weight = dplyr::recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'     female = dplyr::if_else(sex == "female", true = 1, false = 0)
#'   )
#'
#' # unconstrained PCA ordination
#' unconstrained_aitchison_pca <- dietswap %>%
#'   tax_transform("clr", rank = "Genus") %>%
#'   ord_calc() # method = "auto" --> picks PCA as no constraints or distances
#'
#' unconstrained_aitchison_pca %>%
#'   ord_plot(colour = "bmi_group", plot_taxa = 1:5) +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # you can generate an interactive version of the plot by specifying
#' # interactive = TRUE, and passing a variable name to another argument
#' # called `data_id` which is required for interactive point selection
#' interactive_plot <- unconstrained_aitchison_pca %>%
#'   ord_plot(
#'     colour = "bmi_group", plot_taxa = 1:5,
#'     interactive = TRUE, data_id = "sample"
#'   )
#'
#' # to start the html viewer, and allow selecting points, we must use a
#' # ggiraph function called girafe and set some options and css
#' ggiraph::girafe(
#'   ggobj = interactive_plot,
#'   options = list(
#'     ggiraph::opts_selection(
#'       css = ggiraph::girafe_css(
#'         css = "fill:orange;stroke:black;",
#'         point = "stroke-width:1.5px"
#'       ),
#'       type = "multiple", # this activates lasso selection (click top-right)
#'       only_shiny = FALSE # allows interactive plot outside of shiny app
#'     )
#'   )
#' )
#'
#'
#' # remove effect of weight with conditions arg
#' # scaling weight with scale_cc is not necessary as only 1 condition is used
#' dietswap %>%
#'   tax_transform("clr", rank = "Genus") %>%
#'   ord_calc(conditions = "weight", scale_cc = FALSE) %>%
#'   ord_plot(colour = "bmi_group") +
#'   stat_ellipse(aes(linetype = bmi_group, colour = bmi_group))
#'
#' # alternatively, constrain variation on weight and female
#' constrained_aitchison_rda <- dietswap %>%
#'   tax_transform("clr", rank = "Genus") %>%
#'   ord_calc(constraints = c("weight", "female")) # constraints --> RDA
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
#' # you can rename the taxa on the labels with any function that
#' # takes and modifies a character vector
#' constrained_aitchison_rda %>%
#'   ord_plot(
#'     colour = "bmi_group",
#'     plot_taxa = 1:3,
#'     taxon_renamer = function(x) stringr::str_extract(x, "^.")
#'   ) +
#'   lims(x = c(-5, 6), y = c(-5, 5)) +
#'   scale_colour_brewer(palette = "Set1")
#'
#' # You can plot PCoA and constrained PCoA plots too.
#' # You don't typically need/want to use transformed taxa variables for PCoA
#' # But it is good practice to call tax_transform("identity") so that
#' # the automatic caption can record that no transformation was applied
#' dietswap %>%
#'   tax_agg("Genus") %>%
#'   tax_transform("identity") %>%
#'   # so caption can record (lack of) transform
#'   dist_calc("bray") %>%
#'   # bray curtis
#'   ord_calc() %>%
#'   # guesses you want an unconstrained PCoA
#'   ord_plot(colour = "bmi_group")
#'
#' # it is possible to facet these plots
#' # (although I'm not sure it makes sense to)
#' # but only unconstrained ordination plots and with plot_taxa = FALSE
#' unconstrained_aitchison_pca %>%
#'   ord_plot(color = "sex", auto_caption = NA) +
#'   facet_wrap("sex") +
#'   theme(line = element_blank()) +
#'   stat_density2d(aes(colour = sex)) +
#'   guides(colour = FALSE)
#'
#' unconstrained_aitchison_pca %>%
#'   ord_plot(color = "bmi_group", plot_samples = FALSE, auto_caption = NA) +
#'   facet_wrap("sex") +
#'   theme(line = element_blank(), axis.text = element_blank()) +
#'   stat_density2d_filled(show.legend = FALSE) +
#'   geom_point(size = 1, shape = 21, colour = "black", fill = "white")
ord_plot <-
  function(data,
           axes = 1:2,
           plot_taxa = FALSE,
           tax_vec_length = NA,
           tax_vec_style_all = list(),
           tax_vec_style_sel = list(),
           tax_lab_length = tax_vec_length * 1.1,
           tax_lab_style = list(),
           taxon_renamer = function(x) identity(x),
           constraint_vec_length = NA,
           constraint_vec_style = list(),
           constraint_lab_length = constraint_vec_length * 1.1,
           constraint_lab_style = list(),
           var_renamer = function(x) identity(x),
           plot_samples = TRUE,
           scaling = 2, # or "species" scaling in vegan lingo
           auto_caption = 8,
           center = FALSE,
           clip = "off",
           expand = !center,
           interactive = FALSE,
           ...) {
    ps <- ps_get(data)
    ordination <- ord_get(data)

    # check input data object class and extract the most used objects to function env
    if (inherits(data, "ps_extra") && !identical(ordination, NULL)) {
      info <- info_get(data)
    } else {
      stop("data argument should be a ps_extra list, specifically the output from ord_calc")
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
    meta <- data.frame(phyloseq::sample_data(ps), check.names = FALSE)

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
      siteScoresDf <- as.data.frame(
        vegan::scores(ordination, display = "sites", choices = axes)
      )
      axeslabels <- axesNames <- colnames(siteScoresDf)
    } else {

      # compute summary of ordination object to ensure
      # consistent scaling of components
      ordsum <- summary(ordination, scaling = scaling)

      # retrieve scores from model object
      siteScoresDf <- as.data.frame(ordsum[["sites"]][, axes, drop = FALSE])

      # if RDA/PCA method: get species scores (aka feature loadings)
      if (info[["ordMethod"]] %in% c("RDA", "PCA", "CCA")) {
        speciesScoresDf <-
          as.data.frame(ordsum[["species"]][, axes, drop = FALSE])
      }

      # if constrained model: get constraints coordinates for plotting
      if (!identical(info[["constraints"]], NA_character_)) {
        constraintDf <-
          as.data.frame(ordsum[["biplot"]][, axes, drop = FALSE])
      }

      # extract "explained variation" for labelling axes
      explainedVar <-
        vegan::eigenvals(ordination)[axes] / sum(vegan::eigenvals(ordination))
      axesNames <- colnames(siteScoresDf)
      axeslabels <-
        paste0(axesNames, " [", sprintf("%.1f", 100 * explainedVar), "%]")
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
      ggplot2::coord_cartesian(clip = clip, default = TRUE, expand = expand)

    # set geom_point variable aesthetics
    aesthetics <- do.call(what = ggplot2::aes_string, args = aestheticArgs)

    # gather all args for use in geom_point (sample data)
    geompointArgs <- c(list(mapping = aesthetics), fixed_aesthetics)

    # add sample/site points, sized dynamically or fixed size
    if (plot_samples) {
      if (isTRUE(interactive)) {
        p <- p + do.call(ggiraph::geom_point_interactive, args = geompointArgs)
      } else {
        p <- p + do.call(ggplot2::geom_point, args = geompointArgs)
      }
    }

    # add loadings/ species-scores arrows for RDA/PCA methods
    if (info[["ordMethod"]] %in% c("RDA", "CCA", "PCA")) {

      # calculate line length for taxa vectors
      speciesLineLength <-
        sqrt(speciesScoresDf[, 1]^2 + speciesScoresDf[, 2]^2)

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
            speciesScoresDf[
              rev(order(speciesLineLength)),
            ][plot_taxa, , drop = FALSE]
          },
          # numeric e.g. 0.75 --> plot labels for taxa with line length > 0.75
          "numeric" = {
            speciesScoresDf[speciesLineLength > plot_taxa[[1]], , drop = FALSE]
          },
          # character e.g. c('g__Bacteroides', 'g__Veillonella')
          # --> plot labels for exactly named taxa
          "character" = {
            speciesScoresDf[
              rownames(speciesScoresDf) %in% plot_taxa, ,
              drop = FALSE
            ]
          }
        )

      # if a selection of species scores was calculated,
      # add lines and labels to plot
      if (!identical(selectSpeciesScoresDf, NULL)) {
        # automatic taxa vector length setting
        if (identical(tax_vec_length, NA)) {
          x <- max(siteScoresDf[[1]]) / max(speciesScoresDf[[1]])
          tax_vec_length <- x * 0.85
        }

        # (semi-transparent) lines for all taxa
        tax_vec_all_args <- list(
          data = speciesScoresDf * tax_vec_length,
          mapping = ggplot2::aes_string(
            xend = axesNames[1], yend = axesNames[2], x = 0, y = 0
          ),
          size = 0.5, alpha = 0.25
        )
        tax_vec_all_args[names(tax_vec_style_all)] <- tax_vec_style_all
        p <- p + do.call(what = ggplot2::geom_segment, args = tax_vec_all_args)

        # (opaque) lines for selected taxa
        tax_vec_sel_args <- list(
          data = selectSpeciesScoresDf * tax_vec_length,
          mapping = ggplot2::aes_string(
            xend = axesNames[1], yend = axesNames[2], x = 0, y = 0
          ),
          lineend = "round", linejoin = "mitre",
          arrow = grid::arrow(
            length = grid::unit(0.005, "npc"), type = "closed"
          ),
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
    if (!identical(info[["constraints"]], NA_character_)) {
      # automatic constraint length setting
      if (identical(constraint_vec_length, NA)) {
        x <- max(abs(siteScoresDf[[1]])) / max(abs(constraintDf[[1]]))
        constraint_vec_length <- x * 0.45
      }
      # draw vector segments at length set by constraint_vec_length argument
      # (proportion of original length)
      constraint_vec_args <- list(
        mapping = ggplot2::aes_string(
          xend = axesNames[1], yend = axesNames[2], x = 0, y = 0
        ),
        alpha = 0.8, colour = "brown",
        lineend = "round", linejoin = "mitre",
        arrow = grid::arrow(
          length = grid::unit(0.005, "npc"), type = "closed"
        ),
        data = constraintDf * constraint_vec_length
      )
      constraint_vec_args[names(constraint_vec_style)] <- constraint_vec_style
      p <- p +
        do.call(what = ggplot2::geom_segment, args = constraint_vec_args)

      # draw vector tip labels at length set by constraint_lab_length argument
      constraint_lab_args <- list(
        label = var_renamer(rownames(constraintDf)),
        data = constraintDf * constraint_lab_length,
        size = 2.5, colour = "brown", alpha = 0.8
      )
      constraint_lab_args[names(constraint_lab_style)] <- constraint_lab_style
      p <- p + do.call(what = ggplot2::geom_label, args = constraint_lab_args)
    }
    # auto caption ------------------------------------------------------------
    # add automated caption if requested (default size = 8)
    if (!identical(NA, auto_caption)) {
      o <- info[["ordMethod"]]
      if (o %in% c("RDA", "CCA", "CAP")) {
        o <- paste0(o, " (scaling=", scaling, ")")
      }
      if (!is.na(info[["constraints"]])) {
        o <- paste0(o, " constraints=", info[["constraints"]])
      }
      if (!is.na(info[["conditions"]])) {
        o <- paste0(o, " conditions=", info[["conditions"]])
      }

      caption <- paste0(
        nrow(p[["data"]]), " samples & ", phyloseq::ntaxa(ps),
        " taxa (", info[["tax_agg"]], "). ", o
      )

      if (!is.na(info[["tax_transform"]])) {
        caption <- paste0(caption, " tax_transform=", info[["tax_transform"]])
      }

      if (!is.na(info[["distMethod"]])) {
        caption <- paste0(caption, " dist=", info[["distMethod"]])
      }

      p <- p + ggplot2::labs(caption = caption) +
        ggplot2::theme(plot.caption = ggplot2::element_text(size = auto_caption))
    }

    # center the plot if requested using helper function
    if (isTRUE(center)) p <- center_plot(p, clip = clip, expand = expand)

    return(p)
  }

# helper functions
center_plot <- function(plot, clip = "off", expand = TRUE) {
  lims <- get_plot_limits(plot)
  plot + ggplot2::coord_cartesian(
    xlim = c(-max(abs(lims$x)), max(abs(lims$x))),
    ylim = c(-max(abs(lims$y)), max(abs(lims$y))),
    default = TRUE,
    clip = clip,
    expand = expand
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
