#' Interactively explore microbial compositions of ordinated samples
#'
#' @description
#' A Shiny app used like an interactive version of ord_plot (taking the output of ord_calc).
#' You can select samples on an ordination plot and view their composition with stacked barplots.
#'
#' Once running: click and drag to select 2 or more samples to view their compositions.
#' You can style the ordination plot points and compositional barplot using the options in the left panel.
#'
#'
#' @details
#' If you get an interactive error like the one below:
#'
#' "ids don't have the same length than str (most often, it occurs because of clipping)"
#'
#' 1. make your points smaller
#' 2. set point shape to fixed, or to a variable with fewer categories
#'
#' Limitation: When a selection grouping variable is NA for some samples,
#' that grouping variable cannot be used to select those samples
#'
#'
#' @param data ps_extra list output of ord_calc
#' @param sample_id name of sample ID variable to use as default for selecting samples
#' @param seriate_method
#' seriation method to order phyloseq samples by similarity
#' @param tax_transform_for_ordering
#' transform taxa before ordering with ps_seriate
#' @param app_options passed to shinyApp() options argument
#' @param ... additional arguments passed to ord_plot
#'
#' @return nothing, opens default browser
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#'
#' # simple example #
#' if (interactive()) {
#'   data("enterotype")
#'   taxa_names(enterotype)[1] <- "unclassified" # replaces the "-1" taxon name
#'   ps <- tax_fix(enterotype) # remove NA taxa
#'   ord1 <- ps %>%
#'     tax_transform("identity", rank = "Genus") %>%
#'     dist_calc("bray") %>%
#'     ord_calc(method = "PCoA")
#'
#'   ord_explore(data = ord1, auto_caption = NA)
#' }
#'
#' # constrained ordination example #
#' if (interactive()) {
#'   data("dietswap", package = "microbiome")
#'
#'   # create a couple of numerical variables to use as constraints
#'   dietswap <- dietswap %>%
#'     ps_mutate(
#'       weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'       female = if_else(sex == "female", true = 1, false = 0)
#'     ) %>%
#'     tax_agg("Genus")
#'
#'   constrained_aitchison_rda <- dietswap %>%
#'     tax_transform("clr") %>%
#'     ord_calc(constraints = c("weight", "female"))
#'
#'   constrained_aitchison_rda %>%
#'     ord_explore(
#'       plot_taxa = 1:5, tax_lab_style = list(size = 3),
#'       constraint_lab_style = list(size = 4), auto_caption = 7
#'     )
#' }
#' # try changing the point colour to bmi_group or similar
#' # (style interactively! e.g. colour doesn't work as argument to ord_explore)
#'
#' # another dataset
#' if (interactive()) {
#'   microbiomeutilities::hmp2 %>%
#'     tax_fix() %>%
#'     dist_calc("aitchison") %>%
#'     ord_calc() %>%
#'     ord_explore()
#' }
#'
#' # another dataset
#' if (interactive()) {
#'   data("soilrep", package = "phyloseq")
#'   # test auto creation of SAMPLE var
#'   ps <- soilrep %>% ps_select(-Sample)
#'   # The barplot is actually quite useless with the 16000+ anonymous OTUs
#'   # in this dataset, but the 1000s of unmerged "other" categories do render
#'   phyloseq_validate(ps) %>%
#'     tax_fix() %>%
#'     dist_calc("aitchison") %>%
#'     ord_calc() %>%
#'     ord_explore()
#' }
ord_explore <- function(data,
                        sample_id = NULL, # id var name for data_id ggiraph
                        seriate_method = "OLO_ward", # ordering samples
                        tax_transform_for_ordering = "identity", # samples
                        app_options = list(launch.browser = TRUE), # shinyApp()
                        ...) {
  # SETUP -------------------------------------------------------------------

  # widths of plots including space for legends, in inches
  p_width <- c(7, 9)

  # create a SAMPLE id variable
  data$ps <- ps_mutate(data$ps, SAMPLE = phyloseq::sample_names(data$ps))

  # get info about input data to initialise settings modal choices
  info <- list(
    rank = info_get(data)[["tax_agg"]],
    trans = info_get(data)[["tax_transform"]],
    scale = info_get(data)[["tax_scale"]],
    dist = info_get(data)[["distMethod"]],
    ord = info_get(data)[["ordMethod"]],
    constraints = info_get(data)[["constraints"]],
    conditions = info_get(data)[["conditions"]]
  )

  ps <- ps_get(data)
  samdat <- methods::as(phyloseq::sample_data(ps), "data.frame")

  # get list of certain types of variables for populating selectize lists
  numerical_vars <- colnames(
    samdat[, sapply(X = samdat, function(x) !is.character(x) & !is.factor(x))]
  )
  categorical_vars <-
    colnames(samdat[, sapply(samdat, function(x) !is.numeric(x))])

  message("To stop the app: Click red stop button or hit Esc in the console")

  # APP ---------------------------------------------------------------------

  # Define UI for application that draws ord_plot and comp_barplot
  ui <-
    shiny::fluidPage(
      title = "ord_explore",
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "body {
              line-height: 1;
            }
            /* allow dropdown menus to show in split layout input blocks */
            .shiny-split-layout > div {
              overflow: visible;
            }
            .selectize-input, .irs, .form-control {
              font-size: 12px;
            }
            "
          )
        )
      ),
      shiny::sidebarLayout(
        position = "left",
        ## inputs UI ----------------------------------------------------------
        sidebarPanel = shiny::sidebarPanel(
          width = 3,
          shiny::fluidRow(
            shiny::h4("Ordination options"),
            shiny::splitLayout(
              cellWidths = c("30%", "30%", "30%"),
              shiny::helpText("Dims:"),
              shiny::numericInput(
                inputId = "x1", label = NULL,
                value = 1, min = 1, max = 15, step = 1
              ),
              shiny::numericInput(
                inputId = "y1", label = NULL,
                value = 2, min = 1, max = 15, step = 1
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Click:"),
              shiny::selectInput(
                inputId = "id_var", label = NULL,
                choices = union("SAMPLE", colnames(samdat)),
                selected = c(sample_id, "SAMPLE")[[1]]
              )
            ),
            # shape
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Shape:"),
              shiny::selectInput(
                inputId = "ord_shape", label = NULL,
                choices = list(
                  Variable = phyloseq::sample_variables(ps),
                  Fixed = ggplot2_shapes()
                ),
                selected = "circle filled"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Color:"),
              shiny::selectInput(
                inputId = "ord_colour", label = NULL,
                choices = list(
                  Variable = phyloseq::sample_variables(ps),
                  Fixed = grDevices::colors(distinct = TRUE)
                ),
                selected = "gray"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Alpha:"),
              shiny::sliderInput(
                inputId = "ord_alpha", label = NULL,
                value = 0.6, min = 0, max = 1, ticks = FALSE
              )
            ),
            # size
            shiny::radioButtons(
              inputId = "size_var_type",
              label = "Point size:", inline = TRUE,
              choices = c(
                "Fixed" = "fixed",
                "Variable" = "variable"
              ),
              selected = "fixed"
            ),
            shiny::splitLayout(
              cellWidths = c("35%", "65%"),
              shiny::numericInput(
                inputId = "ord_size_num", label = NULL,
                value = 2, min = 0, step = 0.5, max = 15
              ),
              shiny::selectInput(
                inputId = "ord_size_var", label = NULL,
                choices = numerical_vars,
                selected = NULL
              )
            ),
            shiny::actionButton(
              inputId = "settings",
              class = "btn-success",
              label = "Ordination builder"
            )
          ),
          shiny::hr(),
          shiny::fluidRow(
            shiny::h4("Composition options"),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Labels:"),
              shiny::selectInput(
                inputId = "comp_label", label = NULL,
                choices = union(
                  "SAMPLE", phyloseq::sample_variables(ps)
                ),
                selected = "SAMPLE"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Facets:"),
              shiny::selectInput(
                inputId = "facet_by", label = NULL,
                choices = c("NA", categorical_vars),
                selected = "NA"
              )
            ),
            # rank
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Rank:"),
              shiny::selectInput(
                inputId = "tax_level_comp", label = NULL,
                choices = phyloseq::rank_names(ps),
                selected = utils::tail(
                  setdiff(phyloseq::rank_names(ps), "unique"),
                  n = 1
                )
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Order:"),
              shiny::selectInput(
                inputId = "tax_order", label = NULL,
                choices = c("sum", "median", "mean", "max", "var"),
                selected = "sum"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("45%", "50%"),
              shiny::helpText("N Colors:"),
              shiny::sliderInput(
                inputId = "ntaxa", label = NULL,
                min = 1, max = 39, value = 9, step = 1,
                round = TRUE, ticks = FALSE
              )
            ),
            shiny::splitLayout(
              cellWidths = c("45%", "50%"),
              shiny::helpText("N Distinct:"),
              shiny::sliderInput(
                inputId = "taxmax", label = NULL,
                min = 1, max = 500, value = 200, step = 1,
                round = TRUE, ticks = FALSE
              )
            ),
            shiny::splitLayout(
              cellWidths = c("47.5%", "47.5%"),
              shiny::checkboxInput(
                inputId = "interactive", label = "Interactive",
                value = TRUE
              ),
              shiny::checkboxInput(
                inputId = "merge_other", label = "Merge other",
                value = TRUE
              )
            )
          )
        ),
        ## plots UI ----------------------------------------------------------
        mainPanel = shiny::mainPanel(
          width = 9,
          shiny::fluidRow(
            shiny::column(
              width = 11,
              # interactive ordination plot
              ggiraph::girafeOutput(
                outputId = "ord_plot", height = "4.5in",
                width = paste0(p_width[[1]], "in")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 11, align = "center",
              shiny::tabsetPanel(
                selected = "girafe", type = "hidden", id = "tabs",
                shiny::tabPanel(
                  title = "ggplot",
                  shiny::plotOutput(
                    outputId = "comps_gg",
                    height = "5in", width = paste0(p_width[[2]], "in")
                  )
                ),
                shiny::tabPanel(
                  title = "girafe",
                  ggiraph::girafeOutput(
                    outputId = "comps_girafe",
                    height = "5in", width = paste0(p_width[[2]], "in")
                  )
                )
              )
            )
          )
        )
      )
    )

  # SERVER --------------------------------------------------------------------

  server <- function(input, output, session) {

    # settings modal ----------------------------------------------------------
    ## modal dialog -----------------------------------------------------------
    settingsModal <- shiny::reactive(
      shiny::modalDialog(
        shiny::h3("Ordination Builder"),
        shiny::helpText("Choose options to modify ordination created:"),
        shiny::selectizeInput(
          inputId = "rank", label = "Rank",
          selected = m_sel$rank,
          choices = rev(phyloseq::rank_names(ps))
        ),
        shiny::selectizeInput(
          inputId = "trans", label = "Transformation.",
          choices = c(
            "None (identity)" = "identity",
            "Centred log ratio (clr)" = "clr",
            "log10p", "compositional", "hellinger"
          ),
          selected = m_sel$trans
        ),
        shiny::selectizeInput(
          inputId = "dist", label = "Distance",
          choices = union(
            c("none", "bray", "aitchison", "euclidean"),
            unlist(phyloseq::distanceMethodList)
          ),
          selected = m_sel$distInfo
        ),
        shiny::selectizeInput(
          inputId = "method", label = "Ordination method",
          choices = c(
            "auto (chooses for you)" = "auto",
            "PCA (Principle Components Analysis)" = "PCA",
            "PCoA (Principle Co-ordinates Analysis)" = "PCoA",
            "RDA (Redundancy Analysis)" = "RDA",
            "CAP (Constrained PCoA)" = "CAP",
            "NMDS (Non-metric MDS)",
            "CCA (Canonical Correspondence Analysis)" = "CCA"
          ),
          selected = m_sel$ordInfo
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = "build", "Build")
        )
      )
    )
    ## show modal -------------------------------------------------------------
    # if data provided to ord_explore has no ordination, open settingsModal
    shiny::observeEvent(
      eventExpr = data,
      handlerExpr = {
        if (identical(ord_get(data), NULL)) {
          shiny::showModal(ui = settingsModal(), session = session)
        }
      }
    )

    # Show settings modal when button is clicked.
    shiny::observeEvent(input$settings, {
      shiny::showModal(ui = settingsModal(), session = session)
    })

    ## modal memory -----------------------------------------------------------
    ### selected --------------------------------------------------------------
    # for remembering selected and possible choices in modal selectize inputs
    # initialise selected choices
    m_sel <- shiny::reactiveValues(
      rank = if (is.na(info$rank)) "unique" else info$rank,
      trans = if (is.na(info$trans)) "identity" else info$trans,
      scale = if (is.na(info$scale)) "neither" else info$scale,
      distInfo = if (is.na(info$dist)) "none" else info$dist,
      ordInfo = if (is.na(info$ord)) "auto" else info$ord,
      const = if (is.na(info$constraints)) NULL else info$constraints,
      conds = if (is.na(info$conditions)) NULL else info$conditions,
      x1 = 1, y1 = 2
    )

    # update selected choices whenever model selection confirmed
    shiny::observeEvent(
      eventExpr = input$build,
      handlerExpr = {
        m_sel$rank <- input$rank
        m_sel$trans <- input$trans
        m_sel$distInfo <- input$dist
        m_sel$ordInfo <- input$method
        # TODO scale inputs
        # m_sel$scale <- input$scale
        # TODO constraints and conditions inputs

      }
    )


    ### choices ---------------------------------------------------------------
    # TODO adjust choice availability dynamically to prevent user errors


    # build ordination --------------------------------------------------------

    ## initialise reactive data ----------------------------------------------
    # initialise data reactive values with data provided
    v <- shiny::reactiveValues(
      dat = data, # for ord_plot
      comp_dat = ps_seriate(
        ps = ps_counts(data, warn = TRUE),
        method = seriate_method,
        tax_transform = shiny::isolate(m_sel$trans),
        dist = setdiff(
          c(shiny::isolate(m_sel$distInfo), "euclidean"), "none"
        )[[1]]
      ) # for comp_barplot (samples can be reordered)
    )

    # when build button clicked, update ordination and close modal
    shiny::observeEvent(
      eventExpr = input$build,
      handlerExpr = {
        out <- try(
          expr = {
            v$dat <- ord_build(
              data = data,
              rank = input$rank,
              trans = input$trans,
              dist = if (input$dist == "none") NA else input$dist,
              method = input$method
            )
          }
        )
        if (inherits(out, "try-error")) {
          shiny::showNotification(
            ui = "Invalid build combination, try again!",
            type = "error", session = session
          )
        } else {
          shiny::removeModal(session = session)
        }
      }
    )

    # ordination plot ---------------------------------------------------------
    output$ord_plot <- ggiraph::renderGirafe({
      if (identical(ord_get(v$dat), NULL)) {
        # placeholder instructions if data does not have ordination already
        p1 <- ggplot2::ggplot() +
          ggplot2::annotate(
            geom = "text", x = 0.1, y = 0.5, size = 3,
            label = paste0(
              "Click on the 'Ordination Builder' button.\n\n",
              "(data provided to ord_explore does not contain an ordination)"
            )
          ) +
          ggplot2::theme_void()
      } else {
        # create ordination ggplot
        p1 <- ord_plot(
          v$dat,
          axes = c(input$x1, input$y1),
          shape = input$ord_shape,
          size = size(),
          colour = input$ord_colour,
          fill = input$ord_colour,
          alpha = input$ord_alpha,
          interactive = TRUE,
          data_id = input$id_var,
          tooltip = input$id_var,
          ...
        ) +
          ggplot2::scale_shape_discrete(na.translate = TRUE, na.value = 1)

        # (blank) legend in separate plot for consistent sizing of main plot
        p1 <- legend_separate(p1, rel_widths = c(80, 20))
      }
      # make ggplot into interactive ggiraph object
      p1 <- ggiraph::girafe(
        code = print(p1),
        width_svg = p_width[[1]], height_svg = 4.5,
        options = list(
          ggiraph::opts_hover(
            css = "fill:orange;stroke:black;cursor:pointer;",
            reactive = TRUE
          ),
          ggiraph::opts_selection(
            type = "multiple", css = "stroke:black;stroke-width:2"
          ),
          ggiraph::opts_zoom(min = 0.5, max = 5)
        )
      )
      return(p1)
    })

    size <- shiny::reactive({
      switch(input$size_var_type,
        "fixed" = input$ord_size_num,
        "variable" = input$ord_size_var
      )
    })

    # composition plot --------------------------------------------------------

    ## reorder samples on ord build -------------------------------------------
    # reorder samples based on hierarchical clustering
    shiny::observeEvent(
      eventExpr = input$build,
      handlerExpr = {
        message("Ordering samples with ", seriate_method, "...")
        v$comp_dat <- ps_seriate(
          ps = v$comp_dat, method = seriate_method,
          tax_transform = m_sel$trans,
          dist = setdiff(x = c(m_sel$distInfo, "euclidean"), y = "none")[[1]]
        )
      }
    )

    ## tax order & colour -----------------------------------------------------
    # order taxa using ALL samples
    ordered_taxa <- shiny::reactive({
      shiny::showNotification(
        ui = " - Sorting taxa", duration = 2, session = session
      )
      tax_top(
        data = v$comp_dat, n = NA,
        by = get(input$tax_order),
        rank = input$tax_level_comp
      )
    })
    # set colour palette using ALL samples
    # (depends on tax level of composition plot)
    palet <- shiny::reactive({
      shiny::showNotification(
        ui = " - Setting taxa colour palette", duration = 2, session = session
      )
      ord_explore_palet_fun(
        ps = v$comp_dat, tax_level = input$tax_level_comp,
        top_by = get(input$tax_order)
      )
    })

    ## build ggplot ----------------------------------------------------------

    # make plot
    comp_plot <- shiny::reactive({
      # get ggiraph interactivity
      selected_samples <- input$ord_plot_selected
      # logical selection of kept samples
      sample_kept <-
        phyloseq::sample_data(v$comp_dat)[[input$id_var]] %in% selected_samples
      if (sum(sample_kept) >= 2) {
        # TODO fix issue that comp_barplot only works with 2+ samples
        # select samples
        ps_sel <-
          phyloseq::prune_samples(x = v$comp_dat, samples = sample_kept)

        facet_by <-
          ifelse(input$facet_by == "NA", yes = NA, no = input$facet_by)

        # plot composition of selected samples
        p_comp <- ps_sel %>%
          comp_barplot(
            n_taxa = input$ntaxa,
            tax_level = input$tax_level_comp,
            tax_order = ordered_taxa(),
            palette = palet(),
            bar_outline_colour = "black",
            sample_order = "default",
            label = input$comp_label,
            merge_other = input$merge_other,
            interactive = TRUE,
            max_taxa = input$taxmax,
            facet_by = facet_by,
            ncol = 1
          )
        p_comp <- p_comp +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme(legend.justification = "left")

      } else {
        p_comp <- ggplot2::ggplot() +
          ggplot2::annotate(
            geom = "text", x = 0.1, y = 0.5, size = 3,
            label = paste0(
              "Select 2 or more samples on the ordination plot above\n",
              "either by clicking or by using the lasso selection tool"
            )
          ) +
          ggplot2::theme_void()
      }
      p_comp
    })

    # static version comps
    output$comps_gg <- shiny::renderPlot({
      legend_separate(
        ggplot = comp_plot() + ggplot2::theme(
          legend.text = ggplot2::element_text(size = 9),
          legend.key.size = ggplot2::unit(8, "mm")
        ),
        rel_widths = c(70, 30)
      )
    })

    ## build girafe -----------------------------------------------------------
    # interactive girafe composition plot
    output$comps_girafe <- ggiraph::renderggiraph({
      gg <- legend_separate(
        ggplot = comp_plot() +
          # TODO work out how to match static/interactive sizes properly
          ggplot2::theme(
            text = ggplot2::element_text(size = 8)
          ),
        rel_widths = c(70, 30)
      )
      ggiraph::girafe(
        ggobj = gg,
        width_svg = p_width[[2]], "in", height_svg = 5,
        options = list(
          # ggiraph::opts_sizing(rescale = FALSE),
          ggiraph::opts_zoom(min = 0.5, max = 3),
          ggiraph::opts_hover(css = "fill:orange;stroke:gray;"),
          ggiraph::opts_hover_inv("opacity:0.2"),
          ggiraph::opts_selection(
            type = "single", css = "fill:orange;stroke:gray;"
          )
        )
      )
    })

    ## tabs & notifications --------------------------------------------------
    # handling (de)selection of interactive and "merge other" checkboxes
    # interactive and static plots exist on two separate tabs
    shiny::observeEvent(
      eventExpr = {
        input$interactive
        input$merge_other
      },
      handlerExpr = {
        if (isTRUE(input$interactive)) {
          shiny::updateTabsetPanel(
            session = session, inputId = "tabs", selected = "girafe"
          )
          if (isFALSE(input$merge_other)) {
            shiny::updateSliderInput(
              session = session, inputId = "taxmax", value = 40
            )
            # warn about lag with too many distinct taxa (and set maxtax = 50)
            shiny::showNotification(
              "ALERT: Max Distinct taxa reduced to 40 to avoid freezing!",
              duration = 10, closeButton = TRUE, type = "warning",
              session = session
            )
            shiny::showNotification(
              "Interactive bars lag if too many taxa and/or samples shown!",
              duration = 20, closeButton = TRUE, type = "warning",
              session = session
            )
          }
        } else {
          shiny::updateTabsetPanel(
            session = session, inputId = "tabs", selected = "ggplot"
          )
        }
      }
    )
  }
  # Run the application
  shiny::shinyApp(ui = ui, server = server, options = app_options)
}

# Create ordination from data, bundling several steps
ord_build <- function(data,
                      rank = "unique",
                      trans = "clr",
                      dist = NA,
                      method = "auto",
                      ...) {
  dat <- ps_counts(data, warn = TRUE)
  dat <- tax_agg(ps = dat, rank = rank)
  dat <- tax_transform(data = dat, transformation = trans)
  if (!identical(dist, NA)) {
    dat <- dist_calc(data = dat, dist = dist)
  }
  dat <- ord_calc(data = dat, method = method, ...)
  return(dat)
}

#' Create fixed named palette for ord_explore: tax_name = colour
#'
#' @param ps phyloseq object
#' @param tax_level tax_level at which to create fixed palette
#'
#' @return named vector of colours
#' @noRd
ord_explore_palet_fun <- function(ps,
                                  tax_level,
                                  top_by = sum,
                                  other = "grey90") {
  # set up colour palette and link to common taxa names and "Other"

  palet <- distinct_palette(n = NA)

  top_tax <- tax_top(ps, n = NA, by = top_by, rank = tax_level)

  numb <- min(length(top_tax), length(palet))

  palet <- palet[seq_len(numb)]
  names(palet) <- top_tax[seq_len(numb)]

  palet <- c(palet, c(other = other))

  return(palet)
}


#' Use cowplot to place ggplot legend alongside plot
#'
#' This aims to ensure plot sizing remains the same,
#' whether or not a legend is present
#'
#' @param ggplot a ggplot object with or without a legend
#' @param rel_widths passed to cowplot::plot_grid
#'
#' @return
#' @noRd
legend_separate <- function(ggplot, rel_widths = c(3, 1)) {
  leg <- cowplot::get_legend(ggplot)
  ggplot <- ggplot + ggplot2::theme(legend.position = "none")
  out <- cowplot::plot_grid(ggplot, leg, rel_widths = rel_widths)
  return(out)
}
