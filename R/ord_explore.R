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
#' data("enterotype")
#'
#' # simple example #
#' taxa_names(enterotype)[1] <- "unclassified" # replaces the "-1" taxon name
#' ps <- tax_fix(enterotype) # remove NA taxa
#' ord1 <- ps %>%
#'   dist_calc("bray") %>%
#'   ord_calc(method = "PCoA")
#'
#' # ord_explore(data = ord1, auto_caption = NA)
#'
#' # constrained ordination example #
#'
#' data("dietswap", package = "microbiome")
#'
#' # create a couple of numerical variables to use as constraints
#' dietswap <- dietswap %>%
#'   ps_mutate(
#'     weight = recode(bmi_group, obese = 3, overweight = 2, lean = 1),
#'     female = if_else(sex == "female", true = 1, false = 0)
#'   ) %>%
#'   tax_agg("Genus")
#'
#' constrained_aitchison_rda <- dietswap %>%
#'   tax_transform("clr") %>%
#'   ord_calc(constraints = c("weight", "female"))
#'
#' # constrained_aitchison_rda %>%
#' #   ord_explore(
#' #     plot_taxa = 1:5, tax_lab_style = list(size = 3),
#' #     constraint_lab_style = list(size = 4), auto_caption = 7
#' #   )
#' # try changing the point colour to bmi_group or similar
#' # (style interactively! e.g. colour doesn't work as argument to ord_explore)
#'
#' # another dataset
#' # microbiomeutilities::hmp2 %>%
#' #   tax_fix() %>%
#' #   dist_calc("aitchison") %>%
#' #   ord_calc() %>%
#' #   ord_explore()
#'
#' # another dataset
#' data("soilrep", package = "phyloseq")
#' # test auto creation of SAMPLE var
#' ps <- soilrep %>% ps_select(-Sample)
#' # The barplot is actually quite useless with the 16000+ anonymous OTUs
#' # in this dataset, but the 1000s of unmerged "other" categories do render
#' # phyloseq_validate(ps) %>%
#' #   tax_fix() %>%
#' #   dist_calc("aitchison") %>%
#' #   ord_calc() %>%
#' #   ord_explore()
ord_explore <- function(data,
                        sample_id = NULL, # id var name for data_id ggiraph
                        seriate_method = "OLO_ward", # ordering samples
                        tax_transform_for_ordering = "identity", # samples
                        app_options = list(launch.browser = TRUE), # shinyApp()
                        ...) {
  # SETUP -------------------------------------------------------------------

  # create a SAMPLE id variable
  data$ps <- ps_mutate(data$ps, SAMPLE = phyloseq::sample_names(data$ps))

  # sample data of `data` used in ord_plot (but otu table not used by ord_plot)
  ps <- ps_counts(data, warn = TRUE)

  # dist needed if a distance-based seriate_method is requested as is default
  dist <- info_get(data)[["distMethod"]]
  if (is.na(dist)) {
    dist <- "euclidean"
    tax_transform_for_ordering <- info_get(data)[["tax_transform"]]
  }
  samdat <- methods::as(phyloseq::sample_data(ps), "data.frame")

  # get list of certain types of variables for populating selectize lists
  numerical_vars <- colnames(
    samdat[, sapply(X = samdat, function(x) !is.character(x) & !is.factor(x))]
  )
  categorical_vars <-
    colnames(samdat[, sapply(samdat, function(x) !is.numeric(x))])

  # reorder samples based on hierarchical clustering
  message("Ordering samples with ", seriate_method, "...")
  ps_ordered <- ps_seriate(
    ps,
    dist = dist, method = seriate_method,
    tax_transform = tax_transform_for_ordering
  )
  message("Finished ordering.\n")
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
              line-height: 1.2;
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
        sidebarPanel = shiny::sidebarPanel(
          width = 3,
          shiny::fluidRow(
            shiny::h4("Ordination options"),
            shiny::splitLayout(
              cellWidths = c("30%", "70%"),
              shiny::helpText("Click:"),
              shiny::selectInput(
                inputId = "id_var", label = NULL,
                choices = union("SAMPLE", colnames(samdat)),
                selected = c(sample_id, "SAMPLE")[[1]]
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "70%"),
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
            shiny::sliderInput(
              inputId = "ord_alpha", label = "Point alpha",
              value = 0.6, min = 0, max = 1, ticks = FALSE
            ),
            # shape
            shiny::radioButtons(
              inputId = "shape_var_type",
              label = "Point shape:", inline = TRUE,
              choices = c(
                "Fixed" = "fixed",
                "Variable" = "variable"
              ),
              selected = "fixed"
            ),
            shiny::splitLayout(
              cellWidths = c("35%", "65%"),
              shiny::numericInput(
                inputId = "ord_shape_num", label = NULL,
                value = 21, min = 1, step = 1, max = 21
              ),
              shiny::selectInput(
                inputId = "ord_shape_var", label = NULL,
                choices = categorical_vars,
                selected = NULL
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
          ),
          shiny::fluidRow(
            shiny::h4("Composition options"),
            shiny::splitLayout(
              cellWidths = c("30%", "70%"),
              shiny::helpText("Labels:"),
              shiny::selectInput(
                inputId = "comp_label", label = NULL,
                choices = union("SAMPLE", phyloseq::sample_variables(ps)),
                selected = "SAMPLE"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "70%"),
              shiny::helpText("Facets:"),
              shiny::selectInput(
                inputId = "facet_by", label = NULL,
                choices = c("NA", categorical_vars),
                selected = "NA"
              )
            ),
            # rank
            shiny::splitLayout(
              cellWidths = c("30%", "70%"),
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
              cellWidths = c("30%", "70%"),
              shiny::helpText("Order:"),
              shiny::selectInput(
                inputId = "tax_order", label = NULL,
                choices = c("sum", "median", "mean", "max", "var"),
                selected = "sum"
              )
            ),
            shiny::splitLayout(
              cellWidths = c("45%", "55%"),
              shiny::helpText("N Colors:"),
              shiny::sliderInput(
                inputId = "ntaxa", label = NULL,
                min = 1, max = 39, value = 9, step = 1,
                round = TRUE, ticks = FALSE
              )
            ),
            shiny::splitLayout(
              cellWidths = c("45%", "55%"),
              shiny::helpText("N Distinct:"),
              shiny::sliderInput(
                inputId = "taxmax", label = NULL,
                min = 1, max = 500, value = 200, step = 1,
                round = TRUE, ticks = FALSE
              )
            ),
            shiny::checkboxInput(
              inputId = "merge_other", label = "Merge other?",
              value = TRUE
            ),
            shiny::checkboxInput(
              inputId = "interactive", label = "Interactive bars?",
              value = FALSE
            )
          )
        ),
        mainPanel = shiny::mainPanel(
          width = 9,
          shiny::fluidRow(
            shiny::column(
              width = 11,
              # interactive ordination plot
              ggiraph::girafeOutput(
                outputId = "ord_plot", height = "4.5in", width = "6in"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 11, align = "center",
              shiny::tabsetPanel(
                selected = "ggplot", type = "hidden", id = "tabs",
                shiny::tabPanel(
                  title = "ggplot",
                  shiny::plotOutput(
                    height = "5in", width = "6in", outputId = "comps_gg"
                  )
                ),
                shiny::tabPanel(
                  title = "girafe",
                  ggiraph::girafeOutput(
                    outputId = "comps_girafe", height = "5in", width = "6in"
                  )
                )
              )
            )
          )
        )
      )
    )

  # SERVER ------------------------------------------------------------------

  server <- function(input, output, session) {

    # ordination plot ---------------------------------------------------------
    output$ord_plot <- ggiraph::renderGirafe({
      p1 <- ord_plot(
        data,
        shape = shape(),
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
      p1 <- ggiraph::girafe(
        code = print(p1),
        width_svg = 6, height_svg = 4.5,
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
      p1
    })

    # ord_plot aesthetic vars
    shape <- shiny::reactive({
      switch(input$shape_var_type,
        "fixed" = input$ord_shape_num,
        "variable" = input$ord_shape_var
      )
    })
    size <- shiny::reactive({
      switch(input$size_var_type,
        "fixed" = input$ord_size_num,
        "variable" = input$ord_size_var
      )
    })

    # composition plot --------------------------------------------------------

    # static version comps
    output$comps_gg <- shiny::renderPlot({
      comp_plot() + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 9)
      )
    })

    # girafe version
    output$comps_girafe <- ggiraph::renderggiraph({
      ggiraph::girafe(
        ggobj = comp_plot() +
          # TODO work out how to match static/interactive sizes properly
          ggplot2::theme(
            text = ggplot2::element_text(size = 8)
          ),
        width_svg = 6, height_svg = 5, pointsize = 8,
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

    # make plot
    comp_plot <- shiny::reactive({
      # get ggiraph interactivity
      selected_samples <- input$ord_plot_selected
      # logical selection of kept samples
      sample_kept <-
        phyloseq::sample_data(ps_ordered)[[input$id_var]] %in% selected_samples
      if (sum(sample_kept) >= 2) {
        # TODO fix issue that comp_barplot only works with 2+ samples
        # select samples
        ps_sel <-
          phyloseq::prune_samples(x = ps_ordered, samples = sample_kept)

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
          ggplot2::labs(x = NULL, y = NULL)
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

    # order taxa using whole population
    ordered_taxa <- shiny::reactive({
      shiny::showNotification(" - Sorting taxa", duration = 2)
      tax_top(
        data = ps_ordered, n = NA,
        by = get(input$tax_order),
        rank = input$tax_level_comp
      )
    })
    # set colour palette (depends on tax level of composition plot)
    palet <- shiny::reactive({
      shiny::showNotification(" - Setting taxa colour palette", duration = 2)
      ord_explore_palet_fun(
        ps = ps_ordered, tax_level = input$tax_level_comp,
        top_by = get(input$tax_order)
      )
    })

    # message about lag with too many distinct taxa (and set maxtax = 50)
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
            shiny::showNotification(
              "ALERT: Max Distinct taxa reduced to 40 to avoid freezing!",
              duration = 10, closeButton = TRUE, type = "warning"
            )
            shiny::showNotification(
              "Interactive bars lag if too many taxa and/or samples shown!",
              duration = 20, closeButton = TRUE, type = "warning"
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
