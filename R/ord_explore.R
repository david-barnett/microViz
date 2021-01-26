#' Interactively explore compositions of ordinated samples
#'
#' Once running: click and drag to draw box over 2 or more samples to view their compositions.
#' You can style the ordination plot points using the options on the left panel.
#'
#' @param ord list output of ord_calc
#' @param seriate_method seriation method to order phyloseq samples by similarity
#' @param ... additional arguments passed to ord_plot
#' @param tax_transform_for_ordering transform tax before ordering with ps_seriate
#'
#' @return nothing, opens html viewer
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' data("enterotype")
#'
#' ps <- enterotype
#' ord1 <- ps %>%
#'   dist_calc("bray") %>%
#'   ord_calc("PCoA")
#'
#' # ord_explore(ord = ord1)

ord_explore <- function(ord, seriate_method = "OLO_ward", tax_transform_for_ordering = "identity", ...) {
  # SETUP -------------------------------------------------------------------

  library(shiny)
  ps <- ord$ps
  ordination <- ord$ordination
  # calculate sample order based on hierarchical clustering of first PCs
  message("- ordering samples")
  ps_ordered <- ps_seriate(
    ps,
    dist = ord$info$distName, method = seriate_method,
    tax_transform = tax_transform_for_ordering
  )

  # APP ---------------------------------------------------------------------

  # Define UI for application that draws 2 linked plots
  ui <-
    shiny::fluidPage(
      shiny::sidebarLayout(
        position = "left",
        sidebarPanel = shiny::sidebarPanel(
          width = 3,
          shiny::fluidRow(
            shiny::selectInput(
              inputId = "ord_colour", label = "Point colour",
              choices = list(Variable = phyloseq::sample_variables(ps), Fixed = grDevices::colors()),
              selected = "grey0"
            ),
            # shape
            shiny::radioButtons(
              inputId = "shape_var_type", label = "Point shape is: ", inline = TRUE,
              choices = c(
                "Fixed" = "fixed",
                "Variable" = "variable"
              ),
              selected = "fixed"
            ),
            shiny::sliderInput(
              inputId = "ord_shape_num",
              label = "Fixed point shape",
              value = 19, min = 1, step = 1, max = 21
            ),
            shiny::selectInput(
              inputId = "ord_shape_var", label = "Variable point shape",
              choices = phyloseq::sample_variables(ps),
              selected = NULL
            ),
            # size
            shiny::radioButtons(
              inputId = "size_var_type", label = "Points size is: ", inline = TRUE,
              choices = c(
                "Fixed" = "fixed",
                "Variable" = "variable"
              ),
              selected = "fixed"
            ),
            shiny::sliderInput(
              inputId = "ord_size_num",
              label = "Fixed point size",
              value = 1.5, min = 0.5, step = 0.1, max = 10
            ),
            shiny::selectInput(
              inputId = "ord_size_var", label = "Variable point size",
              choices = phyloseq::sample_variables(ps),
              selected = NULL
            )
          ),
          shiny::fluidRow(
            shiny::h3("Composition options"),
            shiny::selectInput(
              inputId = "tax_level_comp", label = "Taxonomic rank",
              choices = phyloseq::rank_names(ps),
              selected = utils::tail(phyloseq::rank_names(ps), n = 1)
            ),
            shiny::sliderInput(
              inputId = "ntaxa", label = "Taxa to display",
              min = 1, max = 20, value = 9, ticks = FALSE,
              step = 1, round = TRUE
            ),
            shiny::selectInput(
              inputId = "comp_label", label = "Sample labels",
              choices = union("SAMPLE", phyloseq::sample_variables(ps)),
              selected = "SAMPLE"
            )
          )
        ),
        mainPanel = shiny::mainPanel(
          width = 9,
          shiny::fluidRow(
            shiny::column(
              width = 11,
              # ordination plot
              shiny::plotOutput(height = "450px", outputId = "ord_plot", brush = shiny::brushOpts(id = "ord_plot_brush")),
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 11,
              shiny::plotOutput(height = "450px", "compositions")
            )
          )
        )
      )
    )

  # SERVER ------------------------------------------------------------------

  server <- function(input, output) {

    # ord_plot aesthetic vars
    shape <- shiny::reactive({
      switch(input$shape_var_type,
        "fixed" = {
          input$ord_shape_num
        },
        "variable" = {
          input$ord_shape_var
        }
      )
    })
    size <- shiny::reactive({
      switch(input$size_var_type,
        "fixed" = {
          input$ord_size_num
        },
        "variable" = {
          input$ord_size_var
        }
      )
    })


    # get PC sample values (scores)
    score_df <- shiny::reactive({
      vegan::scores(ordination, display = "sites", choices = 1:2)
    })


    # ord_plot plot #
    output$ord_plot <- shiny::renderCachedPlot(
      {
        message("- making ord_plot plot")

        microViz::ord_plot(
          ord,
          shape = shape(),
          size = size(),
          colour = input$ord_colour,
          ...
        )
      },
      cacheKeyExpr = {
        list(
          shape(), size(), input$ord_colour
        )
      }
    )


    # composition plot --------------------------------------------------------

    # set colour palette (depends on tax level of ord_plot)
    palet <- shiny::reactive({
      message(" - Setting taxa colour palette")
      ord_explore_palet_fun(ps, input$tax_level_comp)
    })

    # make plot
    output$compositions <- shiny::renderPlot({

      # select points that fall within selection region
      rowindex <-
        score_df()[, 1] > input$ord_plot_brush$xmin & score_df()[, 1] < input$ord_plot_brush$xmax &
          score_df()[, 2] > input$ord_plot_brush$ymin & score_df()[, 2] < input$ord_plot_brush$ymax

      # future todo note: add any other selection criteria here above too (in combination), like antibiotics status

      # if any points have been selected
      if (sum(rowindex) > 1) {
        selected_scores_df <- score_df()[rowindex, , drop = FALSE]

        ps_sel <- phyloseq::prune_samples(x = ps_ordered, samples = rownames(selected_scores_df))

        # plot composition of selected samples
        p_comp <- ps_sel %>%
          plot_comp_bar(
            n_taxa = input$ntaxa,
            tax_level = input$tax_level_comp,
            palette = palet(),
            bar_outline_colour = "black",
            sample_order = "default",
            label = input$comp_label
          )

        p_comp + ggplot2::coord_flip() + ggplot2::labs(x = NULL, y = NULL)
      } else {
        message("Select 2 or more points")

        # return blank plot
        p_comp <- ggplot2::ggplot()
      }
    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}



#' Create fixed palette for ord_explore: tax_name = colour
#'
#' @param ps phyloseq object
#' @param tax_level tax_level at which to create fixed palette
#'
#' @return named vector of colours
ord_explore_palet_fun <- function(ps, tax_level) {
  # set up colour palette and link to common taxa names and "Other"

  palet <- distinct_palette(n = NA)

  top_tax <- ps %>% microbiome::aggregate_taxa(level = tax_level) %>%
    microbiome::top_taxa()

  numb <- min(length(top_tax), length(palet))

  palet <- palet[1:numb]
  names(palet) <- top_tax[1:numb]

  palet <- c(palet, c(Other = "#ebebeb"))

  return(palet)
}

