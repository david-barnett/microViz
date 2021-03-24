#' Interactively explore compositions of ordinated samples
#'
#' Once running: click and drag to draw box over 2 or more samples to view their compositions.
#' You can style the ordination plot points using the options on the left panel.
#'
#' @param ord list output of ord_calc
#' @param ps phyloseq object containing untransformed counts if needed (must otherwise be identical to ps used to make ord!)
#' @param seriate_method seriation method to order phyloseq samples by similarity
#' @param tax_transform_for_ordering transform tax before ordering with ps_seriate
#' @param ... additional arguments passed to ord_plot
#' @param sample_id id variable for ordering
#' @param tax_order
#'
#' @return nothing, opens html viewer
#' @export
#'
#' @examples
#' library(phyloseq)
#' library(dplyr)
#' data("enterotype")
#'
#' # simple example #
#' taxa_names(enterotype)[1] <- "unclassified"
#' ps <- tax_fill_unknowns(enterotype) # remove NA taxa
#' ord1 <- ps %>%
#'   dist_calc("bray") %>%
#'   ord_calc(method = "PCoA")
#'
#' # ord_explore(
#' #   ord = ord1, auto_caption = NA,
#' #   sample_id = "Sample_ID", tooltip = "Sample_ID"
#' # )
#'
#' # constrained biplot example #
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
#' # note if you want to visualise an ordination that required transformation
#' # you must take care to provide an untransformed phyloseq to ps,
#' # otherwise the compositions plotted will be also transformed!
#' # constrained_aitchison_rda %>%
#' #   ord_explore(
#' #     ps = dietswap, plot_taxa = 1:5, tax_lab_style = list(size = 3),
#' #     constraint_lab_style = list(size = 4), auto_caption = 7
#' #   )
#' # try changing the point colour to bmi_group or similar
#' # (style interactively! e.g. colour doesn't work as argument to ord_explore)
ord_explore <- function(ord,
                        sample_id, # id var name for data_id ggiraph
                        ps = NULL,
                        seriate_method = "OLO_ward", # ordering samples
                        tax_transform_for_ordering = "identity", # samples
                        tax_order = sum, # ordering taxa
                        ...) {
  # SETUP -------------------------------------------------------------------

  if (identical(ps, NULL)) ps <- ps_get(ord)
  if (inherits(ps, "ps_extra")) ps <- ps_get(ps)
  ordination <- ord_get(ord)
  dist <- info_get(ord)[["distMethod"]]
  # (needed if a distance-based seriate_method is requested, as is default)
  if (is.na(dist)) {
    dist <- "euclidean"
    tax_transform_for_ordering <- info_get(ord)[["tax_transform"]]
  }
  samdat <- phyloseq::sample_data(ps)

  # reorder samples based on hierarchical clustering
  message("- ordering samples")
  ps_ordered <- ps_seriate(
    ps,
    dist = dist, method = seriate_method,
    tax_transform = tax_transform_for_ordering
  )



  # APP ---------------------------------------------------------------------

  # Define UI for application that draws ord_plot and comp_barplot
  ui <-
    shiny::fluidPage(
      title = "ord_explore",
      shiny::tags$head(
        shiny::tags$style(
          shiny::HTML(
            "body {
              background-color: 'grey90';
            }
            * {
              font-size: 12px;
            }
            .shiny-input-container {
              color: grey10;
              background-color: 'grey85';
            }
            .selectize-input {
              height: 10px;
            }
            .irs-slider {
              height: 10px;
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
            shiny::h5("Ordination options"),
            shiny::selectInput(
              inputId = "id_var", label = "Selection group variable:",
              choices = names(samdat),
              selected = "Sample_ID"
            ),
            shiny::sliderInput(
              inputId = "ord_alpha", label = "Point alpha",
              value = 1, min = 0, max = 1, ticks = FALSE
            ),
            shiny::selectInput(
              inputId = "ord_colour", label = "Point colour",
              choices = list(
                Variable = phyloseq::sample_variables(ps),
                Fixed = grDevices::colors(distinct = TRUE)
              ),
              selected = "gray"
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
            shiny::sliderInput(
              inputId = "ord_shape_num", label = NULL,
              value = 1, min = 1, step = 1, max = 21, ticks = FALSE
            ),
            shiny::selectInput(
              inputId = "ord_shape_var", label = NULL,
              choices = phyloseq::sample_variables(ps),
              selected = NULL
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
            shiny::sliderInput(
              inputId = "ord_size_num", label = NULL,
              value = 1.5, min = 0.5, step = 0.1, max = 10, ticks = FALSE
            ),
            shiny::selectInput(
              inputId = "ord_size_var", label = NULL,
              choices = names(samdat[, sapply(X = samdat, function(x) !is.character(x) & !is.factor(x))]),
              selected = NULL
            ),
          ),
          shiny::fluidRow(
            shiny::h5("Composition options"),
            shiny::selectInput(
              inputId = "tax_level_comp", label = "Taxonomic rank",
              choices = phyloseq::rank_names(ps),
              selected = utils::tail(phyloseq::rank_names(ps), n = 1)
            ),
            shiny::sliderInput(
              inputId = "ntaxa", label = "Taxa to display",
              min = 1, max = 19, value = 9, ticks = FALSE,
              step = 1, round = TRUE
            ),
            shiny::selectInput(
              inputId = "comp_label", label = "Sample labels",
              choices = union("SAMPLE", phyloseq::sample_variables(ps)),
              selected = "SAMPLE"
            ),
            shiny::checkboxInput(
              inputId = "merge_other",
              label = "Merge other taxa?", value = TRUE
            )
          )
        ),
        mainPanel = shiny::mainPanel(
          width = 9,
          shiny::fluidRow(
            shiny::column(
              width = 11,
              # interactive ordination plot
              ggiraph::girafeOutput(outputId = "ord_plot", height = "4.5in", width = "6in")
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 11,
              shiny::plotOutput(height = "4.5in", outputId = "compositions")
            )
          )
        )
      )
    )

  # SERVER ------------------------------------------------------------------

  server <- function(input, output) {

    # ord_plot aesthetic vars
    shape <- shiny::reactive({
      switch(
        input$shape_var_type,
        "fixed" = input$ord_shape_num,
        "variable" = input$ord_shape_var
      )
    })
    size <- shiny::reactive({
      switch(
        input$size_var_type,
        "fixed" = input$ord_size_num,
        "variable" = input$ord_size_var
      )
    })

    # ordination plot ---------------------------------------------------------
    output$ord_plot <- ggiraph::renderGirafe({
      p1 <- ord_plot(
        ord,
        shape = shape(),
        size = size(),
        colour = input$ord_colour,
        alpha = input$ord_alpha,
        interactive = TRUE,
        data_id = input$id_var,
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
          )
        )
      )
      p1
    })

    # composition plot --------------------------------------------------------

    # set colour palette (depends on tax level of ord_plot)
    palet <- shiny::reactive({
      message(" - Setting taxa colour palette")
      ord_explore_palet_fun(
        ps = ps, tax_level = input$tax_level_comp, top_by = tax_order
      )
    })
    ordered_taxa <- shiny::reactive({
      message(" - Sorting taxa")
      tax_top(ps, n = NA, by = tax_order, input$tax_level_comp)
    })

    # make plot
    output$compositions <- shiny::renderPlot({

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
            merge_other = input$merge_other
          )

        p_comp <- p_comp +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = NULL)
      } else {
        p_comp <- ggplot2::ggplot() +
          ggplot2::annotate(
            geom = "text", x = 0.1, y = 0.5,
            label = paste0(
              "Select 2 or more samples on the ordination plot above\n",
              "either by clicking or using the lasso selection tool 1 or more times"
            )
          ) +
          ggplot2::theme_void()
      }
      return(p_comp)
    })
  }
  # Run the application
  shiny::shinyApp(ui = ui, server = server)
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

  palet <- palet[1:numb]
  names(palet) <- top_tax[1:numb]

  palet <- c(palet, c(other = other))

  return(palet)
}
