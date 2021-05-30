#' Interactively explore microbial compositions of ordinated samples
#'
#' @description
#' A Shiny app used to create and explore an interactive version of `ord_plot()`.
#' You can select samples on an ordination plot to view their composition with stacked barplots.
#'
#' The `ord_explore()` data argument takes either:
#'
#' - the output of `ord_calc()` (i.e. a ps_extra with an ordination)
#' - a plain phyloseq object: `ord_explore()` will help you build an ordination
#'
#' Once the app is running (in your browser), you can:
#'
#' 1. Create/edit the ordination if required
#'    - look at the R console error messages if your chosen options don't build
#' 2. Style the ordination plot (e.g. choose dimensions; set colour and size; ...)
#'    - Taxa loading arrows can be added only to PCA, RDA and CCA plots
#'    - Convex hulls or ellipses can only be drawn if Colour is set to a variable
#'    - To track individuals over time with the path plotter, your data MUST already be sorted by time (e.g. with ps_arrange)!
#' 3. Click on or use the lasso tool to select 2 or more samples to view their compositions
#'    - By default samples can be selected individually
#'    - Set the "Select" option to another variable to select by level of that variable
#' 4. Style the taxonomic compositions barplot
#'    - The samples are ordered using the distance method
#'    - The app may lag if you select 100s of samples and ungroup the "other" category
#'    - To avoid this lag: either reduce the number of taxa or samples, or deselect "Interactive" barplot
#' 5. Stop the app by clicking the red stop button in the R console
#'    - Closing the web browser window doesn't stop the app,
#'   (you can find the app again at the local http address shown in the R console)
#'    - Don't forget to copy the ordination plot code before you close the app
#'
#'  See the Details section for some known limitations of the app.
#'  Please report any other app problems on the microViz GitHub issues page.
#'
#' @details
#' Limitations:
#'
#' - If a "Select:" grouping variable is NA for some samples,
#' then that grouping variable cannot be used to select those samples
#' - "Shape:" can only be mapped to variables with a maximum of 5 distinct levels,
#' not including NAs. NAs in the shape variable are shown as hollow circles.
#'
#' On some web browsers, e.g. firefox, the numeric inputs' buttons are sometimes
#' hard to click.
#' As a workaround, you can click the box and type a number or use the arrow keys.
#' This problem occurs in all Shiny apps, not just microViz.
#'
#' @param data ps_extra list output of ord_calc, or phyloseq
#' @param sample_id name of sample ID variable to use as default for selecting samples
#' @param seriate_method
#' seriation method to order phyloseq samples by similarity
#' @param app_options passed to shinyApp() options argument
#' @param plot_widths
#' widths of plots in inches, including any legends
#' (first number is ordination, second is composition barplot)
#' @param ... additional arguments passed to ord_plot
#'
#' @return nothing, opens default browser
#' @export
#'
#' @examples
#' # example code only runs in interactive R session
#' if (interactive()) {
#'   library(phyloseq)
#'   library(dplyr)
#'
#'   # example of quickstart approach with interactive ordination calculation #
#'   corncob::ibd_phylo %>%
#'     tax_fix() %>%
#'     ord_explore()
#'
#'   # simple example with precalculated ordination #
#'   data("enterotype")
#'   taxa_names(enterotype)[1] <- "unclassified" # replaces the "-1" taxon name
#'   ps <- tax_fix(enterotype) # remove NA taxa
#'   ord1 <- ps %>%
#'     tax_transform("identity", rank = "Genus") %>%
#'     dist_calc("bray") %>%
#'     ord_calc(method = "PCoA")
#'
#'   ord_explore(data = ord1, auto_caption = 6)
#'
#'   # constrained ordination example #
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
#'   # label style arguments can be passed to ord_explore
#'   constrained_aitchison_rda %>%
#'     ord_explore(
#'       tax_lab_style = list(size = 3),
#'       constraint_lab_style = list(size = 4), auto_caption = 6
#'     )
#'   # Try changing the point colour to bmi_group or similar
#'   # Style points interactively!
#'   # (setting colour/shape/etc as arguments doesn't work)
#'
#'   # dietswap is actually a longitudinal dataset, with multiple samples per
#'   # subject. If we arrange by timepoint first (!!!), we can use the "paths"
#'   # additional plot layer from the ord_explore "Add:" menu to track
#'   # individual subjects over time.
#'   dietswap %>%
#'     ps_arrange(timepoint) %>%
#'     tax_fix() %>%
#'     ord_explore()
#'
#'
#'   # Another dataset, where "size" variable drives gradient on PC1
#'   # Try setting size and/or alpha to correspond to "size"!
#'   # Then edit the ordination to use "size" as a condition, see what happens
#'   # hmp2 <- microbiomeutilities::hmp2
#'   hmp2 %>%
#'     tax_fix() %>%
#'     tax_transform(rank = "Genus", "identity") %>%
#'     dist_calc("aitchison") %>%
#'     ord_calc() %>%
#'     ord_explore()
#'
#'   # another dataset
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
                        seriate_method = "Spectral", # ordering samples
                        app_options = list(launch.browser = TRUE), # shinyApp()
                        plot_widths = c(7, 9),
                        ...) {
  # SETUP -------------------------------------------------------------------

  # widths of plots including space for legends, in inches
  # TODO how to better handle this choice?
  p_width <- plot_widths # 1st is ordination, 2nd is composition

  init <- ord_explore_init(data)

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
              line-height: 1.5;
            }
            .col-sm-3 {
              max-width: 280px;
            }
            /* allow dropdown menus to show in split layout input blocks */
            .shiny-split-layout > div {
              overflow: visible;
            }
            .form-group {
              margin-bottom: 10px;
            }
            .selectize-input, .selectize-dropdown, .irs, .form-control {
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
          ### ordination inputs -----------------------------------------------
          shiny::fluidRow(
            shiny::div(
              style = "display:inline-block; width:47.5%",
              shiny::actionButton(
                inputId = "settings", icon = shiny::icon("cog"),
                label = "Edit", class = "btn-secondary", width = "100%"
              )
            ),
            shiny::div(
              style = "display:inline-block; width:47.5%",
              shiny::actionButton(
                inputId = "code", icon = shiny::icon("code"),
                label = "Code", class = "btn-secondary", width = "100%"
              )
            ),
            shiny::h4("Ordination options"),
            shiny::splitLayout(
              cellWidths = c("30%", "30%", "30%"), shiny::helpText("Dims:"),
              shiny::numericInput(
                inputId = "x1", label = NULL,
                value = 1, min = 1, max = 150, step = 1
              ),
              shiny::numericInput(
                inputId = "y1", label = NULL,
                value = 2, min = 1, max = 150, step = 1
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Select:"),
              shiny::selectInput(
                inputId = "id_var", label = NULL, choices = init$vars$all,
                selected = c(sample_id, "SAMPLE")[[1]] # 'SAMPLE' if id = NULL
              )
            ),
            # shape
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Shape:"),
              shiny::selectInput(
                inputId = "ord_shape", label = NULL, selected = "circle",
                choices = list(
                  Variable = init$vars$all, Fixed = ggplot2_shapes()
                )
              )
            ),
            #### colour -------------------------------------------------------
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Colour:"),
              shiny::selectInput(
                inputId = "ord_colour", label = NULL, selected = "azure4",
                choices = list(
                  Variable = init$vars$all,
                  Fixed = grDevices::colors(distinct = TRUE)
                )
              )
            ),
            #### alpha --------------------------------------------------------
            shiny::splitLayout(
              cellWidths = c("50%", "45%"),
              shiny::checkboxInput(
                inputId = "alphaFixed", label = "Fix alpha", value = TRUE
              ),
              shiny::conditionalPanel(
                condition = "input.alphaFixed == true",
                shiny::numericInput(
                  inputId = "ord_alpha_num", label = NULL,
                  value = 0.5, min = 0, step = 0.05, max = 1
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.alphaFixed == false",
              shiny::splitLayout(
                cellWidths = c("30%", "65%"), shiny::helpText("Alpha:"),
                shiny::selectizeInput(
                  inputId = "ord_alpha_var", label = NULL,
                  choices = init$vars$num, selected = NULL,
                  options = list(placeholder = "numeric var?")
                )
              )
            ),
            #### size --------------------------------------------------------
            shiny::splitLayout(
              cellWidths = c("50%", "45%"),
              shiny::checkboxInput(
                inputId = "sizeFixed", label = "Fix size", value = TRUE
              ),
              shiny::conditionalPanel(
                condition = "input.sizeFixed == true",
                shiny::numericInput(
                  inputId = "ord_size_num", label = NULL,
                  value = 2, min = 0, step = 0.5, max = 15
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.sizeFixed == false",
              shiny::splitLayout(
                cellWidths = c("30%", "65%"),
                shiny::helpText("Size:"),
                shiny::selectizeInput(
                  inputId = "ord_size_var", label = NULL,
                  choices = init$vars$num, selected = NULL,
                  options = list(placeholder = "numeric var?")
                )
              )
            ),
            #### additions ----------------------------------------------------
            shiny::splitLayout(
              cellWidths = c("20%", "75%"), shiny::helpText("Add:"),
              shiny::selectInput(
                inputId = "add", label = NULL, selected = "nothing",
                choices = c(
                  "nothing",
                  "convex hulls (coloured)" = "chulls",
                  "ellipses (coloured)" = "ellipses",
                  "taxa (PCA/RDA/CCA)" = "taxa",
                  "paths (for sorted data!)" = "paths"
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.add == 'taxa'",
              shiny::splitLayout(
                cellWidths = c("50%", "45%"), shiny::helpText("N labels:"),
                shiny::numericInput(
                  inputId = "nLabels", label = NULL, value = 3,
                  min = 1, max = 25, step = 1
                )
              )
            ),
            shiny::conditionalPanel(
              condition = "input.add == 'paths'",
              shiny::splitLayout(
                cellWidths = c("40%", "55%"), shiny::helpText("Group ID:"),
                shiny::selectizeInput(
                  inputId = "pathGroupID", label = NULL,
                  choices = init$vars$all, selected = NULL
                )
              ),
              shiny::splitLayout(
                cellWidths = c("40%", "55%"), shiny::helpText("Selected:"),
                shiny::selectizeInput(
                  inputId = "pathGroupsChosen", label = NULL,
                  choices = character(0), selected = NULL, multiple = TRUE
                )
              )
            )
          ),
          shiny::hr(),
          ### composition inputs ----------------------------------------------
          shiny::fluidRow(
            shiny::h4("Composition options"),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Labels:"),
              shiny::selectInput(
                inputId = "comp_label", label = NULL, selected = "SAMPLE",
                choices = init$vars$all
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Facets:"),
              shiny::selectInput(
                inputId = "facet_by", label = NULL, selected = "NA",
                choices = union("NA", init$vars$all)
              )
            ),
            # rank
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Rank:"),
              shiny::selectInput(
                inputId = "tax_level_comp", label = NULL,
                choices = init$ranks, selected = init$info$rank
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"), shiny::helpText("Order:"),
              shiny::selectInput(
                inputId = "tax_order", label = NULL, selected = "sum",
                choices = c("sum", "median", "mean", "max", "var"),
              )
            ),
            shiny::splitLayout(
              cellWidths = c("45%", "50%"), shiny::helpText("N Colors:"),
              shiny::sliderInput(
                inputId = "ntaxa", label = NULL, min = 1, max = 39,
                value = 9, step = 1, round = TRUE, ticks = FALSE
              )
            ),
            shiny::splitLayout(
              cellWidths = c("47.5%", "47.5%"),
              shiny::checkboxInput(
                inputId = "interactive", label = "Interactive", value = TRUE
              ),
              shiny::checkboxInput(
                inputId = "mergeOther", label = "Merge other", value = TRUE
              )
            ),
            shiny::conditionalPanel(
              condition = "input.mergeOther == false", # ? false
              shiny::splitLayout(
                cellWidths = c("45%", "50%"), shiny::helpText("N Distinct:"),
                shiny::sliderInput(
                  inputId = "taxmax", label = NULL, min = 1, max = 500,
                  value = 100, step = 1, round = TRUE, ticks = FALSE
                )
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
    if (!isFALSE(init$warn)) {
      shiny::showNotification(type = "error", duration = 10, ui = init$warn)
    }

    # code modal --------------------------------------------------------------
    codeModal <- shiny::reactive(
      shiny::modalDialog(
        easyClose = TRUE,
        shiny::h3(shiny::icon("code"), "Ordination plot code"),
        shiny::hr(),
        shiny::renderPrint({
          ord_code(
            rank = ord1chosen$rank, trans = ord1chosen$trans,
            dist = ord1chosen$dist, ord = ord1chosen$ord,
            const = ord1chosen$const, conds = ord1chosen$conds,
            x = input$x1, y = input$y1, colour = input$ord_colour,
            fill = input$ord_colour, # TODO make fill configurable
            shape = input$ord_shape, alpha = alpha(), size = size(),
            plot_taxa = plot_taxa(), ellipses = ellipses(),
            chulls = chulls(), paths = paths(), shapeIsVar = shapeIsVar()
          )
        }),
        shiny::hr(),
        shiny::helpText(
          "Note: replace `your_phyloseq` with the ",
          "object you used for ord_explore's data argument"
        ),
        footer = shiny::modalButton("Close", icon = shiny::icon("times"))
      )
    )
    # Show code modal when button is clicked.
    shiny::observeEvent(input$code, {
      shiny::showModal(ui = codeModal(), session = session)
    })

    # settings modal ----------------------------------------------------------
    ## modal dialog -----------------------------------------------------------
    settingsModal <- shiny::reactive(
      shiny::modalDialog(
        shiny::h3(shiny::icon("cog"), "Edit Ordination"),
        shiny::helpText("Choose options to modify ordination created:"),
        shiny::hr(),
        shiny::selectizeInput(
          inputId = "rank", label = "Taxonomic Rank",
          selected = ord1chosen$rank, choices = ordChoices$rank
        ),
        shiny::selectizeInput(
          inputId = "trans", label = "Taxa transformation",
          choices = ordChoices$trans, selected = ord1chosen$trans
        ),
        shiny::selectizeInput(
          inputId = "dist", label = "Distance / Dissimilarity",
          choices = ordChoices$dist, selected = ord1chosen$dist
        ),
        shiny::checkboxInput(
          inputId = "isCon", label = "Constrain or condition ordination?",
          value = ord1chosen$isCon
        ),
        shiny::conditionalPanel(
          condition = "input.isCon == true",
          shiny::selectizeInput(
            inputId = "const", label = "Constraints", multiple = TRUE,
            choices = ordChoices$const, selected = ord1chosen$const,
            options = list(placeholder = "numeric vars?")
          ),
          shiny::selectizeInput(
            inputId = "conds", label = "Conditions", multiple = TRUE,
            choices = ordChoices$conds, selected = ord1chosen$conds,
            options = list(placeholder = "numeric vars?")
          )
        ),
        shiny::selectizeInput(
          inputId = "method", label = "Ordination method",
          choices = ordChoices$ord, selected = ord1chosen$ord
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel", icon = shiny::icon("times")),
          if (!identical(ord_get(init$data), NULL)) {
            shiny::actionButton(
              inputId = "originalOrd", label = "Use original ordination",
              icon = shiny::icon("history"), class = "btn-primary"
            )
          },
          shiny::actionButton(
            inputId = "build", label = "Build",
            icon = shiny::icon("play"), class = "btn-success"
          )
        )
      )
    )
    ## show modal -------------------------------------------------------------
    # if data provided to ord_explore has no ordination, open settingsModal
    shiny::observeEvent(
      eventExpr = init$data,
      handlerExpr = {
        if (identical(ord_get(init$data), NULL)) {
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
    #### initialise selected choices ------------------------------------------
    ord1chosen <- shiny::reactiveValues(
      rank = init$info$rank, trans = init$info$trans,
      # scale = if (is.na(info$scale)) "neither" else info$scale,
      dist = init$info$dist, ord = init$info$ord,
      const = init$info$constraints, conds = init$info$conditions,
      isCon = init$info$isCon # constrained or conditioned (checkbox)
    )

    #### update on build ------------------------------------------------------
    # update selected choices whenever model selection confirmed
    shiny::observeEvent(
      eventExpr = input$build,
      handlerExpr = {
        ord1chosen$rank <- input$rank
        ord1chosen$trans <- input$trans
        ord1chosen$dist <- input$dist
        ord1chosen$ord <- input$method
        # TODO scale inputs
        # ord1chosen$scale <- input$scale
        ord1chosen$const <- input$const
        ord1chosen$conds <- input$conds
        ord1chosen$isCon <- input$isCon
        # update default choice of taxonomic rank for composition plot
        shiny::updateSelectizeInput(
          session = session, inputId = "tax_level_comp", selected = input$rank
        )
      }
    )

    ### choices ---------------------------------------------------------------

    #### initialise choices ---------------------------------------------------
    ordChoices <- shiny::reactiveValues(
      rank = rev(init$ranks), trans = trans_choices(type = "all"),
      # scale = # TODO
      dist = dist_choices(init$data, type = "all"),
      ord = ord_choices(type = "all"),
      const = init$vars$num, conds = init$vars$num
    )

    #### update choices -------------------------------------------------------

    # modify ordination choices if distance or constraints/conds change
    shiny::observeEvent(
      ignoreInit = TRUE,
      eventExpr = {
        input$dist
        input$isCon
      },
      handlerExpr = {
        x <-
          if (input$dist == "none") {
            if (isTRUE(input$isCon)) {
              ord_choices(c("noDist", "constrained"))
            } else {
              ord_choices(c("noDist", "unconstrained"))
            }
          } else {
            if (isTRUE(input$isCon)) {
              ord_choices(c("dist", "constrained"))
            } else {
              ord_choices(c("dist", "unconstrained"))
            }
          }
        shiny::updateSelectizeInput(
          session = session, inputId = "method", choices = x
        )
      }
    )
    # remove aitchison distance if log10p or clr transform used
    shiny::observeEvent(
      ignoreInit = TRUE,
      eventExpr = input$trans,
      handlerExpr = {
        x <- dist_choices(init$data, type = "all")
        if (input$trans %in% trans_choices("log")) x <- x[x != "aitchison"]
        shiny::updateSelectizeInput(
          session = session, inputId = "dist", choices = x
        )
      }
    )

    # Edit Ordination --------------------------------------------------------

    ## initialise reactive data ----------------------------------------------
    # initialise data reactive values with data provided
    phylos <- shiny::reactiveValues(
      ord1 = init$data, # for ord_plot
      comps = ps_seriate( # for comp_barplot (samples can be reordered)
        ps = ps_counts(init$data, warn = TRUE),
        method = seriate_method,
        tax_transform = shiny::isolate(ord1chosen$trans),
        dist = setdiff(
          c(shiny::isolate(ord1chosen$dist), "euclidean"), "none"
        )[[1]]
      )
    )

    ## Build button event ----------------------------------------------------
    # when build button clicked, update ordination and close modal on success
    shiny::observeEvent(
      eventExpr = input$build,
      handlerExpr = {
        out <- try(
          expr = {
            phylos$ord1 <- ord_build(
              data = init$data, rank = input$rank, trans = input$trans,
              dist = if (input$dist == "none") NA else input$dist,
              method = input$method,
              constraints = input$const, conditions = input$conds
            )
          }
        )
        if (inherits(out, "try-error")) {
          shiny::showNotification(
            ui = paste(
              "Invalid combination of options: try again!",
              "See R console for error message(s)."
            ),
            type = "error", session = session
          )
        } else {
          shiny::removeModal(session = session)
          shiny::showNotification(
            ui = "Reordering samples for barplot", type = "warning"
          )
          phylos$comps <- ps_seriate(
            ps = phylos$comps, method = seriate_method,
            tax_transform = ord1chosen$trans,
            dist = setdiff(c(ord1chosen$dist, "euclidean"), "none")[[1]]
          )
        }
      }
    )

    ## Revert button event ----------------------------------------------------
    # when "use original ordination" button clicked,
    # revert to that data, calculate barplot sample order, and close modal
    shiny::observeEvent(
      eventExpr = input$originalOrd,
      handlerExpr = {
        phylos$ord1 <- init$data
        shiny::showNotification(
          ui = "Reordering samples for barplot", type = "warning"
        )
        # for comp_barplot (samples can be reordered)
        phylos$comps <- ps_seriate(
          ps = ps_counts(init$data, warn = TRUE),
          method = seriate_method, tax_transform = init$info$trans,
          # get current distance, if not "none", else use euclidean
          dist = setdiff(
            c(shiny::isolate(ord1chosen$dist), "euclidean"), "none"
          )[[1]]
        )
        shiny::removeModal(session = session)
      }
    )

    # ord plot ----------------------------------------------------------------
    output$ord_plot <- ggiraph::renderGirafe({
      # prevent execution if no axes selected
      shiny::req(input$x1, input$y1, cancelOutput = TRUE)
      p1 <- ord_ggplot(
        ord = phylos$ord1, x = input$x1, y = input$y1, shape = input$ord_shape,
        size = size(), colour = input$ord_colour, alpha = alpha(),
        id = input$id_var, plot_taxa = plot_taxa(),
        ellipses = ellipses(), chulls = chulls(), paths = paths(), ...
      )
      # (blank) legend in separate plot for consistent sizing of main plot
      p1 <- legend_separate(p1, rel_widths = c(80, 20))
      # make ggplot into interactive ggiraph object
      p1 <- ord_girafe(gg = p1, width = p_width[[1]], height = 4.5)
      return(p1)
    })

    ## arg helpers ------------------------------------------------------------
    shapeIsVar <- shiny::reactive({
      input$ord_shape %in% init$vars$all
    })
    size <- shiny::reactive({
      if (isTRUE(input$sizeFixed)) {
        input$ord_size_num
      } else {
        shiny::req(input$ord_size_var, cancelOutput = TRUE)
      }
    })
    alpha <- shiny::reactive({
      if (isTRUE(input$alphaFixed)) {
        input$ord_alpha_num
      } else {
        shiny::req(input$ord_alpha_var, cancelOutput = TRUE)
      }
    })
    plot_taxa <- shiny::reactive({
      if (input$add != "taxa") {
        FALSE
      } else {
        seq_len(input$nLabels)
      }
    })
    ellipses <- shiny::reactive({
      input$add == "ellipses" & input$ord_colour %in% init$vars$all
    })
    chulls <- shiny::reactive({
      input$add == "chulls" & input$ord_colour %in% init$vars$all
    })
    paths <- shiny::reactive({
      if (input$add == "paths" && length(input$pathGroupsChosen) > 0) {
        list(
          id_var = shiny::isolate(input$pathGroupID),
          id_values = input$pathGroupsChosen,
          colour = input$ord_colour,
          all_vars = init$vars$all
        )
      } else {
        NULL
      }
    })
    # update path group selection choices depending on ID variable selected
    shiny::observeEvent(
      eventExpr = input$pathGroupID,
      handlerExpr = {
        shiny::updateSelectizeInput(
          session = session, inputId = "pathGroupsChosen",
          choices = unique(samdat_tbl(phylos$ord1)[[input$pathGroupID]])
        )
      }
    )

    # barplot -----------------------------------------------------------------

    ## tax order & colour -----------------------------------------------------
    # order taxa using ALL samples
    ordered_taxa <- shiny::reactive({
      shiny::showNotification(
        ui = " - Sorting taxa", duration = 2, session = session
      )
      tax_top(
        data = phylos$comps, n = NA,
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
        ps = phylos$comps, tax_level = input$tax_level_comp,
        top_by = get(input$tax_order)
      )
    })

    ## build ggplot ----------------------------------------------------------

    comp_plot <- shiny::reactive({
      # make logical vector indicating whether sample is selected on ord plot
      isSampleSelected <- markSelectedSamples(
        ordSel = input$ord_plot_selected, id = input$id_var, ps = phylos$comps
      )

      # make barplot (or placeholder)
      barplot <- ggBarplot(
        selected = isSampleSelected, ps = phylos$comps,
        facet_by = input$facet_by, n_taxa = input$ntaxa,
        tax_level = input$tax_level_comp, tax_order = ordered_taxa(),
        palette = palet(), label = input$comp_label,
        max_taxa = input$taxmax, merge_other = input$mergeOther
      )
    })

    ## render ggplot ----------------------------------------------------------
    # static ggplot version of compositional barplot or placeholder
    output$comps_gg <- shiny::renderPlot({
      # tweak sizing of legend text and squares for ggplot output
      plot <- comp_plot() + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 9),
        legend.key.size = ggplot2::unit(8, "mm")
      )
      legend_separate(ggplot = plot, rel_widths = c(70, 30))
    })

    ## render girafe ----------------------------------------------------------
    # interactive girafe composition plot
    output$comps_girafe <- ggiraph::renderggiraph({
      # TODO work out how to match static/interactive sizes properly
      gg <- comp_plot() + ggplot2::theme(text = ggplot2::element_text(size = 8))
      gg <- legend_separate(ggplot = gg, rel_widths = c(70, 30))
      # make interactive html barplot
      girafeBarplot(gg = gg, width = p_width[[2]], height = 5)
    })

    ## tabs & notifications --------------------------------------------------
    # handling (de)selection of interactive and "merge other" checkboxes
    # interactive and static plots exist on two separate tabs
    shiny::observeEvent(
      eventExpr = {
        input$interactive
        input$mergeOther
      },
      handlerExpr = {
        if (isTRUE(input$interactive)) {
          shiny::updateTabsetPanel(
            session = session, inputId = "tabs", selected = "girafe"
          )
          if (isFALSE(input$mergeOther)) {
            shiny::updateSliderInput(
              session = session, inputId = "taxmax", value = 40
            )
            # warn about lag with too many distinct taxa (and set maxtax = 50)
            shiny::showNotification(
              "ALERT: Max Distinct taxa reduced to 40 to avoid freezing!",
              duration = 10, type = "warning", session = session
            )
            shiny::showNotification(
              "Interactive bars lag if too many taxa and/or samples shown!",
              duration = 20, type = "warning", session = session
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


# helper functions ------------------------------------------------------------

## ordination helpers --------------------------------------------------------

#' Handle ord_explore input data
#'
#' Take input data and return list with:
#'
#' - processed ps_extra data (added SAMPLE variable)
#' - ordination builder modal default options
#' - sample variable lists for input choices
#'
#' @param data data as passed to ord_explore
#'
#' @return a list of lists
#' @noRd
ord_explore_init <- function(data) {

  # if data is plain phyloseq, validate and convert to ps_extra
  if (methods::is(data, "phyloseq")) {
    data <- tax_transform(phyloseq_validate(data), "identity", rank = "unique")
  }

  # create a SAMPLE id variable
  data$ps <- ps_mutate(data$ps, SAMPLE = phyloseq::sample_names(data$ps))
  data <- tax_names2tt(data, colname = "unique")

  # ordination info -----------------------------------------------------------
  # get info about input data to initialise settings modal choices
  info <- list(
    rank = info_get(data)[["tax_agg"]],
    trans = info_get(data)[["tax_transform"]],
    scale = info_get(data)[["tax_scale"]],
    dist = info_get(data)[["distMethod"]],
    ord = info_get(data)[["ordMethod"]],
    constraints = read_cons(info_get(data)[["constraints"]]),
    conditions = read_cons(info_get(data)[["conditions"]])
  )
  # read_cons returns NULL if no constraints / conditions found
  info$isCon <- length(c(info$constraints, info$conditions)) > 0

  # handle missing ordination info --------------------------------------------
  # Set up a warning (and shiny notification) if information is complete
  # TODO fix ps_seriate so this info isn't necessary
  if (is.na(info$rank) || is.na(info$trans)) {
    warn <- NULL # initialise
    if (is.na(info$rank)) {
      info$rank <- "unique"
      warn <- "tax rank is 'unique'"
    }
    if (is.na(info$trans)) {
      info$trans <- "identity"
      warn <- paste(warn, "transformation is 'identity'", sep = " and ")
    }
    # add "Guessing" and end bit to warning
    warn <- paste(
      "Guessing", warn,
      "\nPlease use tax_transform and/or tax_agg to set this info explicitly!"
    )
    warning(warn)
  } else {
    warn <- FALSE
  }
  # scale = if (is.na(info$scale)) "neither" else info$scale,
  if (is.na(info$dist)) info$dist <- "none"
  if (is.na(info$ord)) info$ord <- "auto"

  # variables and ranks -------------------------------------------------------
  # get list of certain types of variables for populating selectize lists
  ps <- ps_get(data)
  ranks <- phyloseq::rank_names(ps)
  samdat <- methods::as(phyloseq::sample_data(ps), "data.frame")

  is_num <- function(x) !is.character(x) & !is.factor(x)
  is_cat <- function(x) !is.numeric(x)
  vars <- list(
    all = phyloseq::sample_variables(ps),
    num = colnames(samdat[, sapply(X = samdat, FUN = is_num)]),
    cat = colnames(samdat[, sapply(samdat, FUN = is_cat)])
  )

  out <- list(
    data = data, info = info, vars = vars, ranks = ranks, warn = warn
  )
  return(out)
}

# simple helper function that takes string representing constraints or
# conditions stored in ps_extra info and splits by "+" or returns NULL if NA
read_cons <- function(cons_string) {
  if (is.na(cons_string)) {
    return(NULL)
  } else {
    return(unlist(strsplit(x = cons_string, split = "+", fixed = TRUE)))
  }
}

# Create ordination from data, bundling several steps
ord_build <- function(data,
                      rank = "unique",
                      trans = "clr",
                      dist = NA,
                      method = "auto",
                      constraints = NULL,
                      conditions = NULL) {
  dat <- ps_counts(data, warn = TRUE)
  dat <- tax_agg(ps = dat, rank = rank)
  dat <- tax_transform(data = dat, transformation = trans)
  if (!identical(dist, NA)) {
    dat <- dist_calc(data = dat, dist = dist)
  }
  dat <- ord_calc(
    data = dat, method = method,
    constraints = constraints, conditions = conditions
  )
  return(dat)
}

# create ggplot from built ordination and aesthetic settings
ord_ggplot <- function(ord, x, y, shape, size, colour, alpha, id,
                       plot_taxa, ellipses, chulls, paths, ...) {
  if (identical(ord_get(ord), NULL)) {
    # placeholder instructions if data does not have ordination already
    p1 <- ggmessage(paste0(
      "Click on the 'Edit Ordination' button.\n\n",
      "(data provided to ord_explore does not contain an ordination)"
    ))
  } else if (x == y) {
    p1 <- ggmessage("You must choose a different dimension for each axis")
  } else {
    # create ordination ggplot
    p1 <- ord_plot(
      data = ord, axes = c(x, y), shape = shape, size = size,
      colour = colour, fill = colour, alpha = alpha, plot_taxa = plot_taxa,
      interactive = TRUE, data_id = id, tooltip = id, ...
    ) + scale_shape_girafe_filled()
    # optionally add group 95% ellipses
    if (ellipses) {
      p1 <- p1 + ggplot2::stat_ellipse(
        ggplot2::aes(colour = .data[[colour]])
      )
    }
    # optionally add group convex hulls
    if (chulls) p1 <- p1 + stat_chull(ggplot2::aes(colour = .data[[colour]]))

    # optionally add (time) paths to selected groups
    if (!identical(paths, NULL)) {
      if (paths$colour %in% paths$all_vars) {
        p1 <- add_paths(
          ggplot = p1, id_var = paths$id_var, id_values = paths$id_values,
          mapping = ggplot2::aes(colour = .data[[paths$colour]])
        )
      } else {
        p1 <- add_paths(
          ggplot = p1, id_var = paths$id_var, id_values = paths$id_values,
          colour = paths$colour
        )
      }
    }
  }
  return(p1)
}

# create girafe interactive plot from ggplot ord_plot
ord_girafe <- function(gg, width, height) {
  ggiraph::girafe(
    ggobj = gg, width_svg = width, height_svg = height, canvas_id = "svg_ord1",
    options = list(
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_hover(
        css = "stroke:black;cursor:pointer;stroke-opacity:1;stroke-width:2",
        reactive = TRUE
      ),
      ggiraph::opts_selection(
        type = "multiple",
        css = "stroke:black;stroke-width:1;stroke-opacity:1;fill-opacity:1;"
      ),
      ggiraph::opts_zoom(min = 0.5, max = 5)
    )
  )
}

### choice helpers -------------------------------------------------------------
#' Helps provide list of named choices for ordination builder modal input
#'
#' Finds intersection of type choices? (if not all)
#' e.g. constrained AND uses distance
#'
#' type options below: (provide multiple to be specific)
#' "all", "unconstrained", "constrained", "dist", "noDist"
#'
#' @param type vector specifying which type of ordinations to provide
#'
#' @return named vector of choices
#' @noRd
ord_choices <- function(type) {
  # individual options
  all <- c(
    "auto" = "auto (picks 1 of options below)",
    "PCA" = "PCA (Principle Components Analysis)",
    "PCoA" = "PCoA (Principle Co-ordinates Analysis)",
    "RDA" = "RDA (Redundancy Analysis)",
    "CAP" = "CAP (Constrained PCoA)",
    "CCA" = "CCA (Canonical Correspondence Analysis)",
    "NMDS" = "NMDS (Non-metric MDS)"
  )
  # overlapping type lists
  l <- list(
    all = names(all),
    unconstrained = c("PCA", "PCoA", "NMDS"),
    constrained = c("RDA", "CAP", "CCA"),
    dist = c("PCoA", "CAP", "NMDS"),
    noDist = c("PCA", "RDA", "CCA")
  )
  # select choices by name, with value as long description
  choices <- purrr::reduce(l[type], intersect)
  choices_desc <- all[union("auto", choices)]
  # flip names and values and return, ready for use as selectize input choices
  out <- stats::setNames(names(choices_desc), choices_desc)
  return(out)
}

# data must be ps_extra/phyloseq input as to ord_explore
# type can be all or noTree, but data without phy_tree also adds noTree to type
dist_choices <- function(data, type) {
  ps <- ps_get(data)
  # if no phylogenetic tree, can't use unifrac methods
  if (identical(phyloseq::phy_tree(ps, errorIfNULL = FALSE), NULL)) {
    type <- union(type, "noTree")
  }
  # individual options
  all <- c(
    "none" = "none (for PCA/RDA/CCA)",
    "bray" = "bray (Bray-Curtis)",
    "aitchison" = "aitchison (a.k.a. CLR & euclidean)",
    "euclidean" = "euclidean",
    "gunifrac" = "gunifrac (Generalised UniFrac, alpha=0.5)",
    "unifrac" = "unifrac (unweighted UniFrac)",
    "wunifrac" = "wunifrac (weighted UniFrac)",
    "va-wunifrac" = "va-wunifrac (variance adjusted weighted)"
  )
  # add more phyloseq dist methods
  pdists <- unlist(phyloseq::distanceMethodList)
  more <- stats::setNames(object = pdists, nm = pdists)
  all <- c(all, more[!names(more) %in% names(all)])

  # overlapping type lists
  l <- list(
    all = names(all),
    tree = c(paste0(c("g", "w", "", "va-w"), "unifrac"), "dpcoa")
  )
  l$noTree <- setdiff(l$all, l$tree)

  # select choices by name, with value as long description
  choices <- purrr::reduce(l[type], intersect)
  choices_desc <- all[choices]
  # flip names and values and return, ready for use as selectize input choices
  out <- stats::setNames(names(choices_desc), choices_desc)
  return(out)
}

# type can be all, identity, nonIdentity or log
trans_choices <- function(type) {
  # individual options
  all <- list(
    "identity" = "identity (no transformation)",
    "clr" = "clr (centred log ratio)",
    "log10p" = "log10p (log base 10 with pseudocount)",
    "compositional" = "compositional",
    "hellinger" = "hellinger"
  )
  # overlapping type lists
  l <- list(
    all = names(all),
    identity = "identity",
    nonIdentity = setdiff(names(all), "identity"),
    log = c("clr", "log10p")
  )
  # select choices by name, with value as long description
  choices <- purrr::reduce(l[type], intersect)
  choices_desc <- all[choices]
  # flip names and values and return, ready for use as selectize input choices
  out <- stats::setNames(names(choices_desc), choices_desc)

  return(out)
}

### code modal helpers --------------------------------------------------------
# generate code-styled text for reproducing ordination plot
ord_code <- function(rank, trans, dist, ord, const, conds, x, y,
                     colour, fill, shape, alpha, size,
                     plot_taxa, ellipses, chulls, paths, shapeIsVar = FALSE) {
  # prepare dist_calc line if distance needed
  dist_calc_line <- ord_code_dist(dist)

  # prepare constraint and condition argument lines if necessary
  if (!identical(const, NULL)) {
    const <- paste0(
      '  constraints = c("', paste(const, collapse = '", "'), '"),'
    )
  }
  if (!identical(conds, NULL)) {
    conds <- paste0(
      '  conditions = c("', paste(conds, collapse = '", "'), '"),'
    )
  }

  # prepare plot_taxa line if not default
  taxa_line <- paste0("  plot_taxa = 1:", length(plot_taxa), ",")
  if (isFALSE(plot_taxa)) taxa_line <- NULL

  # prepare alpha and size, which could be numeric or character
  if (!is.numeric(alpha)) alpha <- paste0('"', alpha, '"')
  if (!is.numeric(size)) size <- paste0('"', size, '"')

  # prepare add_paths code for end if necessary
  if (!identical(NULL, paths)) {
    end_lines <- ord_code_paths(paths, shapeIsVar = shapeIsVar)
  } else {
    # prepare extra stat_ellipse/chull lines for end of code if necessary
    end_lines <- ord_code_stat(
      ellipses = ellipses, chulls = chulls, colour = colour, shapeIsVar
    )
  }

  # output code-style text
  cat(
    "your_phyloseq %>%",
    paste0(
      ' tax_transform(rank = "', rank, '", transformation = "', trans, '") %>%'
    ),
    dist_calc_line,
    " ord_calc(",
    const, conds,
    paste0('  method = "', ord, '"'),
    " ) %>% ",
    " ord_plot(",
    paste0("  axes = c(", x, ", ", y, "),"), taxa_line,
    paste0('  colour = "', colour, '", fill = "', colour, '",'),
    paste0('  shape = "', shape, '", alpha = ', alpha, ","),
    paste0("  size = ", size),
    end_lines,
    sep = "\n"
  )
}

# prepare dist_calc line if distance needed
ord_code_dist <- function(dist) {
  if (dist == "none") {
    return(NULL)
  } else {
    return(paste0(' dist_calc(dist = "', dist, '") %>%'))
  }
}

# prepare stat_ellipse lines for ord_code output if necessary
ord_code_stat <- function(ellipses, chulls, colour, shapeIsVar = FALSE) {
  shapeCode <- ord_shape_scale_code(shapeIsVar)
  if (ellipses || chulls) {
    if (ellipses) stat <- " ggplot2::stat_ellipse("
    if (chulls) stat <- " stat_chull("
    colourAes <- paste0("  ggplot2::aes(colour = ", colour, ")")
    end_lines <- paste(
      sep = "\n", paste0(shapeCode, " +"), stat, colourAes, " )"
    )
  } else {
    end_lines <- shapeCode
  }
  return(end_lines)
}

# prepare add_paths code for end of ord_code if necessary
ord_code_paths <- function(paths, shapeIsVar = FALSE) {
  varArg <- paste0('  id_var = "', paths$id_var, '", ')
  valsVec <- paste0('c("', paste(paths$id_values, collapse = '", "'), '")')
  valsArg <- paste0("  id_values = ", valsVec, ",")
  if (paths$colour %in% paths$all_vars) {
    colour <- paste0("  mapping = ggplot2::aes(colour = ", paths$colour, ")")
  } else {
    colour <- paste0('  colour = "', paths$colour, '"')
  }
  shapeCode <- ord_shape_scale_code(shapeIsVar)
  end_lines <- paste(
    sep = "\n", paste0(shapeCode, " %>%"),
    " add_paths(", varArg, valsArg, colour, " )"
  )
  return(end_lines)
}

# add
ord_shape_scale_code <- function(shapeIsVar) {
  if (shapeIsVar) {
    " ) + \n scale_shape_girafe_filled()"
  } else {
    " )"
  }
}

## barplot helpers ------------------------------------------------------------

#' identify which samples in phyloseq are selected on ordination plot
#'
#' @param ordSel values of id that are selected on ordination plot
#' @param id variable used to select (sets of) samples on ordination plot
#' @param ps phyloseq object
#'
#' @return logical vector
#' @noRd
markSelectedSamples <- function(ordSel, id, ps) {
  phyloseq::sample_data(ps)[[id]] %in% ordSel
}

#' create comp_barplot or placeholder from given settings
#'
#' thin wrapper around comp_barplot itself
#'
#' @param selected logical vector indicating which samples in ps are selected
#' @param ps phyloseq
#' @param facet_by input$facet_by
#' @param n_taxa number of taxa to colour in barplot
#' @param tax_level rank at which to agg barplot
#' @param tax_order how to order taxa
#' @param palette to colour taxa
#' @param label variable name to label samples with
#' @param max_taxa maximum number of distinct taxa (relevant if merge is FALSE)
#' @param merge_other merge grey coloured taxa into "other"?
#'
#' @return ggplot barplot
#' @noRd
ggBarplot <- function(selected, ps, facet_by, n_taxa, tax_level, tax_order,
                      palette, label, max_taxa, merge_other) {
  if (sum(selected) >= 2) {
    # TODO fix issue that comp_barplot only works with 2+ samples
    # select samples
    psSelected <- phyloseq::prune_samples(x = ps, samples = selected)

    # plot composition of selected samples
    plot <- psSelected %>%
      comp_barplot(
        n_taxa = n_taxa, tax_level = tax_level, tax_order = tax_order,
        palette = palette, label = label,
        max_taxa = max_taxa, merge_other = merge_other,
        bar_outline_colour = "black", sample_order = "default",
        bar_outline_width = 0.05,
        interactive = TRUE
      )

    # add facet grid if requested
    if (!identical(facet_by, "NA")) {
      plot <- plot + ggplot2::facet_grid(
        rows = facet_by, scales = "free_y", space = "free_y"
      )
    }

    # style plot
    plot <- plot +
      ggplot2::coord_flip() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme(legend.justification = "left")
  } else {
    plot <- ggmessage(paste0(
      "Select 2 or more samples on the ordination plot above\n",
      "either by clicking or by using the lasso selection tool"
    ))
  }
  return(plot)
}

#' create girafe object from ggplot barplot
#'
#' @param gg ggplot output of ggBarplot
#' @param width output width of svg desired
#' @param height output height of svg desired
#'
#' @return htmlWidget girafe S3 list object
#' @noRd
girafeBarplot <- function(gg, width, height) {
  ggiraph::girafe(
    ggobj = gg, width_svg = width, height_svg = 5, canvas_id = "svg_barplot",
    options = list(
      # ggiraph::opts_sizing(rescale = FALSE),
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_zoom(min = 0.5, max = 3),
      ggiraph::opts_hover(css = "fill:black;stroke:black;"),
      ggiraph::opts_hover_inv("opacity:0.2"),
      ggiraph::opts_selection(
        type = "single", css = "fill:black;stroke:black;"
      )
    )
  )
}

## other helpers --------------------------------------------------------------
#' Create fixed named palette for ord_explore: tax_name = colour
#'
#' @param ps phyloseq object
#' @param tax_level tax_level at which to create fixed palette
#' @param other colour of "other" category
#'
#' @return named vector of colours
#' @noRd
ord_explore_palet_fun <- function(ps,
                                  tax_level,
                                  top_by = sum,
                                  other = "grey90") {
  # set up colour palette and link to common taxa names and "other"
  palet <- distinct_palette(n = NA)
  top_tax <- tax_top(ps, n = NA, by = top_by, rank = tax_level)
  numberOfColors <- min(length(top_tax), length(palet))
  palet <- palet[seq_len(numberOfColors)]
  names(palet) <- top_tax[seq_len(numberOfColors)]
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

#' Create simple ggplot with text annotation
#'
#' Useful as a placeholder with instructions
#'
#' @param message text to display
#' @param size size of text
#'
#' @return ggplot
#' @noRd
ggmessage <- function(message, size = 3) {
  plot <- ggplot2::ggplot() +
    ggplot2::annotate(
      geom = "text", x = 0.1, y = 0.5, size = size,
      label = message
    ) +
    ggplot2::theme_void()
  return(plot)
}
