#' Interactively explore microbial compositions of ordinated samples
#'
#' @description
#' A Shiny app used like an interactive version of `ord_plot()`.
#' You can select samples on an ordination plot and view their composition with stacked barplots.
#'
#' You can give the `ord_explore()` data argument either of the following:
#'
#' - the output of `ord_calc()` (i.e. a ps_extra with an ordination)
#' - a plain phyloseq object: `ord_explore()` will help you build an ordination
#'
#'
#' Once running:
#'
#' 1. edit the ordination if required
#' 2. style the ordination plot (e.g. choose dimensions; set colour and size; ...)
#' 3. click or use the lasso tool to select 2 or more samples to view their compositions
#' 4. style the taxonomic compositions barplot
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
#' @param data ps_extra list output of ord_calc, or phyloseq
#' @param sample_id name of sample ID variable to use as default for selecting samples
#' @param seriate_method
#' seriation method to order phyloseq samples by similarity
#' @param tax_transform_for_ordering
#' transform taxa before ordering with ps_seriate
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
#' library(phyloseq)
#' library(dplyr)
#'
#' # example of quickstart approach with interactive ordination calculation #
#' if (interactive()) {
#'   corncob::ibd_phylo %>%
#'     tax_fix() %>%
#'     ord_explore()
#' }
#'
#' # simple example with precalculated ordination #
#' if (interactive()) {
#'   data("enterotype")
#'   taxa_names(enterotype)[1] <- "unclassified" # replaces the "-1" taxon name
#'   ps <- tax_fix(enterotype) # remove NA taxa
#'   ord1 <- ps %>%
#'     tax_transform("identity", rank = "Genus") %>%
#'     dist_calc("bray") %>%
#'     ord_calc(method = "PCoA")
#'
#'   ord_explore(data = ord1, auto_caption = 6)
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
#' # another dataset, where "size" variable drives gradient on PC1
#' # (try setting size and/or alpha to correspond to "size"!)
#' # then edit the ordination to use "size" as a condition, see what happens
#' if (interactive()) {
#'   # hmp2 <- microbiomeutilities::hmp2
#'   hmp2 %>%
#'     tax_fix() %>%
#'     tax_transform(rank = "Genus", "identity") %>%
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
              shiny::helpText("Select:"),
              shiny::selectInput(
                inputId = "id_var", label = NULL, choices = init$vars$all,
                selected = c(sample_id, "SAMPLE")[[1]] # 'SAMPLE' if id = NULL
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Colour:"),
              shiny::selectInput(
                inputId = "ord_colour", label = NULL, selected = "azure4",
                choices = list(
                  Variable = init$vars$all,
                  Fixed = grDevices::colors(distinct = TRUE)
                )
              )
            ),
            # shape
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Shape:"),
              shiny::selectInput(
                inputId = "ord_shape", label = NULL, selected = "circle",
                choices = list(
                  Variable = init$vars$all,
                  Fixed = ggplot2_shapes()
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
                cellWidths = c("30%", "65%"),
                shiny::helpText("Alpha:"),
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
            )
          ),
          shiny::hr(),
          ### composition inputs ----------------------------------------------
          shiny::fluidRow(
            shiny::h4("Composition options"),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Labels:"),
              shiny::selectInput(
                inputId = "comp_label", label = NULL, selected = "SAMPLE",
                choices = init$vars$all
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Facets:"),
              shiny::selectInput(
                inputId = "facet_by", label = NULL, selected = "NA",
                choices = union("NA", init$vars$cat)
              )
            ),
            # rank
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Rank:"),
              shiny::selectInput(
                inputId = "tax_level_comp", label = NULL,
                choices = init$ranks, selected = init$info$rank
              )
            ),
            shiny::splitLayout(
              cellWidths = c("30%", "65%"),
              shiny::helpText("Order:"),
              shiny::selectInput(
                inputId = "tax_order", label = NULL, selected = "sum",
                choices = c("sum", "median", "mean", "max", "var"),
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
                cellWidths = c("45%", "50%"),
                shiny::helpText("N Distinct:"),
                shiny::sliderInput(
                  inputId = "taxmax", label = NULL,
                  min = 1, max = 500, value = 100, step = 1,
                  round = TRUE, ticks = FALSE
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
    # TODO
    codeModal <- shiny::reactive(
      shiny::modalDialog(
        easyClose = TRUE,
        shiny::h3(shiny::icon("code"), "Ordination plot code"),
        shiny::hr(),
        shiny::helpText("Code generation feature coming soon!"),
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
          selected = m_sel$rank, choices = m_choices$rank
        ),
        shiny::selectizeInput(
          inputId = "trans", label = "Taxa transformation",
          choices = m_choices$trans, selected = m_sel$trans
        ),
        shiny::selectizeInput(
          inputId = "dist", label = "Distance / Dissimilarity",
          choices = m_choices$distInfo, selected = m_sel$distInfo
        ),
        shiny::checkboxInput(
          inputId = "concons", label = "Constrain or condition ordination?",
          value = m_sel$concons
        ),
        shiny::conditionalPanel(
          condition = "input.concons == true",
          shiny::selectizeInput(
            inputId = "const", label = "Constraints", multiple = TRUE,
            choices = m_choices$const, selected = m_sel$const,
            options = list(placeholder = "numeric vars?")
          ),
          shiny::selectizeInput(
            inputId = "conds", label = "Conditions", multiple = TRUE,
            choices = m_choices$conds, selected = m_sel$conds,
            options = list(placeholder = "numeric vars?")
          )
        ),
        shiny::selectizeInput(
          inputId = "method", label = "Ordination method",
          choices = m_choices$ordInfo, selected = m_sel$ordInfo
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
    m_sel <- shiny::reactiveValues(
      rank = init$info$rank, trans = init$info$trans,
      # scale = if (is.na(info$scale)) "neither" else info$scale,
      distInfo = init$info$dist, ordInfo = init$info$ord,
      const = init$info$constraints, conds = init$info$conditions,
      concons = init$info$concons # constrained or conditioned (checkbox)
    )

    #### update on build ------------------------------------------------------
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
        m_sel$const <- input$const
        m_sel$conds <- input$conds
        m_sel$concons <- input$concons
        # update default choice of taxonomic rank for composition plot
        shiny::updateSelectizeInput(
          session = session, inputId = "tax_level_comp", selected = input$rank
        )
      }
    )

    ### choices ---------------------------------------------------------------

    #### initialise choices ---------------------------------------------------
    m_choices <- shiny::reactiveValues(
      rank = rev(init$ranks), trans = trans_choices(type = "all"),
      # scale = # TODO
      distInfo = dist_choices(init$data, type = "all"),
      ordInfo = ord_choices(type = "all"),
      const = init$vars$num, conds = init$vars$num
    )

    #### update choices -------------------------------------------------------

    # modify ordination choices if distance or constraints/conds change
    shiny::observeEvent(
      ignoreInit = TRUE,
      eventExpr = {
        input$dist
        input$concons
      },
      handlerExpr = {
        x <-
          if (input$dist == "none") {
            if (isTRUE(input$concons)) {
              ord_choices(c("noDist", "constrained"))
            } else {
              ord_choices(c("noDist", "unconstrained"))
            }
          } else {
            if (isTRUE(input$concons)) {
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
    v <- shiny::reactiveValues(
      dat = init$data, # for ord_plot
      comp_dat = ps_seriate( # for comp_barplot (samples can be reordered)
        ps = ps_counts(init$data, warn = TRUE),
        method = seriate_method,
        tax_transform = shiny::isolate(m_sel$trans),
        dist = setdiff(
          c(shiny::isolate(m_sel$distInfo), "euclidean"), "none"
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
            v$dat <- ord_build(
              data = init$data,
              rank = input$rank,
              trans = input$trans,
              dist = if (input$dist == "none") NA else input$dist,
              method = input$method,
              constraints = input$const,
              conditions = input$conds
            )
          }
        )
        if (inherits(out, "try-error")) {
          shiny::showNotification(
            ui = "Invalid combination of options: try again!",
            type = "error", session = session
          )
        } else {
          shiny::removeModal(session = session)
          shiny::showNotification(
            ui = "Reordering samples for barplot", type = "warning"
          )
          v$comp_dat <- ps_seriate(
            ps = v$comp_dat, method = seriate_method,
            tax_transform = m_sel$trans,
            dist = setdiff(x = c(m_sel$distInfo, "euclidean"), y = "none")[[1]]
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
        v$dat <- init$data
        shiny::showNotification(
          ui = "Reordering samples for barplot", type = "warning"
        )
        # for comp_barplot (samples can be reordered)
        v$comp_dat <- ps_seriate(
          ps = ps_counts(init$data, warn = TRUE),
          method = seriate_method,
          tax_transform = init$info$trans,
          # get current distance, if not "none", else use euclidean
          dist = setdiff(
            c(shiny::isolate(m_sel$distInfo), "euclidean"), "none"
          )[[1]]
        )
        shiny::removeModal(session = session)
      }
    )

    # ordination plot ---------------------------------------------------------
    output$ord_plot <- ggiraph::renderGirafe({
      if (identical(ord_get(v$dat), NULL)) {
        # placeholder instructions if data does not have ordination already
        p1 <- ggmessage(paste0(
          "Click on the 'Edit Ordination' button.\n\n",
          "(data provided to ord_explore does not contain an ordination)"
        ))
      } else if (input$x1 == input$y1) {
        p1 <- ggmessage("You must choose a different dimension for each axis")
      } else {
        # create ordination ggplot
        p1 <- ord_plot(
          v$dat,
          axes = c(input$x1, input$y1),
          shape = input$ord_shape,
          size = size(),
          colour = input$ord_colour,
          fill = input$ord_colour,
          alpha = alpha(),
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

    # barplot compositions ----------------------------------------------------

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
            interactive = TRUE,
            max_taxa = input$taxmax,
            merge_other = input$mergeOther,
            facet_by = facet_by,
            ncol = 1
          )
        p_comp <- p_comp +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme(legend.justification = "left")
      } else {
        p_comp <- ggmessage(paste0(
          "Select 2 or more samples on the ordination plot above\n",
          "either by clicking or by using the lasso selection tool"
        ))
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


# helper functions ------------------------------------------------------------

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
  info$concons <- length(c(info$constraints, info$conditions)) > 0

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

# simple helper function that takes string representing constraints or
# conditions stored in ps_extra info and splits by "+" or returns NULL if NA
read_cons <- function(cons_string) {
  if (is.na(cons_string)) {
    return(NULL)
  } else {
    return(unlist(strsplit(x = cons_string, split = "+", fixed = TRUE)))
  }
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
      geom = "text", x = 0.1, y = 0.5, size = 3,
      label = message
    ) +
    ggplot2::theme_void()
  return(plot)
}

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
  out <- setNames(names(choices_desc), choices_desc)
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
  more <- setNames(object = pdists, nm = pdists)
  all <- c(all, more[!names(more) %in% all])

  # overlapping type lists
  l <- list(
    all = names(all),
    noTree = c(paste0(c("g", "w", "", "va-w"), "unifrac"), "dpcoa")
  )
  l$noTree <- setdiff(l$all, l$tree)

  # select choices by name, with value as long description
  choices <- purrr::reduce(l[type], intersect)
  choices_desc <- all[choices]
  # flip names and values and return, ready for use as selectize input choices
  out <- setNames(names(choices_desc), choices_desc)
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
  out <- setNames(names(choices_desc), choices_desc)

  return(out)
}
