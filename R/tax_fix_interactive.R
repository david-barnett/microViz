#' Shiny app to help you use tax_fix
#'
#' @description
#' Try this app if you get errors with `tax_fix()` that are tricky to work past, or suggestions to use `tax_fix()` that you don't understand.
#'
#' The app shows you the tax_table of your data (searchable) with unknown values highlighted.
#'
#' It allows you to interactively modify minimum allowed length and to select further values to be defined as unknown.
#'
#' It will show you the correct `tax_fix()` code to copy paste into your script to reproduce the interactive filtering.
#'
#' @param data a phyloseq object
#' @param app_options options list passed to shinyApp()
#'
#' @return nothing
#' @export
#'
#' @seealso \code{\link{tax_fix}} for the non-interactive function to use in your scripts
#'
#' @examples
#' library(dplyr)
#' library(phyloseq)
#'
#' # create some problem-filled example tax_table data
#' data(dietswap, package = "microbiome")
#' ps <- dietswap
#' # create unknowns to test filling
#' tt <- tax_table(ps)
#' ntax <- ntaxa(ps)
#' set.seed(123)
#' g <- sample(1:ntax, 30)
#' f <- sample(g, 10)
#' p <- sample(f, 3)
#' tt[g, 3] <- "g__"
#' tt[f, 2] <- "f__"
#' tt[p, 1] <- "p__"
#' tt[sample(1:ntax, 10), 3] <- "unknown"
#' # create a row with only NAs
#' tt[1, ] <- NA
#' tax_table(ps) <- tax_table(tt)
#'
#' # function takes a phyloseq and shows code for how to fix the tax_table
#' # tax_fix_interactive(data = ps)
tax_fix_interactive <- function(data,
                                app_options = list(launch.browser = TRUE)) {
  message("------ tax_fix_interactive looks best fullscreen! ------")
  message("Click red stop button or hit Esc in console to stop app!")
  # https://github.com/rstudio/DT/issues/496
  options(htmlwidgets.TOJSON_ARGS = list(na = "string"))

  # get tax_table
  tt <- unclass(tt_get(data))
  tt_df <- as.data.frame.matrix(tt, optional = TRUE, make.names = FALSE)

  # find common unknown values to populate unknowns list
  unique_tt_vals <- unique.default(tt) # .default = treat as vector
  all_common_unknowns <- tax_common_unknowns(0)
  found_common_unknowns <-
    all_common_unknowns[all_common_unknowns %in% unique_tt_vals]

  # options for datatable formatting/behaviour
  DT_opts <- list(scrollCollapse = TRUE, scroller = TRUE, deferRender = TRUE)

  # UI specification ----------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::hr(),
    title = "tax_fix_interactive()",
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "body, pre {
            font-size: 12px;
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
      shiny::sidebarPanel(
        width = 2,
        shiny::h4("tax_fix_interactive"),
        shiny::hr(),
        shiny::splitLayout(
          cellWidths = c("35%", "60%"),
          shiny::h5("Min length:"),
          shiny::sliderInput(
            inputId = "min_char", label = NULL,
            min = 0, max = 10, value = 4,
            step = 1, round = TRUE, ticks = FALSE
          )
        ),
        shiny::helpText("Values shorter than min length:"),
        shiny::verbatimTextOutput("too_short"),
        shiny::br(),
        shiny::selectizeInput(
          inputId = "selected",
          label = "Select longer unknowns (type/click):",
          multiple = TRUE, choices = unique_tt_vals,
          selected = found_common_unknowns
        ),
        shiny::splitLayout(
          cellWidths = c("30%", "70%"),
          shiny::h5("Highlight:"),
          shiny::selectizeInput(
            inputId = "highlight", label = NULL,
            multiple = FALSE, choices = NULL
          )
        ),
        shiny::hr(),
        shiny::h4("tax_fix code"),
        shiny::helpText("Copy-paste this code to fix your taxa!"),
        shiny::verbatimTextOutput("code"),
        shiny::hr(),
        shiny::h4("Further options:"),
        shiny::splitLayout(
          cellWidths = c("35%", "60%"),
          shiny::h5("Suffix rank:"),
          shiny::selectInput(
            inputId = "suffix", label = NULL,
            multiple = FALSE, choices = c("classified", "current"),
            selected = "classified"
          )
        ),
        shiny::splitLayout(
          cellWidths = c("35%", "60%"),
          shiny::h5("Separator:"),
          shiny::textInput(
            inputId = "sep", label = NULL, value = " "
          )
        ),
        shiny::checkboxInput(
          inputId = "anon_uniq", label = "Keep anonymous taxa unique? ",
          value = TRUE
        ),
      ),
      shiny::mainPanel(
        width = 10,
        shiny::br(),
        shiny::tabsetPanel(
          shiny::tabPanel(
            title = "Full tax_table (check for longer unknowns to add?)",
            shiny::br(),
            DT::dataTableOutput("view")
          ),
          shiny::tabPanel(
            title = "Fixed taxa (see rows that will be altered, and to what)",
            shiny::h4("Before"),
            DT::dataTableOutput("in_tt"),
            shiny::br(),
            shiny::hr(),
            shiny::h4("After"),
            DT::dataTableOutput("out_tt")
          ),
          shiny::tabPanel(
            title = "Click here for tips!",
            shiny::htmlOutput(outputId = "tips")
          )
        )
      )
    ),
    shiny::hr()
  )

  server <- function(input, output) {

    # set highlighted row choices server side (better performance on big data)
    shiny::updateSelectizeInput(
      inputId = "highlight", server = TRUE,
      choices = c("Pick one:" = "", rownames(tt))
    )

    # main datatable ----------------------------------------------------------
    output$view <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df, class = c("compact stripe"),
          options = c(list(scrollY = "650px"), DT_opts),
          extensions = "Scroller", plugins = "scrolling",
          selection = list(
            mode = "single",
            selected = match(input$highlight, rownames(tt_df))
          )
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df), backgroundColor = bad_bg_color()
          )
      }
    )
    # before and after tables -------------------------------------------------
    output$in_tt <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df[bad_rows(), ], class = c("compact stripe"),
          options = c(list(scrollY = "250px"), DT_opts),
          extensions = "Scroller", plugins = "scrolling",
          selection = list(
            mode = "single",
            selected = match(input$highlight, rownames(tt_df[bad_rows(), ]))
          )
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df), backgroundColor = bad_bg_color()
          )
      }
    )
    output$out_tt <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df_fixed()[bad_rows(), ], class = c("compact stripe"),
          options = c(list(scrollY = "250px"), DT_opts),
          extensions = "Scroller", plugins = "scrolling",
          selection = list(
            mode = "single",
            selected = match(
              input$highlight, rownames(tt_df_fixed()[bad_rows(), ])
            )
          )
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df), backgroundColor = bad_bg_color()
          )
      }
    )
    tt_df_fixed <- shiny::reactive({
      ps_fixed <- tax_fix(
        verbose = FALSE,
        ps = ps_get(data), min_length = input$min_char,
        unknowns = input$selected, suffix_rank = input$suffix,
        anon_unique = input$anon_uniq, sep = input$sep
      )
      tt_fixed <- unclass(tt_get(ps_fixed))
      as.data.frame.matrix(tt_fixed, optional = TRUE, make.names = FALSE)
    })

    # find rows where at least one value matches
    bad_rows <- shiny::reactive({
      apply(tt, MARGIN = 1, function(x) any(x %in% bad_or_short() | is.na(x)))
    })
    bad_bg_color <- shiny::reactive({
      DT::styleEqual(
        levels = bad_or_short(),
        values = rep_len("red", length(bad_or_short()))
      )
    })
    bad_or_short <- shiny::reactive({
      union(union(too_short(), "NA"), input$selected)
    })

    # update selected ---------------------------------------------------------
    shiny::observeEvent(
      eventExpr = input$min_char,
      handlerExpr = {
        shiny::updateSelectizeInput(
          inputId = "selected",
          choices = unique_tt_vals[nchar(unique_tt_vals) >= input$min_char],
          selected = input$selected[nchar(input$selected) >= input$min_char]
        )
      }
    )

    # get short values --------------------------------------------------------
    too_short <- shiny::reactive({
      tmp <- unique_tt_vals[nchar(unique_tt_vals) < input$min_char]
      tmp[!is.na(tmp)]
    })
    # Display the too short values
    output$too_short <- shiny::renderPrint(too_short())

    # tax_fix code ------------------------------------------------------------
    # output code needed to fix tax_table
    output$code <- shiny::renderPrint({
      unknown_string <-
        paste0('c("', paste(input$selected, collapse = '", "'), '")')

      cat(
        "your_phyloseq %>%",
        " tax_fix(",
        paste0("  min_length = ", input$min_char, ","),
        paste0("  unknowns = ", unknown_string, ","),
        paste0(
          '  sep = "', input$sep, '", anon_unique = ', input$anon_uniq, ","
        ),
        paste0('  suffix_rank = "', input$suffix, '"'),
        " )",
        sep = "\n"
      )
    })

    # tips tab ----------------------------------------------------------------
    output$tips <- shiny::renderUI({
      shiny::HTML("
      <br>
      <h4>Suggested workflow:</h4>
      <ol>
       <li>Raise min length until just before a valid taxon is excluded</li>
       <li>Start typing possible unknowns into selection box e.g. 'un..'</li>
       <li>Once you have selected all unknowns you can find, change tab</li>
       <li>On the second tab, review the changes that will be made</li>
       <li>You can use the row highlighter to help orient yourself</li>
       <li>Once happy, copy the code from the box, exit the app</li>
       <li>Paste the tax_fix code into your script</li>
      </ol>
      <hr>
      <h4>Possible errors:</h4>
      <p>If on step 4 you see an error that starts:</p>
      <p>'Unknown values detected to the left of known values'</p>
      <p>
       Search for that row in the upper table and change settings to fix:
      </p>
      <ul>
        <li>
          e.g. exclude the lower rank name(s) by selecting as unknown(s).
        </li>
        <li>
          e.g. include the higher rank name(s) by removing from unknowns list.
        </li>
      </ul>
      <hr>
      ")
    })
  }

  shiny::shinyApp(ui = ui, server = server, options = app_options)
}
