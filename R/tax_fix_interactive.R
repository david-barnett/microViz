#' Shiny app to help you use tax_fix
#'
#' @description
#' Try this app if you get errors with tax_fix that are tricky to work past, or suggestions to use tax_fix that you don't understand.
#'
#' The app shows you the tax_table of your data (searchable) with unknown values highlighted.
#'
#' It allows you to interactively modify minimum allowed length and to select further values to be defined as unknown.
#'
#' It will show you the right tax_fix() code to copy paste into your script to reproduce the interactive filtering.
#'
#' @param data a phyloseq object
#'
#' @return nothing
#' @export
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
tax_fix_interactive <- function(data) {

  # setup
  tt <- unclass(tt_get(data))

  # https://github.com/rstudio/DT/issues/496
  options(htmlwidgets.TOJSON_ARGS = list(na = "string"))

  tt_df <- as.data.frame.matrix(tt, optional = TRUE, make.names = FALSE)

  unique_tt_vals <- unique.default(tt) # treat as vector
  all_common_unknowns <- tax_common_unknowns(0)
  found_common_unknowns <-
    all_common_unknowns[all_common_unknowns %in% unique_tt_vals]

  ui <- shiny::fluidPage(
    title = "tax_fix_interactive",
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::h3("tax_fix_interactive"),
            shiny::hr(),
            shiny::sliderInput(
              inputId = "min_char", label = "Min length (characters):",
              min = 0, max = 10, value = 4, step = 1, round = TRUE
            ),
            shiny::helpText("Values shorter than min length:"),
            shiny::verbatimTextOutput("too_short"),
            shiny::br(),
            shiny::selectizeInput(
              inputId = "selected", label = "Select longer unknowns: ",
              multiple = TRUE, choices = unique_tt_vals,
              selected = found_common_unknowns
            ),
            shiny::helpText("Type above to choose unknown values"),
            shiny::h3("tax_fix code"),
            shiny::helpText("Copy-paste this code to fix your taxa!"),
            shiny::verbatimTextOutput("code"),
            shiny::h3("Further options:"),
            shiny::selectInput(
              inputId = "suffix", label = "Suffix rank:",
              multiple = FALSE, choices = c("classified", "current"),
              selected = "classified"
            ),
            shiny::textInput(
              inputId = "sep", label = "Separator:", value = " "
            ),
            shiny::checkboxInput(
              inputId = "anon_uniq", label = "Keep anonymous taxa unique? ",
              value = TRUE
            )
          ),
          shiny::column(
            width = 8,
            shiny::tabsetPanel(
              shiny::tabPanel(
                title = "Full tax_table",
                DT::dataTableOutput("view")
              ),
              shiny::tabPanel(
                title = "Fixed taxa (only)",
                shiny::h4("Before"),
                DT::dataTableOutput("in_tt"),
                shiny::br(),
                shiny::hr(),
                shiny::h4("After"),
                DT::dataTableOutput("out_tt")
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output) {

    # find rows where at least one value matches
    bad_rows <- shiny::reactive({
      apply(tt, 1, function(x) any(x %in% bad_or_short() | is.na(x)))
    })

    tt_df_fixed <- shiny::reactive({
      ps_fixed <- tax_fix(
        ps = ps_get(data), min_length = input$min_char,
        unknowns = input$selected, suffix_rank = input$suffix,
        anon_unique = input$anon_uniq
      )
      tt_fixed <- unclass(tt_get(ps_fixed))
      as.data.frame.matrix(tt_fixed, optional = TRUE, make.names = FALSE)
    })

    output$in_tt <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df[bad_rows(), ], class = c("compact stripe"),
          options = list(scrollY = "250px", scrollCollapse = TRUE, paging = FALSE)
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df),
            backgroundColor = DT::styleEqual(
              levels = bad_or_short(),
              values = rep_len("red", length(bad_or_short()))
            )
          )
      }
    )
    output$out_tt <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df_fixed()[bad_rows(), ], class = c("compact stripe"),
          options = list(scrollY = "250px", scrollCollapse = TRUE, paging = FALSE)
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df),
            backgroundColor = DT::styleEqual(
              levels = bad_or_short(),
              values = rep_len("red", length(bad_or_short()))
            )
          )
      }
    )

    shiny::observeEvent(
      eventExpr = input$min_char,
      handlerExpr = {
        # Can also set the label and select items
        shiny::updateSelectizeInput(
          inputId = "selected",
          choices = unique_tt_vals[nchar(unique_tt_vals) >= input$min_char],
          selected = input$selected[nchar(input$selected) >= input$min_char]
        )
      }
    )

    too_short <- shiny::reactive({
      tmp <- unique_tt_vals[nchar(unique_tt_vals) < input$min_char]
      tmp[!is.na(tmp)]
    })
    # Display the too short values
    output$too_short <- shiny::renderPrint(too_short())

    # output code needed to fix tax_table
    output$code <- shiny::renderPrint({
      unknown_string <-
        paste0('c("', paste(input$selected, collapse = '", "'), '")')

      cat(
        "your_phyloseq %>%",
        " tax_fix(",
        paste0("  min_length = ", input$min_char, ","),
        paste0("  unknowns = ", unknown_string, ","),
        paste0('  sep = " ", anon_unique = ', input$anon_uniq),
        " )",
        sep = "\n"
      )
    })

    bad_or_short <- shiny::reactive({
      union(union(too_short(), "NA"), input$selected)
    })


    # Display the datatable
    output$view <- DT::renderDataTable(
      server = TRUE, # necessary to update highlighting
      expr = {
        DT::datatable(
          data = tt_df, class = c("compact stripe"),
          options = list(scrollY = "650px", scrollCollapse = TRUE, paging = FALSE)
        ) %>%
          DT::formatStyle(
            columns = colnames(tt_df),
            backgroundColor = DT::styleEqual(
              levels = bad_or_short(),
              values = rep_len("red", length(bad_or_short()))
            )
          )
      }
    )
  }

  shiny::shinyApp(ui, server)
}
