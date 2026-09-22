#' Scorecard points table module
#'
#' Displays the fitted scorecard: per-characteristic bins, WOE,
#' points, and IV, with characteristic filtering. Counts and IV are
#' joined from the woe_tables (the scaled points table itself
#' carries only characteristic, bin, woe, points).
#'
#' @keywords internal
mod_scorecard_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shiny::selectInput(
        ns("characteristic"),
        "Characteristic",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        width = "100%"
      ),
      shiny::uiOutput(ns("summary"))
    ),
    DT::DTOutput(ns("points_table"))
  )
}

#' @rdname mod_scorecard_ui
mod_scorecard_server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    full_table <- shiny::reactive({
      pt <- model$points_table
      ## counts and IV come from the woe_tables, joined by
      ## characteristic + bin
      counts <- do.call(rbind, lapply(names(model$woe_tables), function(ch) {
        wt <- model$woe_tables[[ch]]
        data.frame(characteristic = ch, bin = wt$bin,
                   n = wt$n, n_bad = wt$n_bad, n_good = wt$n_good,
                   iv = sum(wt$iv_contribution),
                   stringsAsFactors = FALSE)
      }))
      merged <- merge(pt, counts, by = c("characteristic", "bin"),
                      sort = FALSE)
      ## restore the model's characteristic order (merge scrambles rows)
      merged[order(match(merged$characteristic,
                         names(model$woe_tables))), ]
    })
    
    shiny::observe({
      chs <- unique(full_table()$characteristic)
      shiny::updateSelectInput(session, "characteristic",
                               choices = chs, selected = chs)
    })
    
    filtered <- shiny::reactive({
      ft <- full_table()
      if (is.null(input$characteristic) || length(input$characteristic) == 0) {
        return(ft)
      }
      ft[ft$characteristic %in% input$characteristic, ]
    })
    
    output$summary <- shiny::renderUI({
      shiny::tagList(
        shiny::tags$p(
          "Scale: 600 points at odds 30:1, PDO 40",
          shiny::tags$br(),
          "Factor:", round(model$factor, 4),
          "| Offset:", round(model$offset, 4)
        )
      )
    })
    
    output$points_table <- DT::renderDT({
      ft <- filtered()
      dt <- ft[, c("characteristic", "bin", "n", "n_bad", "n_good",
                   "woe", "points", "iv")]
      dt$woe <- round(dt$woe, 4)
      dt$points <- round(dt$points, 1)
      dt$iv <- round(dt$iv, 4)
      DT::datatable(
        dt,
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE),
        colnames = c("Characteristic", "Bin", "n", "Bad", "Good",
                     "WOE", "Points", "IV")
      ) |> DT::formatStyle(
        # DT resolves style columns by the data column name,
        # not the display header set via colnames
        "points",
        background = DT::styleColorBar(dt$points, "#16537e"),
        backgroundSize = "98% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
    })
  })
}