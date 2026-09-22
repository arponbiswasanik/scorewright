#' Validation module
#'
#' Discrimination and stability outputs: Gini, KS, gains table,
#' score distribution, and decile bad rates.
#'
#' @keywords internal
mod_validation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_column_wrap(
      width = 1 / 4,
      bslib::value_box(
        title = "Gini",
        value = shiny::textOutput(ns("gini_box")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "KS statistic",
        value = shiny::textOutput(ns("ks_box")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "KS at",
        value = shiny::textOutput(ns("ksat_box")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Bad rate",
        value = shiny::textOutput(ns("badrate_box")),
        theme = "primary"
      )
    ),
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        bslib::card_header("Score distribution"),
        shiny::plotOutput(ns("score_hist"))
      ),
      bslib::card(
        bslib::card_header("Bad rate by score decile"),
        shiny::plotOutput(ns("decile_plot"))
      )
    ),
    bslib::card(
      bslib::card_header("Gains table (10 bins, riskiest first)"),
      DT::DTOutput(ns("gains_table"))
    )
  )
}

#' @rdname mod_validation_ui
mod_validation_server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$gini_box <- shiny::renderText(
      sprintf("%.3f", model$validation$gini)
    )
    output$ks_box <- shiny::renderText(
      sprintf("%.3f", model$validation$ks)
    )
    output$ksat_box <- shiny::renderText(
      sprintf("%.0f%%", 100 * model$validation$ks_at)
    )
    output$badrate_box <- shiny::renderText(
      sprintf("%.1f%%", 100 * mean(model$y))
    )
    
    output$score_hist <- shiny::renderPlot({
      graphics::hist(model$scores, breaks = 60,
                     col = "#16537e", border = "white",
                     main = NULL, xlab = "Score",
                     ylab = "Applicants")
      graphics::abline(v = 600, lty = 2, col = "#c0392b")
      graphics::text(600, graphics::par("usr")[4] * 0.9,
                     "600 @ 30:1", pos = 4, col = "#c0392b", cex = 0.9)
    })
    
    output$decile_plot <- shiny::renderPlot({
      br <- unique(stats::quantile(model$scores, seq(0, 1, 0.1)))
      dec <- cut(model$scores, breaks = br, include.lowest = TRUE)
      rates <- tapply(model$y, dec, mean)
      graphics::barplot(rev(rates), horiz = TRUE,
                        col = "#16537e", border = "white",
                        xlab = "Bad rate",
                        main = NULL,
                        las = 1, space = 0.4, cex.names = 0.55)
    })
    
    output$gains_table <- DT::renderDT({
      g <- model$validation$gains
      ## gains bins were computed on -scores (risk orientation).
      ## A risk bin "(-472,-341]" covers actual points scores
      ## [341, 472): negate AND swap the extracted bounds.
      neg_bins <- g$bin
      bnds <- regmatches(
        neg_bins,
        gregexpr("-?[0-9]+\\.?[0-9]*", neg_bins)
      )
      lo_hi <- vapply(bnds, function(b) {
        v <- as.numeric(b)
        paste0("[", format(-max(v)), ", ", format(-min(v)), "]")
      }, character(1))
      g$bin <- paste0(lo_hi, "  (riskiest first)")
      
      g$bad_rate <- round(g$bad_rate, 4)
      g$cum_bad_share <- round(g$cum_bad_share, 4)
      g$cum_good_share <- round(g$cum_good_share, 4)
      g$ks <- round(g$ks, 4)
      g$lift <- round(g$lift, 3)
      DT::datatable(
        g[, c("bin", "n", "n_bad", "n_good", "bad_rate",
              "cum_bad_share", "cum_good_share", "ks", "lift")],
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE, dom = "t"),
        colnames = c("Points-score bin (riskiest first)", "n", "Bad",
                     "Good", "Bad rate", "Cum bad share",
                     "Cum good share", "KS", "Lift")
      )
    })
  })
}