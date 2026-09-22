#' Cutoff strategy module
#'
#' Interactive cutoff policy explorer: slider on the score cutoff,
#' live approval statistics and swap-set analysis against the
#' median-score baseline policy.
#'
#' @keywords internal
mod_cutoff_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        shiny::sliderInput(
          ns("cutoff"),
          "Approval cutoff (approve score >= cutoff)",
          min = 340, max = 730,
          value = 558, step = 1
        ),
        shiny::uiOutput(ns("policy_stats")),
        shiny::uiOutput(ns("swap_stats"))
      ),
      shiny::plotOutput(ns("tradeoff_plot"))
    )
  )
}

#' @rdname mod_cutoff_ui
mod_cutoff_server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stats_at <- shiny::reactive({
      cutoff_analysis(model$scores, model$y, cutoff = input$cutoff)
    })
    
    swap_at <- shiny::reactive({
      cutoff_swap(model$scores, model$y,
                  from = stats::median(model$scores),
                  to = input$cutoff)
    })
    
    output$policy_stats <- shiny::renderUI({
      ca <- stats_at()
      shiny::tagList(
        shiny::tags$h5("Policy statistics"),
        shiny::tags$p(
          "Approval rate:", sprintf("%.1f%%", 100 * ca$approval_rate),
          shiny::tags$br(),
          "Approved bad rate:", sprintf("%.2f%%",
                                        100 * ca$bad_rate_approved),
          shiny::tags$br(),
          "Bads rejected:", sprintf("%.1f%%",
                                    100 * ca$bad_rejected_share)
        )
      )
    })
    
    output$swap_stats <- shiny::renderUI({
      sw <- swap_at()
      sac <- if (sw$n_out_bad > 0) {
        round(sw$n_out_good / sw$n_out_bad, 2)
      } else NA
      shiny::tagList(
        shiny::tags$h5("Swap vs median baseline (558)"),
        shiny::tags$p(
          if (sw$n_out > 0) paste0(
            "Declined: ", sw$n_out, " loans (",
            sw$n_out_good, " good / ", sw$n_out_bad, " bad)"
          ) else paste0(
            "Approved additionally: ", sw$n_in, " loans (",
            sw$n_in_good, " good / ", sw$n_in_bad, " bad)"
          ),
          shiny::tags$br(),
          if (sw$n_out > 0) paste0(
            "Sacrifice ratio: ", sac,
            " goods declined per bad declined"
          ) else "Loosening: no goods sacrificed"
        )
      )
    })
    
    output$tradeoff_plot <- shiny::renderPlot({
      ct <- model$cutoff_curve
      graphics::par(mar = c(5, 4, 1, 4))
      graphics::plot(ct$cutoff, ct$approval_rate, type = "l",
                     col = "#16537e", lwd = 2,
                     xlab = "Cutoff score",
                     ylab = "Approval rate",
                     xlim = range(model$scores),
                     ylim = c(0, 1), las = 1)
      graphics::par(new = TRUE)
      graphics::plot(ct$cutoff, ct$bad_rate_approved, type = "l",
                     col = "#c0392b", lwd = 2, axes = FALSE,
                     xlab = "", ylab = "", ylim = c(0, 0.4))
      graphics::axis(4, las = 1, col = "#c0392b")
      graphics::mtext("Approved bad rate", side = 4, line = 2.5,
                      col = "#c0392b", cex = 0.9)
      graphics::abline(v = input$cutoff, lty = 2)
      graphics::legend("topleft", bty = "n", lwd = 2,
                       col = c("#16537e", "#c0392b"),
                       legend = c("Approval rate",
                                  "Approved bad rate"))
    })
  })
}