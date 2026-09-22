#' Monitoring module
#'
#' Post-deployment stability: score-level PSI by month and the
#' characteristic-stability heatmap with threshold coloring.
#'
#' @keywords internal
mod_monitoring_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header("Score-level PSI by month"),
      shiny::plotOutput(ns("psi_plot"))
    ),
    bslib::card(
      bslib::card_header(
        "Characteristic stability index (CSI) heatmap"
      ),
      shiny::plotOutput(ns("csi_heatmap")),
      bslib::card_body(
        shiny::tags$p(
          "Thresholds: below 0.10 stable, 0.10-0.25 moderate,",
          " above 0.25 significant (industry convention,",
          " Siddiqi 2017)."
        )
      )
    )
  )
}

#' @rdname mod_monitoring_ui
mod_monitoring_server <- function(id, model) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$psi_plot <- shiny::renderPlot({
      psi <- model$psi_monthly
      cols <- ifelse(psi < 0.10, "#2c7fb8",
                     ifelse(psi < 0.25, "#e6a532", "#c0392b"))
      graphics::barplot(psi, names.arg = names(psi),
                        col = cols, border = "white",
                        ylim = c(0, max(c(psi, 0.12)) * 1.15),
                        ylab = "PSI", main = NULL, las = 1)
      graphics::abline(h = 0.10, lty = 2, col = "#7f8c8d")
      graphics::abline(h = 0.25, lty = 2, col = "#c0392b")
    })
    
    output$csi_heatmap <- shiny::renderPlot({
      m <- model$csi_matrix
      nr <- nrow(m)
      graphics::layout(matrix(c(1, 2), nrow = 1), widths = c(4, 1))
      graphics::par(mar = c(6, 10, 3, 1))
      cols <- matrix(
        ifelse(m < 0.10, "#e8f1f8",
               ifelse(m < 0.25, "#f6e3c5", "#f5c6c6")),
        nrow = nr
      )
      graphics::image(x = seq_len(ncol(m)), y = seq_len(nr),
                      z = t(m), col = NULL, axes = FALSE,
                      xlab = "", ylab = "")
      for (i in seq_len(nr)) for (j in seq_len(ncol(m))) {
        graphics::rect(j - 0.5, nr - i + 0.5, j + 0.5, nr - i + 1.5,
                       col = cols[i, j], border = "white")
      }
      graphics::axis(1, at = seq_len(ncol(m)),
                     labels = colnames(m), las = 2, cex.axis = 0.85)
      graphics::axis(2, at = seq_len(nr), labels = rev(rownames(m)),
                     las = 1, cex.axis = 0.85)
      for (i in seq_len(nr)) for (j in seq_len(ncol(m))) {
        graphics::text(j, nr - i + 1, sprintf("%.2f", m[i, j]),
                       cex = 0.75)
      }
      graphics::par(mar = c(6, 1, 3, 0.5))
      graphics::plot.new()
      graphics::legend("top", fill = c("#e8f1f8", "#f6e3c5", "#f5c6c6"),
                       legend = c("< 0.10", "0.10 - 0.25", ">= 0.25"),
                       title = "CSI", bty = "n", cex = 0.8)
    })
  })
}