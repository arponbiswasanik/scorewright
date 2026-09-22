#' Methodology module
#'
#' In-app summary of the development methodology, verification
#' results, and sources.
#'
#' @keywords internal
mod_methodology_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_column_wrap(
    width = 1 / 2,
    bslib::card(
      bslib::card_header("Pipeline"),
      bslib::card_body(
        shiny::tags$ul(
          shiny::tags$li("Coarse classing with monotonic WOE binning ",
                         "(bin_numeric, woe_table)"),
          shiny::tags$li("Logistic estimation on WOE-transformed inputs"),
          shiny::tags$li("Points-to-score scaling: 600 at odds 30:1, ",
                         "PDO 40 (Siddiqi, 2006, ch. 5)"),
          shiny::tags$li("Validation: Gini, KS, gains/lift ",
                         "(validate_scorecard)"),
          shiny::tags$li("Monitoring: PSI/CSI with 0.10/0.25 materiality ",
                         "thresholds (psi_table)"),
          shiny::tags$li("Cutoff strategy: approval/bad-rate trade-off ",
                         "and swap sets (cutoff_analysis)")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Verification against ground truth"),
      bslib::card_body(
        shiny::tags$table(
          class = "table table-sm",
          shiny::tags$tbody(
            shiny::tags$tr(
              shiny::tags$td("IV ranking vs designed spectrum"),
              shiny::tags$td("4/4 strong predictors top-4; all nulls bottom-5")
            ),
            shiny::tags$tr(
              shiny::tags$td("Scaling reconstruction"),
              shiny::tags$td("max difference 2.3e-13")
            ),
            shiny::tags$tr(
              shiny::tags$td("Discrimination vs oracle ceiling"),
              shiny::tags$td("Gini 0.563 of 0.591 (95.3%)")
            ),
            shiny::tags$tr(
              shiny::tags$td("Blind drift detection"),
              shiny::tags$td("4/5 channels confirmed; zero false alarms")
            ),
            shiny::tags$tr(
              shiny::tags$td("Score-level masking"),
              shiny::tags$td("max PSI 0.046 while CSI fired to 0.54")
            )
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header("Sources"),
      bslib::card_body(
        shiny::tags$ul(
          shiny::tags$li("Siddiqi, N. (2006). Credit Risk Scorecards. ",
                         "Wiley."),
          shiny::tags$li("Siddiqi, N. (2017). Intelligent Credit ",
                         "Scoring. Wiley."),
          shiny::tags$li("Thomas, Edelman and Crook (2017). Credit ",
                         "Scoring and Its Applications, 2nd ed. SIAM."),
          shiny::tags$li("Lessmann et al. (2015). Benchmarking ",
                         "classification algorithms for credit scoring. ",
                         "EJOR 247(1).")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Data"),
      bslib::card_body(
        shiny::tags$p(
          "Synthetic SME development sample (5,000 loans, 24 monthly ",
          "cohorts, 2023-2024) and six scoring-period snapshots ",
          "(2026). Generated with a documented ground truth: known ",
          "signal spectrum, informative missingness, acceptance ",
          "screening, and pre-registered drift. Regenerate with ",
          "data-raw/sme_datagen.R (fixed seed)."
        )
      )
    )
  )
}

#' @rdname mod_methodology_ui
mod_methodology_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # static content only; no server logic required
    NULL
  })
}