#' UI of the scorewright application
#'
#' @export
app_ui <- function() {
  bslib::page_navbar(
    theme = bslib::bs_theme(
      version = 5,
      bg = "#ffffff",
      fg = "#1a1a2e",
      primary = "#16537e"
    ),
    title = "Scorewright",
    bslib::nav_panel(title = "Scorecard", mod_scorecard_ui("scorecard")),
    bslib::nav_panel(title = "Validation", mod_validation_ui("validation")),
    bslib::nav_panel(title = "Monitoring", mod_monitoring_ui("monitoring")),
    bslib::nav_panel(title = "Cutoff Strategy", mod_cutoff_ui("cutoff"))
  )
}