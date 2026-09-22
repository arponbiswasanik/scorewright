#' Server logic of the scorewright application
#'
#' @export
app_server <- function(input, output, session) {
  
  ## Precompute the scorecard once at startup: the engine is
  ## deterministic, so the fitted model, score distribution, and
  ## validation results are static inputs to the modules.
  sc <- scorewright_model()
  
  mod_scorecard_server("scorecard", model = sc)
  mod_validation_server("validation", model = sc)
  mod_monitoring_server("monitoring", model = sc)
  mod_cutoff_server("cutoff", model = sc)
}