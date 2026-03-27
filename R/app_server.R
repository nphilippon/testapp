#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize the global Small r Strategy explicit mapping matrix ensuring universally seamless data access safely explicitly
  r <- shiny::reactiveValues()
  
  # Inject explicit mapping boundaries to universally connect logical routing matrices natively logically robustly seamlessly systematically
  mod_data_explorer_server("data_explorer_1", r = r)
  mod_strategy_discovery_server("strategy_discovery_1", r = r)
  mod_screener_server("screener_1", r = r)
  mod_optimizer_server("optimizer_1", r = r)
  mod_backtester_server("backtester_1", r = r)
  
  # Map tab selection routing states natively explicitly
  shiny::observeEvent(r$opt_payload, {
    shiny::req(r$opt_payload)
    bslib::nav_select("main_tabs", selected = "Backtester")
  })
  
  shiny::observeEvent(r$screen_payload, {
    shiny::req(r$screen_payload)
    bslib::nav_select("main_tabs", selected = "Optimizer (Heatmap)")
  })
}
