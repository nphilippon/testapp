#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_data_explorer_server("data_explorer_1")
  mod_strategy_discovery_server("strategy_discovery_1")
  
  screen_payload <- mod_screener_server("screener_1")
  opt_payload <- mod_optimizer_server("optimizer_1", screen_payload = screen_payload)
  mod_backtester_server("backtester_1", opt_payload)
  
  observeEvent(opt_payload(), {
    req(opt_payload())
    bslib::nav_select("main_tabs", selected = "Backtester")
  })
  
  observeEvent(screen_payload(), {
    req(screen_payload())
    bslib::nav_select("main_tabs", selected = "Optimizer (Heatmap)")
  })
}
