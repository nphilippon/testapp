#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      theme = bslib::bs_theme(preset = "darkly"),
      title = "Commodity Strategy Discovery",
      id = "main_tabs",
      
      bslib::nav_panel("Data Explorer", 
        mod_data_explorer_ui("data_explorer_1")
      ),
      bslib::nav_panel("Strategy Discovery", 
        mod_strategy_discovery_ui("strategy_discovery_1")
      ),
      bslib::nav_panel("Backtester", 
        mod_backtester_ui("backtester_1")
      ),
      bslib::nav_panel("Optimizer (Heatmap)", 
        mod_optimizer_ui("optimizer_1")
      ),
      bslib::nav_panel("Market Screener", 
        mod_screener_ui("screener_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dataapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
