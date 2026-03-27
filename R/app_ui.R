#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  # Wrap the entire UI definition in a shiny tagList
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Core Application UI logic using bs_theme
    bslib::page_navbar(
      # Apply preset darkly theme for a modern aesthetic
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
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "dataapp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
