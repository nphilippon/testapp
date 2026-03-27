#' Data Explorer UI
#' @description UI for interactive charting of historical market prices
#' @param id NA
#' @import shiny bslib
#' @export
mod_data_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      # Tickers Picker Input
      shinyWidgets::pickerInput(
        ns("tickers"), 
        shiny::tags$h5("Select Assets to Compare:"),
        choices = asset_choices,
        selected = c("SPY", "SU.TO", "CL=F"),
        multiple = TRUE,
        width = "100%",
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          `selected-text-format` = "count > 3"
        )
      ),
      
      # Date Range Buttons. status="primary" gives them a blue colour, size="sm" is short.
      shinyWidgets::radioGroupButtons(
        ns("date_range"), "Date Range", 
        choices = c("10yr", "5yr", "2yr", "1yr", "6mo", "3mo", "YTD"), 
        selected = "2yr",
        size = "sm",
        status = "primary"
      ),
      
      # Chart Type Toggle (Relative Base 100 vs Actual Price)
      shinyWidgets::switchInput(ns("chart_type"), value = FALSE, onLabel = "Relative (Base 100)", offLabel = "Actual Price", size = "normal", width = "100%")
    ),
    
    # Main Plotly Card component container
    bslib::card(
      bslib::card_header("Price Chart"),
      plotly::plotlyOutput(ns("price_chart"))
    ),
    
    # Raw Data Table Card component container
    bslib::card(
      bslib::card_header("Wide-Form Data View"),
      DT::DTOutput(ns("table"))
    )
  )
}

#' Data Explorer Server
#' @description Server logic for retrieving, formatting, and charting market data
#' @param r SrS global application reactive payload structural matrix state dictionary unconditionally
mod_data_explorer_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Dataset Generation
    dataset <- shiny::reactive({
      # Require essential inputs
      shiny::req(input$tickers, input$date_range)
      
      end_date <- Sys.Date()
      
      # Select start date based on the chosen window
      start_date <- switch(input$date_range,
                           "10yr" = end_date - lubridate::years(10),
                           "5yr" = end_date - lubridate::years(5),
                           "2yr" = end_date - lubridate::years(2),
                           "1yr" = end_date - lubridate::years(1),
                           "6mo" = seq(end_date, length.out = 2, by = "-6 months")[2],
                           "3mo" = seq(end_date, length.out = 2, by = "-3 months")[2],
                           "YTD" = as.Date(paste0(format(end_date, "%Y"), "-01-01")))
      
      data <- fetch_asset_data(input$tickers, start_date, end_date)
      
      # Pre-fill empty dataframes appropriately safely
      if (!"symbol" %in% names(data)) {
        data$symbol <- input$tickers[1]
      }
      
      # If Relative chart toggle is ON, compute base-100 returns for standard scaled comparative visualization
      if (isTRUE(input$chart_type)) {
        data <- data |> 
          dplyr::group_by(symbol) |>
          dplyr::arrange(date) |>
          dplyr::mutate(close = (close / dplyr::first(close)) * 100) |>
          dplyr::ungroup()
      }
      
      data
    })
    
    # Process price chart
    output$price_chart <- plotly::renderPlotly({
      # Ensure dataset has loaded
      shiny::req(dataset())
      dat <- dataset()
      
      y_title <- if(isTRUE(input$chart_type)) "Relative Perf (Base 100)" else "Price"
      
      # Color palette configuration. Clean, vibrant colors suited for dark mode visualization constraints.
      # Provides high contrast visibility and modern aesthetic.
      clean_colors <- c("#00d2ff", "#ff007f", "#00ff7f", "#ffcc00", "#b200ff", "#ff5500", "#00fceb")
      
      # Base Plotly line chart rendering mapping
      p <- plotly::plot_ly(data = dat, x = ~date, y = ~close, color = ~symbol, 
                           colors = clean_colors,
                           type = "scatter", mode = "lines",
                           # Line width parameterization for clean visibility
                           line = list(width = 2.5))
      
      # Applying dark-mode compliant Plotly Layout params. 
      # gridcolor = "#444444" provides a muted grey grid framework
      plotly::layout(p, 
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = y_title, showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent",
                     paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    # Process corresponding raw data datatable output
    output$table <- DT::renderDT({
      # Verify dataset existence before processing tables
      shiny::req(dataset())
      
      # Pivot to wide format (date as row, tickers as columns) using actual close prices
      wide_data <- dataset() |>
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(dplyr::desc(date))
      
      # Apply formatCurrency using DT for monetary precision presentation.
      # Currency symbol designated as "$".
      numeric_cols <- setdiff(names(wide_data), "date")
      DT::datatable(wide_data, options = list(pageLength = 15)) |>
        DT::formatCurrency(columns = numeric_cols, currency = "$", digits = 2)
    })
  })
}
