#' Data Explorer UI
#' @param id NA
#' @import shiny bslib
#' @export
mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      shinyWidgets::pickerInput(
        ns("tickers"), 
        tags$h5("Select Assets to Compare:"),
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
      shinyWidgets::radioGroupButtons(
        ns("date_range"), "Date Range", 
        choices = c("10yr", "5yr", "2yr", "1yr", "6mo", "3mo", "YTD"), 
        selected = "2yr",
        size = "sm",
        status = "primary"
      ),
      shinyWidgets::switchInput(ns("chart_type"), value = FALSE, onLabel = "Relative (Base 100)", offLabel = "Actual Price", size = "normal", width = "100%")
    ),
    bslib::card(
      bslib::card_header("Price Chart"),
      plotly::plotlyOutput(ns("price_chart"))
    ),
    bslib::card(
      bslib::card_header("Wide-Form Data View"),
      DT::DTOutput(ns("table"))
    )
  )
}

#' Data Explorer Server
#' @param id NA
#' @export
mod_data_explorer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    dataset <- reactive({
      req(input$tickers, input$date_range)
      
      end_date <- Sys.Date()
      start_date <- switch(input$date_range,
                           "10yr" = end_date - lubridate::years(10),
                           "5yr" = end_date - lubridate::years(5),
                           "2yr" = end_date - lubridate::years(2),
                           "1yr" = end_date - lubridate::years(1),
                           "6mo" = seq(end_date, length.out = 2, by = "-6 months")[2],
                           "3mo" = seq(end_date, length.out = 2, by = "-3 months")[2],
                           "YTD" = as.Date(paste0(format(end_date, "%Y"), "-01-01")))
      
      data <- fetch_asset_data(input$tickers, start_date, end_date)
      
      if (!"symbol" %in% names(data)) {
        data$symbol <- input$tickers[1]
      }
      
      # If Relative chart toggle is ON, compute base-100 returns
      if (isTRUE(input$chart_type)) {
        data <- data |> 
          dplyr::group_by(symbol) |>
          dplyr::arrange(date) |>
          dplyr::mutate(close = (close / dplyr::first(close)) * 100) |>
          dplyr::ungroup()
      }
      
      data
    })
    
    output$price_chart <- plotly::renderPlotly({
      req(dataset())
      dat <- dataset()
      
      y_title <- if(isTRUE(input$chart_type)) "Relative Perf (Base 100)" else "Price"
      
      # Clean, vibrant colors suited for dark mode
      clean_colors <- c("#00d2ff", "#ff007f", "#00ff7f", "#ffcc00", "#b200ff", "#ff5500", "#00fceb")
      
      p <- plotly::plot_ly(data = dat, x = ~date, y = ~close, color = ~symbol, 
                           colors = clean_colors,
                           type = "scatter", mode = "lines",
                           line = list(width = 2.5))
      
      plotly::layout(p, 
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = y_title, showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent",
                     paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    output$table <- DT::renderDT({
      req(dataset())
      # Pivot to wide format (date as row, tickers as columns) using actual close prices
      wide_data <- dataset() |>
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(dplyr::desc(date))
      
      # Apply formatCurrency using DT
      numeric_cols <- setdiff(names(wide_data), "date")
      DT::datatable(wide_data, options = list(pageLength = 15)) |>
        DT::formatCurrency(columns = numeric_cols, currency = "$", digits = 2)
    })
  })
}
