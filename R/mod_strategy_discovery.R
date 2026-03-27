#' Strategy Discovery UI
#' @description UI for running statistical discovery and quantitative scanning
#' @param id NA
#' @import shiny bslib
#' @export
mod_strategy_discovery_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      # Header for discovery engine setup
      shiny::tags$h5("Discovery Engine"),
      
      # Picker input for the target equity
      shinyWidgets::pickerInput(ns("target_asset"), "Target Equity (Y)", choices = asset_choices, selected = "SU.TO", options = list(`live-search` = TRUE, size = 10)),
      
      # Picker input for the base commodity
      shinyWidgets::pickerInput(ns("base_asset"), "Base Commodity (X)", choices = asset_choices, selected = "CL=F", options = list(`live-search` = TRUE, size = 10)),
      
      # Group buttons for selecting the lookback parameter. status="primary" gives blue formatting.
      shinyWidgets::radioGroupButtons(
        ns("date_range"), "Lookback Period", 
        choices = c("10yr", "5yr", "2yr", "1yr", "6mo", "3mo"), 
        selected = "5yr", size = "sm", status = "primary"
      ),
      
      # Numeric input for the sliding window
      shiny::numericInput(ns("rolling_window"), "Rolling Z-Score Window (Days)", value = 60, min = 10, max = 252),
      
      # Action button to trigger the execution. class="btn-success" creates a green button.
      shiny::actionButton(ns("run_btn"), "Run Statistical Discovery", class = "btn-success w-100")
    ),
    
    # Underline style navset card to hold different charts
    bslib::navset_card_underline(
      title = "Quantitative Scanner",
      
      # Navigation panel for divergence
      bslib::nav_panel(
        "Divergence (Z-Score)",
        plotly::plotlyOutput(ns("zscore_chart"))
      ),
      
      # Navigation panel for lead-lag
      bslib::nav_panel(
        "Lead-Lag (CCF)",
        plotly::plotlyOutput(ns("ccf_chart")),
        shiny::tags$hr(),
        
        # Muted text clarification hint. class="text-muted" renders greyed-out font.
        shiny::tags$p("A positive lag (e.g., +1) means the Base Commodity leads the Target Equity by 1 day.", class="text-muted")
      )
    )
  )
}

#' Strategy Discovery Server
#' @description Server module for statistical calculations and chart rendering
#' @param r SrS standard parameter dictionary logically efficiently natively correctly
mod_strategy_discovery_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe and calculate core findings on button run
    results <- shiny::eventReactive(input$run_btn, {
      shiny::req(input$target_asset, input$base_asset, input$date_range, input$rolling_window)
      
      end_date <- Sys.Date()
      
      # Deduce proper start date offset
      start_date <- switch(input$date_range,
                           "10yr" = end_date - lubridate::years(10),
                           "5yr" = end_date - lubridate::years(5),
                           "2yr" = end_date - lubridate::years(2),
                           "1yr" = end_date - lubridate::years(1),
                           "6mo" = seq(end_date, length.out = 2, by = "-6 months")[2],
                           "3mo" = seq(end_date, length.out = 2, by = "-3 months")[2])
                           
      raw_data <- fetch_asset_data(c(input$target_asset, input$base_asset), start_date, end_date)
      
      # Reshape raw historical limits to wide view
      wide <- raw_data |> 
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(date) |>
        tidyr::drop_na()
        
      tgt <- input$target_asset
      base <- input$base_asset
      shiny::req(tgt %in% names(wide), base %in% names(wide))
      
      # Divergence Logic (Rolling Z-Score of the Price Ratio)
      wide$ratio <- wide[[tgt]] / wide[[base]]
      win <- input$rolling_window
      
      wide$roll_mean <- TTR::runMean(wide$ratio, n = win)
      wide$roll_sd <- TTR::runSD(wide$ratio, n = win)
      wide$z_score <- (wide$ratio - wide$roll_mean) / wide$roll_sd
      
      # Lead-Lag Logic (Cross-Correlation of Returns)
      wide$tgt_ret <- c(NA, diff(wide[[tgt]]) / utils::head(wide[[tgt]], -1))
      wide$base_ret <- c(NA, diff(wide[[base]]) / utils::head(wide[[base]], -1))
      
      clean_rets <- tidyr::drop_na(wide[, c("tgt_ret", "base_ret")])
      
      # Calculate empirical Cross-Correlation Function
      ccf_res <- stats::ccf(clean_rets$tgt_ret, clean_rets$base_ret, plot = FALSE, lag.max = 10, na.action = stats::na.pass)
      
      ccf_df <- data.frame(
        lag = as.numeric(ccf_res$lag),
        acf = as.numeric(ccf_res$acf)
      )
      
      list(wide_data = wide, ccf_df = ccf_df)
    }, ignoreNULL = FALSE)
    
    # Display the rolling divergence z-score chart
    output$zscore_chart <- plotly::renderPlotly({
      shiny::req(results())
      df <- results()$wide_data |> tidyr::drop_na(z_score)
      
      # Line formatting: color = "#00d2ff" provides bright cyan for darkmode
      p <- plotly::plot_ly(data = df, x = ~date) |>
        plotly::add_lines(y = ~z_score, name = "Z-Score", line = list(color = "#00d2ff", width = 2)) |>
        # Add positive deviation boundary; dash = "dash" renders a disconnected limit line, color = "#ff007f" is bright pink
        plotly::add_segments(x = min(df$date), xend = max(df$date), y = 2, yend = 2, 
                             line = list(color = "#ff007f", dash = "dash"), name = "+2 SD") |>
        # Add negative deviation boundary; dash = "dash" renders a disconnected limit line, color = "#00ff7f" is bright green
        plotly::add_segments(x = min(df$date), xend = max(df$date), y = -2, yend = -2, 
                             line = list(color = "#00ff7f", dash = "dash"), name = "-2 SD")
      
      # Apply grid formatting: gridcolor = "#444444" provides structural muted grey guidelines
      plotly::layout(p,
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = "Spread Z-Score", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent",
                     paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    # Display the CCF significance chart
    output$ccf_chart <- plotly::renderPlotly({
      shiny::req(results())
      df <- results()$ccf_df
      n <- nrow(results()$wide_data)
      sig_level <- 2 / sqrt(n)
      
      # Bar formatting: color = "#ffcc00" gives a standout yellow filling, white outlines the border
      p <- plotly::plot_ly(data = df, x = ~lag, y = ~acf, type = "bar", marker = list(color = "#ffcc00", line = list(color = "white", width = 1)), name = "Correlation") |>
        # Dash formatting: color = "#ff007f" specifies pink bounding threshold lines
        plotly::add_segments(x = min(df$lag)-1, xend = max(df$lag)+1, y = sig_level, yend = sig_level,
                             line = list(color = "#ff007f", dash = "dash"), name = "Statistical Significance") |>
        plotly::add_segments(x = min(df$lag)-1, xend = max(df$lag)+1, y = -sig_level, yend = -sig_level,
                             line = list(color = "#ff007f", dash = "dash"), showlegend = FALSE)
                             
      # Layout gridcolor = "#444444" sets deep grey background grid elements
      plotly::layout(p,
                     xaxis = list(title = "Lag (Days)", showgrid = FALSE, dtick = 1),
                     yaxis = list(title = "Cross-Correlation", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent",
                     paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
  })
}
