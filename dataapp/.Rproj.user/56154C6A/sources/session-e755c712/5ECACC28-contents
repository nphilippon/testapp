#' Optimizer UI
#' @param id NA
#' @import shiny bslib
#' @export
mod_optimizer_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      tags$h5("Grid Search Setup"),
      shinyWidgets::pickerInput(ns("target_asset"), "Target Asset", choices = asset_choices, selected = "SU.TO", options = list(`live-search` = TRUE, size = 10)),
      shinyWidgets::pickerInput(ns("base_asset"), "Base Asset (if needed)", choices = asset_choices, selected = "CL=F", options = list(`live-search` = TRUE, size = 10)),
      selectInput(ns("strat"), "Strategy to Optimize", choices = c("MA Crossover", "RSI Mean Reversion", "Divergence Pairs", "Lead-Lag (1-Day Momentum)", "Rolling OLS Beta (Hedging)")),
      selectInput(ns("opt_metric"), "Optimization Target", choices = c("Sharpe Ratio", "Excess Return (vs Buy & Hold)")),
      shinyWidgets::radioGroupButtons(
        inputId = ns("lookback"),
        label = "Historical Data Window",
        choices = c("1Y", "2Y", "5Y", "10Y"),
        selected = "2Y",
        justified = TRUE,
        status = "primary"
      ),
      
      uiOutput(ns("dynamic_inputs")),
      
      actionButton(ns("run_opt"), "Generate Heatmap", class = "btn-warning w-100")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Optimization Heatmap (Annualized Sharpe Ratio)"),
        plotly::plotlyOutput(ns("heatmap"))
      ),
      uiOutput(ns("best_params"))
    )
  )
}

#' Screener Server
#' @param id NA
#' @param screen_payload Input payload reacting dynamically via market screen pipelines.
#' @export
mod_optimizer_server <- function(id, screen_payload = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (!is.null(screen_payload)) {
      observeEvent(screen_payload(), {
        val <- screen_payload()
        req(val)
        
        opt_strat <- val$strat
        if (opt_strat == "MA Crossover (Trend)") opt_strat <- "MA Crossover"
        if (opt_strat == "Divergence (Z-Score Pairs)") opt_strat <- "Divergence Pairs"
        
        shinyWidgets::updatePickerInput(session, "target_asset", selected = val$tgt)
        shinyWidgets::updatePickerInput(session, "base_asset", selected = val$base)
        updateSelectInput(session, "strat", selected = opt_strat)
      })
    }
    
    output$dynamic_inputs <- renderUI({
      strat <- input$strat
      
      if (strat == "MA Crossover") {
        p1_label <- "Fast MA Window (X-Axis)"
        p1_min_val <- 10; p1_max_val <- 50; p1_step_val <- 10
        p2_label <- "Slow MA Window (Y-Axis)"
        p2_min_val <- 100; p2_max_val <- 200; p2_step_val <- 20
      } else if (strat == "RSI Mean Reversion") {
        p1_label <- "RSI Lookback Window (X-Axis)"
        p1_min_val <- 10; p1_max_val <- 30; p1_step_val <- 2
        p2_label <- "Oversold Threshold (Y-Axis)"
        p2_min_val <- 10; p2_max_val <- 40; p2_step_val <- 5
      } else if (strat == "Divergence Pairs") {
        p1_label <- "Rolling Z-Score Window (X-Axis)"
        p1_min_val <- 20; p1_max_val <- 120; p1_step_val <- 20
        p2_label <- "Z-Score Threshold (Y-Axis)"
        p2_min_val <- 1.0; p2_max_val <- 3.0; p2_step_val <- 0.5
      } else if (strat == "Lead-Lag (1-Day Momentum)") {
        p1_label <- "Long Trigger (%) (X-Axis)"
        p1_min_val <- 0.5; p1_max_val <- 3.0; p1_step_val <- 0.5
        p2_label <- "Short Trigger (%) (Y-Axis)"
        p2_min_val <- 0.5; p2_max_val <- 3.0; p2_step_val <- 0.5
      } else if (strat == "Rolling OLS Beta (Hedging)") {
        p1_label <- "Beta Window (X-Axis)"
        p1_min_val <- 20; p1_max_val <- 60; p1_step_val <- 10
        p2_label <- "Alpha Z-Score (Y-Axis)"
        p2_min_val <- 1.0; p2_max_val <- 3.0; p2_step_val <- 0.5
      }
      
      tagList(
        tags$hr(),
        tags$h6(p1_label, style = "color: #00d2ff;"),
        bslib::layout_column_wrap(
          width = 1/3,
          numericInput(ns("p1_min"), "Min", p1_min_val),
          numericInput(ns("p1_max"), "Max", p1_max_val),
          numericInput(ns("p1_step"), "Step", p1_step_val)
        ),
        tags$hr(),
        tags$h6(p2_label, style = "color: #00ff7f;"),
        bslib::layout_column_wrap(
          width = 1/3,
          numericInput(ns("p2_min"), "Min", p2_min_val),
          numericInput(ns("p2_max"), "Max", p2_max_val),
          numericInput(ns("p2_step"), "Step", p2_step_val)
        ),
        tags$hr()
      )
    })
    
    results <- eventReactive(input$run_opt, {
      req(input$target_asset, input$base_asset, input$p1_min, input$p2_min, input$lookback, input$opt_metric)
      
      end_date <- Sys.Date()
      lookback_val <- switch(input$lookback,
                             "1Y" = lubridate::years(1),
                             "2Y" = lubridate::years(2),
                             "5Y" = lubridate::years(5),
                             "10Y" = lubridate::years(10))
      start_date <- end_date - lookback_val
      
      raw_data <- fetch_asset_data(c(input$target_asset, input$base_asset), start_date, end_date)
      wide <- raw_data |> 
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(date) |>
        tidyr::drop_na()
        
      tgt <- input$target_asset
      base <- input$base_asset
      req(tgt %in% names(wide), base %in% names(wide))
      
      wide$tgt_ret <- c(NA, diff(wide[[tgt]]) / head(wide[[tgt]], -1))
      wide$base_ret <- c(NA, diff(wide[[base]]) / head(wide[[base]], -1))
      wide <- tidyr::drop_na(wide)
      
      p1_seq <- seq(input$p1_min, input$p1_max, by = input$p1_step)
      p2_seq <- seq(input$p2_min, input$p2_max, by = input$p2_step)
      grid <- expand.grid(p1 = p1_seq, p2 = p2_seq)
      
      # Enforce reasonable limit
      if (nrow(grid) > 5000) {
        stop("Grid exceeds 5,000 parallel runs, please increase step size.")
      }
      
      strat <- input$strat
      
      # Functional loop across grid parameters piping to C++ Engine
      res_list <- purrr::pmap_dfr(grid, function(p1, p2) {
        w <- wide
        signal <- rep(0, nrow(w))
        
        if (strat == "MA Crossover") {
          fast <- suppressWarnings(TTR::SMA(w[[tgt]], n = p1))
          slow <- suppressWarnings(TTR::SMA(w[[tgt]], n = p2))
          signal <- dplyr::case_when(
            fast > slow ~ 1,
            fast < slow ~ -1,
            TRUE ~ 0
          )
        } else if (strat == "RSI Mean Reversion") {
          rsi <- suppressWarnings(TTR::RSI(w[[tgt]], n = p1))
          signal <- dplyr::case_when(
            rsi < p2 ~ 1,
            rsi > (100 - p2) ~ -1,
            TRUE ~ 0
          )
        } else if (strat == "Divergence Pairs") {
          r <- w[[tgt]] / w[[base]]
          rm <- suppressWarnings(TTR::runMean(r, n = p1))
          rsd <- suppressWarnings(TTR::runSD(r, n = p1))
          z <- (r - rm) / rsd
          signal <- dplyr::case_when(
            z > p2 ~ -1,
            z < -p2 ~ 1,
            TRUE ~ 0
          )
        } else if (strat == "Lead-Lag (1-Day Momentum)") {
          thresh_l <- p1 / 100
          thresh_s <- p2 / 100
          signal <- dplyr::case_when(
            w$base_ret > thresh_l ~ 1,
            w$base_ret < -thresh_s ~ -1,
            TRUE ~ 0
          )
        } else if (strat == "Rolling OLS Beta (Hedging)") {
          win <- p1
          cov_xy <- TTR::runCov(w$tgt_ret, w$base_ret, n = win)
          var_x <- TTR::runVar(w$base_ret, n = win)
          beta_vec <- cov_xy / var_x
          
          expected_ret <- beta_vec * w$base_ret
          alpha_vec <- w$tgt_ret - expected_ret
          
          alpha_mean <- TTR::runMean(alpha_vec, n = win)
          alpha_sd <- TTR::runSD(alpha_vec, n = win)
          alpha_z <- (alpha_vec - alpha_mean) / alpha_sd
          
          signal <- dplyr::case_when(
            alpha_z < -p2 ~ 1,
            alpha_z > p2 ~ -1,
            TRUE ~ 0
          )
        }
        
        # Nullify generated NAs from leading indicators
        signal[is.na(signal)] <- 0
        
        # Compounding C++ Engine Iteration
        strat_eq <- run_backtest(w$tgt_ret, signal)
        daily_ret <- c(NA, diff(strat_eq) / head(strat_eq, -1))
        
        # Performance Metrics
        dr <- na.omit(daily_ret)
        sharpe <- if (length(dr) == 0 || sd(dr) == 0) 0 else sqrt(252) * mean(dr) / sd(dr)
        
        bh_eq <- cumprod(1 + tidyr::replace_na(w$tgt_ret, 0))
        strat_final <- tail(strat_eq, 1) - 1
        bh_final <- tail(bh_eq, 1) - 1
        excess_ret <- strat_final - bh_final
        
        data.frame(p1 = p1, p2 = p2, sharpe = sharpe, excess_ret = excess_ret * 100)
      })
      
      list(grid = res_list, strat = strat, metric = input$opt_metric)
    })
    
    output$heatmap <- plotly::renderPlotly({
      req(results())
      df <- results()$grid
      strat <- results()$strat
      metric <- results()$metric
      
      x_title <- if(strat == "MA Crossover") "Fast MA" else if(strat=="RSI Mean Reversion") "RSI Window" else if(strat=="Lead-Lag (1-Day Momentum)") "Long Trigger (%)" else if(strat=="Rolling OLS Beta (Hedging)") "Beta Window" else "Rolling Window"
      y_title <- if(strat == "MA Crossover") "Slow MA" else if(strat=="RSI Mean Reversion") "Oversold Threshold" else if(strat=="Lead-Lag (1-Day Momentum)") "Short Trigger (%)" else if(strat=="Rolling OLS Beta (Hedging)") "Alpha Z-Score" else "Z-Score Threshold"
      
      opt_var <- if(metric == "Sharpe Ratio") "sharpe" else "excess_ret"
      df$val <- df[[opt_var]]
      
      mat <- df |> dplyr::select(p1, p2, val) |>
             tidyr::pivot_wider(names_from = p1, values_from = val) |>
             dplyr::arrange(p2)
             
      mat_y <- mat$p2
      mat_x <- as.numeric(colnames(mat)[-1])
      mat_z <- as.matrix(mat[,-1])
      
      # Determine absolute maximum value to perfectly center 0.0 at the colorscale midpoint
      max_val <- max(abs(mat_z), na.rm = TRUE)
      if (max_val == 0) max_val <- 1
      
      plotly::plot_ly(
        x = mat_x, y = mat_y, z = mat_z, 
        type = "heatmap", 
        colorscale = "RdYlGn",
        zmin = -max_val, zmax = max_val,
        texttemplate = "%{z:.2f}",
        textfont = list(color = "#000000", size = 11, family = "Arial Black")
      ) |>
        plotly::layout(
          xaxis = list(title = x_title, showgrid = FALSE),
          yaxis = list(title = y_title, showgrid = FALSE),
          plot_bgcolor = "transparent", paper_bgcolor = "transparent",
          font = list(color = "#ffffff")
        )
    })
    
    output$best_params <- renderUI({
      req(results())
      df <- results()$grid
      strat <- results()$strat
      metric <- results()$metric
      
      x_title <- if(strat == "MA Crossover") "Fast MA" else if(strat=="RSI Mean Reversion") "RSI Window" else if(strat=="Lead-Lag (1-Day Momentum)") "Long Trigger (%)" else if(strat=="Rolling OLS Beta (Hedging)") "Beta Window" else "Rolling Window"
      y_title <- if(strat == "MA Crossover") "Slow MA" else if(strat=="RSI Mean Reversion") "Oversold Threshold" else if(strat=="Lead-Lag (1-Day Momentum)") "Short Trigger (%)" else if(strat=="Rolling OLS Beta (Hedging)") "Alpha Z-Score" else "Z-Score Threshold"
      
      opt_var <- if(metric == "Sharpe Ratio") "sharpe" else "excess_ret"
      best <- df[which.max(df[[opt_var]]), ]
      
      best_label <- if(metric == "Sharpe Ratio") "Peak Sharpe Ratio" else "Peak Excess Return"
      best_val <- if(metric == "Sharpe Ratio") sprintf("%.2f", best$sharpe) else sprintf("%+.2f%%", best$excess_ret)
      
      bslib::layout_column_wrap(
        width = 1/4,
        bslib::value_box(sprintf("Optimal %s", x_title), best$p1, theme = "bg-primary"),
        bslib::value_box(sprintf("Optimal %s", y_title), best$p2, theme = "bg-primary"),
        bslib::value_box(best_label, best_val, theme = "bg-success"),
        actionButton(ns("send_to_backtester"), HTML("Load in Backtester<br><small>(Switches Tab)</small>"), class="btn-secondary h-100", style="border-radius: 10px; font-weight: bold;")
      )
    })
    
    # Send payload to main app server when clicked
    eventReactive(input$send_to_backtester, {
      req(results())
      metric <- results()$metric
      opt_var <- if(metric == "Sharpe Ratio") "sharpe" else "excess_ret"
      best <- results()$grid[which.max(results()$grid[[opt_var]]), ]
      
      list(
        strat = results()$strat,
        tgt = input$target_asset,
        base = input$base_asset,
        p1 = best$p1,
        p2 = best$p2
      )
    })
  })
}
