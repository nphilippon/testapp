#' Backtester UI
#' @param id NA
#' @import shiny bslib
#' @export
mod_backtester_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      tags$h5("Strategy Configuration"),
      shinyWidgets::pickerInput(ns("target_asset"), "Trade Equity (Target Y)", choices = asset_choices, selected = "SU.TO", options = list(`live-search` = TRUE, size = 10)),
      shinyWidgets::pickerInput(ns("base_asset"), "Signal Asset (Base X)", choices = asset_choices, selected = "CL=F", options = list(`live-search` = TRUE, size = 10)),
      shinyWidgets::radioGroupButtons(
        ns("date_range"), "Lookback", 
        choices = c("10yr", "5yr", "2yr", "1yr", "6mo"), 
        selected = "2yr", size = "sm", status = "primary"
      ),
      selectInput(ns("strategy_type"), "Statistical Logic", choices = c("Divergence (Z-Score Pairs)", "Lead-Lag (1-Day Momentum)", "MA Crossover (Trend)", "RSI Mean Reversion", "Rolling OLS Beta (Hedging)")),
      
      conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Divergence (Z-Score Pairs)'"),
        numericInput(ns("z_window"), "Rolling Window (Days)", value = 60),
        numericInput(ns("z_upper"), "Short Threshold (Sell Z)", value = 2.0, step = 0.5),
        numericInput(ns("z_lower"), "Long Threshold (Buy Z)", value = -2.0, step = 0.5)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Lead-Lag (1-Day Momentum)'"),
        numericInput(ns("mom_thresh"), "Long Target Trigger (%)", value = 1.0, step = 0.5),
        numericInput(ns("mom_thresh_short"), "Short Target Trigger (%)", value = 1.0, step = 0.5)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'MA Crossover (Trend)'"),
        numericInput(ns("fast_ma"), "Fast SMA Window", value = 50, step = 10),
        numericInput(ns("slow_ma"), "Slow SMA Window", value = 200, step = 10)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'RSI Mean Reversion'"),
        numericInput(ns("rsi_n"), "RSI Window", value = 14, step = 2),
        numericInput(ns("rsi_upper"), "Overbought (Sell Threshold)", value = 70, step = 5),
        numericInput(ns("rsi_lower"), "Oversold (Buy Threshold)", value = 30, step = 5)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Rolling OLS Beta (Hedging)'"),
        numericInput(ns("beta_window"), "Rolling Regression Window", value = 60, step = 10),
        numericInput(ns("alpha_trigger"), "Alpha Z-Score Trigger", value = 2.0, step = 0.5)
      ),
      
      tags$hr(),
      tags$h6("Position Sizing"),
      shinyWidgets::materialSwitch(ns("continuous_scaling"), "Proportional Allocation (Scale % to Signal Strength)", status = "success", right = TRUE),
      
      tags$br(),
      actionButton(ns("run_btn"), "Run C++ Backtest", class = "btn-danger w-100")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      bslib::navset_card_underline(
        title = "Backtest Results",
        bslib::nav_panel(
          "Cumulative Equity",
          plotly::plotlyOutput(ns("equity_chart"))
        ),
        bslib::nav_panel(
          "Underlying Price & Signals",
          plotly::plotlyOutput(ns("price_signal_chart"))
        ),
        bslib::nav_panel(
          "Trade Execution Log",
          DT::DTOutput(ns("trade_log_table"))
        )
      ),
      bslib::layout_column_wrap(
        width = 1/4,
        bslib::value_box("Strategy Return", textOutput(ns("strat_ret")), theme = "bg-success"),
        bslib::value_box("Buy & Hold", textOutput(ns("bh_ret")), theme = "bg-dark"),
        bslib::value_box("Sharpe Ratio", textOutput(ns("sharpe")), theme = "bg-info"),
        bslib::value_box("Max Drawdown", textOutput(ns("drawdown")), theme = "bg-danger")
      )
    )
  )
}

#' Backtester Server
#' @param id NA
#' @param opt_payload Reactive payload imported from mod_optimizer securely linking module states
#' @export
mod_backtester_server <- function(id, opt_payload = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    if (!is.null(opt_payload)) {
      observeEvent(opt_payload(), {
        payload <- opt_payload()
        req(payload)
        
        shinyWidgets::updatePickerInput(session, "target_asset", selected = payload$tgt)
        shinyWidgets::updatePickerInput(session, "base_asset", selected = payload$base)
        
        if (payload$strat == "MA Crossover") {
          updateSelectInput(session, "strategy_type", selected = "MA Crossover (Trend)")
          updateNumericInput(session, "fast_ma", value = payload$p1)
          updateNumericInput(session, "slow_ma", value = payload$p2)
        } else if (payload$strat == "RSI Mean Reversion") {
          updateSelectInput(session, "strategy_type", selected = "RSI Mean Reversion")
          updateNumericInput(session, "rsi_n", value = payload$p1)
          updateNumericInput(session, "rsi_upper", value = 100 - payload$p2)
          updateNumericInput(session, "rsi_lower", value = payload$p2)
        } else if (payload$strat == "Divergence Pairs") {
          updateSelectInput(session, "strategy_type", selected = "Divergence (Z-Score Pairs)")
          updateNumericInput(session, "z_window", value = payload$p1)
          updateNumericInput(session, "z_upper", value = abs(payload$p2))
          updateNumericInput(session, "z_lower", value = -abs(payload$p2))
        } else if (payload$strat == "Lead-Lag (1-Day Momentum)") {
          updateSelectInput(session, "strategy_type", selected = "Lead-Lag (1-Day Momentum)")
          updateNumericInput(session, "mom_thresh", value = payload$p1)
          updateNumericInput(session, "mom_thresh_short", value = payload$p2)
        } else if (payload$strat == "Rolling OLS Beta (Hedging)") {
          updateSelectInput(session, "strategy_type", selected = "Rolling OLS Beta (Hedging)")
          updateNumericInput(session, "beta_window", value = payload$p1)
          updateNumericInput(session, "alpha_trigger", value = payload$p2)
        }
      })
    }
    
    results <- eventReactive(input$run_btn, {
      req(input$target_asset, input$base_asset, input$date_range)
      
      end_date <- Sys.Date()
      start_date <- switch(input$date_range,
                           "10yr" = end_date - lubridate::years(10),
                           "5yr" = end_date - lubridate::years(5),
                           "2yr" = end_date - lubridate::years(2),
                           "1yr" = end_date - lubridate::years(1),
                           "6mo" = seq(end_date, length.out = 2, by = "-6 months")[2])
                           
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
      wide$signal <- 0
      wide$reason <- ""
      
      strat <- input$strategy_type
      
      scale_pos <- input$continuous_scaling
      
      if (strat == "Divergence (Z-Score Pairs)") {
        win <- input$z_window
        wide$ratio <- wide[[tgt]] / wide[[base]]
        wide$roll_mean <- TTR::runMean(wide$ratio, n = win)
        wide$roll_sd <- TTR::runSD(wide$ratio, n = win)
        wide$z_score <- (wide$ratio - wide$roll_mean) / wide$roll_sd
        wide$ind1 <- wide$z_score
        
        if (!scale_pos) {
          wide$signal <- dplyr::case_when(wide$z_score > input$z_upper ~ -1, wide$z_score < input$z_lower ~ 1, TRUE ~ 0)
        } else {
          wide$signal <- dplyr::case_when(
            wide$z_score > input$z_upper ~ pmax(-1, -(wide$z_score - input$z_upper) / 2.0),
            wide$z_score < input$z_lower ~ pmin(1, (input$z_lower - wide$z_score) / 2.0),
            TRUE ~ 0
          )
        }
        wide$reason <- paste0("Z-Score: ", round(wide$z_score, 2))
        
      } else if (strat == "Lead-Lag (1-Day Momentum)") {
        thresh_l <- input$mom_thresh / 100
        thresh_s <- input$mom_thresh_short / 100
        wide$ind1 <- wide[[base]]
        if (!scale_pos) {
          wide$signal <- dplyr::case_when(wide$base_ret > thresh_l ~ 1, wide$base_ret < -thresh_s ~ -1, TRUE ~ 0)
        } else {
          wide$signal <- dplyr::case_when(
            wide$base_ret > thresh_l ~ pmin(1, (wide$base_ret - thresh_l) / 0.05),
            wide$base_ret < -thresh_s ~ pmax(-1, (wide$base_ret + thresh_s) / 0.05),
            TRUE ~ 0
          )
        }
        wide$reason <- paste0(base, " Ret: ", round(wide$base_ret * 100, 2), "%")
        
      } else if (strat == "MA Crossover (Trend)") {
        wide$fast <- TTR::SMA(wide[[tgt]], n = input$fast_ma)
        wide$slow <- TTR::SMA(wide[[tgt]], n = input$slow_ma)
        wide$ind1 <- wide$fast
        wide$ind2 <- wide$slow
        if (!scale_pos) {
          wide$signal <- dplyr::case_when(wide$fast > wide$slow ~ 1, wide$fast < wide$slow ~ -1, TRUE ~ 0)
        } else {
          diff_ratio <- (wide$fast - wide$slow) / wide$slow
          wide$signal <- dplyr::case_when(
            diff_ratio > 0 ~ pmin(1, diff_ratio / 0.05), # max scale out at 5% drift
            diff_ratio < 0 ~ pmax(-1, diff_ratio / 0.05),
            TRUE ~ 0
          )
        }
        wide$reason <- paste0("F/S: ", round(wide$fast, 2), " / ", round(wide$slow, 2))
        
      } else if (strat == "RSI Mean Reversion") {
        wide$rsi <- TTR::RSI(wide[[tgt]], n = input$rsi_n)
        wide$ind1 <- wide$rsi
        if (!scale_pos) {
          wide$signal <- dplyr::case_when(wide$rsi < input$rsi_lower ~ 1, wide$rsi > input$rsi_upper ~ -1, TRUE ~ 0)
        } else {
          wide$signal <- dplyr::case_when(
            wide$rsi < input$rsi_lower ~ pmin(1, (input$rsi_lower - wide$rsi) / input$rsi_lower),
            wide$rsi > input$rsi_upper ~ pmax(-1, -(wide$rsi - input$rsi_upper) / (100 - input$rsi_upper)),
            TRUE ~ 0
          )
        }
        wide$reason <- paste0("RSI: ", round(wide$rsi, 1))
        
      } else if (strat == "Rolling OLS Beta (Hedging)") {
        win <- input$beta_window
        cov_xy <- TTR::runCov(wide$tgt_ret, wide$base_ret, n = win)
        var_x <- TTR::runVar(wide$base_ret, n = win)
        wide$beta <- cov_xy / var_x
        
        expected_ret <- wide$beta * wide$base_ret
        wide$alpha <- wide$tgt_ret - expected_ret
        
        wide$alpha_mean <- TTR::runMean(wide$alpha, n = win)
        wide$alpha_sd <- TTR::runSD(wide$alpha, n = win)
        wide$alpha_z <- (wide$alpha - wide$alpha_mean) / wide$alpha_sd
        wide$ind1 <- wide$alpha_z
        
        if (!scale_pos) {
          wide$signal <- dplyr::case_when(wide$alpha_z < -input$alpha_trigger ~ 1, wide$alpha_z > input$alpha_trigger ~ -1, TRUE ~ 0)
        } else {
          wide$signal <- dplyr::case_when(
            wide$alpha_z < -input$alpha_trigger ~ pmin(1, (-input$alpha_trigger - wide$alpha_z) / 2.0),
            wide$alpha_z > input$alpha_trigger ~ pmax(-1, -(wide$alpha_z - input$alpha_trigger) / 2.0),
            TRUE ~ 0
          )
        }
        wide$reason <- paste0("Alpha Z: ", round(wide$alpha_z, 2))
      }
      
      clean <- tidyr::drop_na(wide)
      
      # Execute custom native C++ loop (automatically protects against lookahead bias)
      strat_equity <- run_backtest(clean$tgt_ret, clean$signal)
      bh_equity <- cumprod(1 + clean$tgt_ret)
      strat_daily_ret <- c(NA, diff(strat_equity) / head(strat_equity, -1))
      
      list(
        date = clean$date,
        strat_eq = strat_equity * 100,
        bh_eq = bh_equity * 100,
        strat_ret = strat_daily_ret,
        clean_data = clean,
        tgt = tgt,
        base = base,
        strat = strat
      )
    }, ignoreNULL = FALSE)
    
    output$equity_chart <- plotly::renderPlotly({
      req(results())
      res <- results()
      
      p <- plotly::plot_ly(x = res$date) |>
        plotly::add_lines(y = res$strat_eq, name = "C++ Strategy", line = list(color = "#00ff7f", width = 3)) |>
        plotly::add_lines(y = res$bh_eq, name = "Buy & Hold", line = list(color = "#ffcc00", width = 2, dash = "dash"))
      
      plotly::layout(p,
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = "Equity (Base 100)", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent", paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    output$price_signal_chart <- plotly::renderPlotly({
      req(results())
      res <- results()
      df <- res$clean_data
      tgt <- res$tgt
      strat <- res$strat
      base <- res$base
      
      buys <- df |> dplyr::filter(signal == 1 & dplyr::lag(signal, default=0) != 1)
      sells <- df |> dplyr::filter(signal == -1 & dplyr::lag(signal, default=0) != -1)
      
      req(tgt %in% names(df))
      
      p <- plotly::plot_ly(x = df$date) |>
        plotly::add_lines(y = df[[tgt]], name = tgt, line = list(color = "#ffffff", width = 1.5))
        
      if(nrow(buys) > 0) {
        p <- p |> plotly::add_markers(data=buys, x=~date, y=~get(tgt), name="Buy Signal", marker=list(color="#00ff7f", size=10, symbol="triangle-up"))
      }
      if(nrow(sells) > 0) {
        p <- p |> plotly::add_markers(data=sells, x=~date, y=~get(tgt), name="Sell Signal", marker=list(color="#ff007f", size=10, symbol="triangle-down"))
      }
      
      if (strat == "MA Crossover (Trend)") {
        p <- p |> 
          plotly::add_lines(y = df$ind1, name = "Fast MA", line = list(color = "#00d2ff", dash = "dot", width = 1)) |>
          plotly::add_lines(y = df$ind2, name = "Slow MA", line = list(color = "#ffaa00", dash = "dot", width = 1))
      } else if (strat == "RSI Mean Reversion") {
        p <- p |> 
          plotly::add_lines(y = df$ind1, name = "RSI", yaxis = "y2", line = list(color = "#ffaa00", width = 1)) |>
          plotly::layout(yaxis2 = list(title = "RSI", overlaying = "y", side = "right", showgrid=FALSE, range=c(0, 100)))
      } else if (strat == "Divergence (Z-Score Pairs)") {
        p <- p |> 
          plotly::add_lines(y = df$ind1, name = "Z-Score Divergence", yaxis = "y2", line = list(color = "#ffaa00", width = 1)) |>
          plotly::layout(yaxis2 = list(title = "Z-Score", overlaying = "y", side = "right", showgrid=FALSE))
      } else if (strat == "Lead-Lag (1-Day Momentum)") {
        p <- p |> 
          plotly::add_lines(y = df$ind1, name = paste(base, "Price"), yaxis = "y2", line = list(color = "#00d2ff", dash = "dot", width = 1.5)) |>
          plotly::layout(yaxis2 = list(title = paste(base, "Price"), overlaying = "y", side = "right", showgrid=FALSE))
      } else if (strat == "Rolling OLS Beta (Hedging)") {
        p <- p |> 
          plotly::add_lines(y = df$ind1, name = "Alpha Z-Score", yaxis = "y2", line = list(color = "#ffaa00", width = 1)) |>
          plotly::layout(yaxis2 = list(title = "Alpha Z-Score", overlaying = "y", side = "right", showgrid=FALSE))
      }
        
      plotly::layout(p,
                     title = paste("Trading Signals:", strat),
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = "Asset Price", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent", paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    output$trade_log_table <- DT::renderDT({
       req(results())
       res <- results()
       df <- res$clean_data
       tgt <- res$tgt
       
       req(tgt %in% names(df))
       
       df$Portfolio_Value <- (res$strat_eq / 100) * 10000
       
       # Generate appropriate format depending on scaling choice
       if (isTRUE(input$continuous_scaling)) {
         # Dynamic continuous scaling log 
         log <- df |> 
           dplyr::mutate(
             prev_sig = dplyr::lag(signal, default = 0),
             Alloc_Change = signal - prev_sig,
             Action = dplyr::case_when(
               Alloc_Change > 0.05 ~ paste0("BUY (Scale UP to ", round(signal*100), "%)"), 
               Alloc_Change < -0.05 ~ paste0("SELL (Scale DOWN to ", round(signal*100), "%)"), 
               TRUE ~ "HOLD"
             )
           ) |>
           dplyr::filter(Action != "HOLD")
         
         log$Price <- log[[tgt]]
         log <- log |>
           dplyr::select(`Enter Date` = date, `Enter Price` = Price, Action, `Enter Reason` = reason, `Portfolio Value` = Portfolio_Value) |>
           dplyr::arrange(dplyr::desc(`Enter Date`))
           
         log_df <- as.data.frame(log)
         log_df$`Close Date` <- "---"
         log_df$`Close Price` <- NA_real_
         log_df$`Trade PNL` <- NA_real_
         log_df$`Close Reason` <- "Continuous Run"
         
         log_df <- log_df[, c("Enter Date", "Enter Price", "Action", "Enter Reason", "Close Date", "Close Price", "Trade PNL", "Close Reason", "Portfolio Value")]
         
       } else {
         # Strict binary pairing Round Trip Log
         trades <- list()
         current_trade <- NULL
         
         for (i in seq_len(nrow(df))) {
           s <- df$signal[i]
         p <- df[[tgt]][i]
         d <- as.character(df$date[i])
         r <- df$reason[i]
         
         if (is.null(current_trade)) {
           if (s != 0) {
             current_trade <- list(
               Enter_Date = d,
               Enter_Price = p,
               Action = if(s == 1) "LONG" else "SHORT",
               Enter_Reason = r
             )
           }
         } else {
           if (current_trade$Action == "LONG" && s != 1) {
             trades[[length(trades) + 1]] <- c(current_trade, list(
               Close_Date = d, Close_Price = p, Trade_PNL = (p - current_trade$Enter_Price) / current_trade$Enter_Price, Close_Reason = r
             ))
             current_trade <- NULL
             if (s == -1) {
               current_trade <- list(
                 Enter_Date = d, Enter_Price = p, Action = "SHORT", Enter_Reason = r
               )
             }
           } else if (current_trade$Action == "SHORT" && s != -1) {
             trades[[length(trades) + 1]] <- c(current_trade, list(
               Close_Date = d, Close_Price = p, Trade_PNL = (current_trade$Enter_Price - p) / current_trade$Enter_Price, Close_Reason = r
             ))
             current_trade <- NULL
             if (s == 1) {
               current_trade <- list(
                 Enter_Date = d, Enter_Price = p, Action = "LONG", Enter_Reason = r
               )
             }
           }
         }
       }
       
       if (!is.null(current_trade)) {
         trades[[length(trades) + 1]] <- c(current_trade, list(
           Close_Date = "Open", Close_Price = NA_real_, Trade_PNL = NA_real_, Close_Reason = "N/A"
         ))
       }
       
         if (length(trades) > 0) {
           log_df <- do.call(rbind, lapply(trades, as.data.frame, stringsAsFactors=FALSE))
           log_df <- log_df |> dplyr::arrange(dplyr::desc(Enter_Date))
           colnames(log_df) <- c("Enter Date", "Enter Price", "Action", "Enter Reason", "Close Date", "Close Price", "Trade PNL", "Close Reason")
         } else {
           log_df <- data.frame(`Enter Date`=character(), `Enter Price`=numeric(), Action=character(), `Enter Reason`=character(), `Close Date`=character(), `Close Price`=numeric(), `Trade PNL`=numeric(), `Close Reason`=character(), check.names = FALSE)
         }
       } # End Else block
       
       cols_currency <- c("Enter Price", "Close Price")
       if ("Portfolio Value" %in% names(log_df)) cols_currency <- c(cols_currency, "Portfolio Value")
       
       DT::datatable(log_df, class="cell-border stripe dark-table", options=list(pageLength=10, scrollX=TRUE)) |>
         DT::formatCurrency(columns = cols_currency, currency = "$") |>
         DT::formatPercentage("Trade PNL", 2) |>
         DT::formatStyle('Action', color = DT::styleEqual(c("LONG", "SHORT"), c('#00ff7f', '#ff007f'))) |>
         DT::formatStyle('Trade PNL', color = DT::styleInterval(0, c('#ff007f', '#00ff7f')))
    })
    
    output$strat_ret <- renderText({
      req(results())
      val <- tail(results()$strat_eq, 1) - 100
      sprintf("%+.2f%%", val)
    })
    output$bh_ret <- renderText({
      req(results())
      val <- tail(results()$bh_eq, 1) - 100
      sprintf("%+.2f%%", val)
    })
    output$sharpe <- renderText({
      req(results())
      rets <- na.omit(results()$strat_ret)
      if (length(rets) == 0 || sd(rets) == 0) return("0.00")
      val <- sqrt(252) * mean(rets) / sd(rets)
      sprintf("%.2f", val)
    })
    output$drawdown <- renderText({
      req(results())
      eq <- results()$strat_eq
      roll_max <- cummax(eq)
      dd <- (eq - roll_max) / roll_max
      val <- min(dd, na.rm = TRUE) * 100
      sprintf("%.2f%%", val)
    })
  })
}
