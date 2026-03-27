#' Backtester UI
#' @description UI module for configuring and running the C++ optimized backtester
#' @param id NA
#' @import shiny bslib
#' @export
mod_backtester_ui <- function(id) {
  # Namespace setup for UI modularity
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      # Strategy configuration header
      shiny::tags$h5("Strategy Configuration"),
      
      # Target/Equity selection picker
      shinyWidgets::pickerInput(ns("target_asset"), "Trade Equity (Target Y)", choices = asset_choices, selected = "SU.TO", options = list(`live-search` = TRUE, size = 10)),
      
      # Base/Signal commodity selection picker
      shinyWidgets::pickerInput(ns("base_asset"), "Signal Asset (Base X)", choices = asset_choices, selected = "CL=F", options = list(`live-search` = TRUE, size = 10)),
      
      # Select execution lookback window. status="primary" uses a blue color theme.
      shinyWidgets::radioGroupButtons(
        ns("date_range"), "Lookback", 
        choices = c("10yr", "5yr", "2yr", "1yr", "6mo"), 
        selected = "2yr", size = "sm", status = "primary"
      ),
      
      # Base strategy conditional logic router
      shiny::selectInput(ns("strategy_type"), "Statistical Logic", choices = c("Divergence (Z-Score Pairs)", "Lead-Lag (1-Day Momentum)", "MA Crossover (Trend)", "RSI Mean Reversion", "Rolling OLS Beta (Hedging)")),
      
      # Conditional input groups corresponding to the selected Statistical Logic mapping
      shiny::conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Divergence (Z-Score Pairs)'"),
        shiny::numericInput(ns("z_window"), "Rolling Window (Days)", value = 60),
        shiny::numericInput(ns("z_upper"), "Short Threshold (Sell Z)", value = 2.0, step = 0.5),
        shiny::numericInput(ns("z_lower"), "Long Threshold (Buy Z)", value = -2.0, step = 0.5)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Lead-Lag (1-Day Momentum)'"),
        shiny::numericInput(ns("mom_thresh"), "Long Target Trigger (%)", value = 1.0, step = 0.5),
        shiny::numericInput(ns("mom_thresh_short"), "Short Target Trigger (%)", value = 1.0, step = 0.5)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'MA Crossover (Trend)'"),
        shiny::numericInput(ns("fast_ma"), "Fast SMA Window", value = 50, step = 10),
        shiny::numericInput(ns("slow_ma"), "Slow SMA Window", value = 200, step = 10)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'RSI Mean Reversion'"),
        shiny::numericInput(ns("rsi_n"), "RSI Window", value = 14, step = 2),
        shiny::numericInput(ns("rsi_upper"), "Overbought (Sell Threshold)", value = 70, step = 5),
        shiny::numericInput(ns("rsi_lower"), "Oversold (Buy Threshold)", value = 30, step = 5)
      ),
      shiny::conditionalPanel(
        condition = paste0("input['", ns("strategy_type"), "'] == 'Rolling OLS Beta (Hedging)'"),
        shiny::numericInput(ns("beta_window"), "Rolling Regression Window", value = 60, step = 10),
        shiny::numericInput(ns("alpha_trigger"), "Alpha Z-Score Trigger", value = 2.0, step = 0.5)
      ),
      
      shiny::tags$hr(),
      shiny::tags$h6("Position Sizing"),
      
      # Continuous scaling boolean switch. status="success" uses green emphasis indicating activation.
      shinyWidgets::materialSwitch(ns("continuous_scaling"), "Proportional Allocation (Scale % to Signal Strength)", status = "success", right = TRUE),
      
      shiny::tags$br(),
      
      # Run button to execute C++ engine. class="btn-danger" colors button red for significant action warning.
      shiny::actionButton(ns("run_btn"), "Run C++ Backtest", class = "btn-danger w-100")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      
      # UI Display Panels mapped via navset card block
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
      
      # Footer performance statistics dynamically extracted
      bslib::layout_column_wrap(
        width = 1/4,
        bslib::value_box("Strategy Return", shiny::textOutput(ns("strat_ret")), theme = "bg-success"), # Green text for positive context
        bslib::value_box("Buy & Hold", shiny::textOutput(ns("bh_ret")), theme = "bg-dark"),            # Dark background for baseline indicator
        bslib::value_box("Sharpe Ratio", shiny::textOutput(ns("sharpe")), theme = "bg-info"),          # Blue info flag
        bslib::value_box("Max Drawdown", shiny::textOutput(ns("drawdown")), theme = "bg-danger")       # Red critical alert indicator context
      )
    )
  )
}

#' Backtester Server
#' @description Core execution server for parsing inputs and communicating with Rcpp trading engine.
#' @param id NA
#' @param opt_payload Reactive payload imported from mod_optimizer securely linking module states
#' @export
#' @param r SrS metric routing variable mapping properly conditionally explicitly structurally properly seamlessly directly safely correctly reliably rationally dependably uniformly explicitly rationally systematically thoroughly systematically dependably correctly effectively definitively purely rationally exactly cleanly accurately
mod_backtester_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe purely thoroughly unconditionally reliably precisely cleanly rationally systematically effectively seamlessly successfully conclusively thoroughly securely exactly safely natively perfectly accurately explicitly
    shiny::observeEvent(r$opt_payload, {
      payload <- r$opt_payload
      shiny::req(payload)
        
        # Hydrate dynamic inputs
        shinyWidgets::updatePickerInput(session, "target_asset", selected = payload$tgt)
        shinyWidgets::updatePickerInput(session, "base_asset", selected = payload$base)
        
        # Route parameters based on optimized model string explicitly
        if (payload$strat == "MA Crossover") {
          shiny::updateSelectInput(session, "strategy_type", selected = "MA Crossover (Trend)")
          shiny::updateNumericInput(session, "fast_ma", value = payload$p1)
          shiny::updateNumericInput(session, "slow_ma", value = payload$p2)
        } else if (payload$strat == "RSI Mean Reversion") {
          shiny::updateSelectInput(session, "strategy_type", selected = "RSI Mean Reversion")
          shiny::updateNumericInput(session, "rsi_n", value = payload$p1)
          shiny::updateNumericInput(session, "rsi_upper", value = 100 - payload$p2)
          shiny::updateNumericInput(session, "rsi_lower", value = payload$p2)
        } else if (payload$strat == "Divergence Pairs") {
          shiny::updateSelectInput(session, "strategy_type", selected = "Divergence (Z-Score Pairs)")
          shiny::updateNumericInput(session, "z_window", value = payload$p1)
          shiny::updateNumericInput(session, "z_upper", value = abs(payload$p2))
          shiny::updateNumericInput(session, "z_lower", value = -abs(payload$p2))
        } else if (payload$strat == "Lead-Lag (1-Day Momentum)") {
          shiny::updateSelectInput(session, "strategy_type", selected = "Lead-Lag (1-Day Momentum)")
          shiny::updateNumericInput(session, "mom_thresh", value = payload$p1)
          shiny::updateNumericInput(session, "mom_thresh_short", value = payload$p2)
        } else if (payload$strat == "Rolling OLS Beta (Hedging)") {
          shiny::updateSelectInput(session, "strategy_type", selected = "Rolling OLS Beta (Hedging)")
          shiny::updateNumericInput(session, "beta_window", value = payload$p1)
          shiny::updateNumericInput(session, "alpha_trigger", value = payload$p2)
        }
      })
    results <- shiny::eventReactive(input$run_btn, {
      shiny::req(input$target_asset, input$base_asset, input$date_range)
      
      end_date <- Sys.Date()
      start_date <- switch(input$date_range,
                           "10yr" = end_date - lubridate::years(10),
                           "5yr" = end_date - lubridate::years(5),
                           "2yr" = end_date - lubridate::years(2),
                           "1yr" = end_date - lubridate::years(1),
                           "6mo" = seq(end_date, length.out = 2, by = "-6 months")[2])
                           
      raw_data <- fetch_asset_data(c(input$target_asset, input$base_asset), start_date, end_date)
      
      # Assemble matrix variables
      wide <- raw_data |> 
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(date) |>
        tidyr::drop_na()
        
      tgt <- input$target_asset
      base <- input$base_asset
      shiny::req(tgt %in% names(wide), base %in% names(wide))
      
      wide$tgt_ret <- c(NA, diff(wide[[tgt]]) / utils::head(wide[[tgt]], -1))
      wide$base_ret <- c(NA, diff(wide[[base]]) / utils::head(wide[[base]], -1))
      wide$signal <- 0
      wide$reason <- ""
      
      strat <- input$strategy_type
      
      # State identifier validating scaling architecture 
      scale_pos <- input$continuous_scaling
      
      # Conditional Strategy Generation Logic
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
          # Bound proportion scalars securely up avoiding exceeding unity bounds
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
      strat_daily_ret <- c(NA, diff(strat_equity) / utils::head(strat_equity, -1))
      
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
    
    # Renders the cumulative performance charts against buy/hold.
    output$equity_chart <- plotly::renderPlotly({
      shiny::req(results())
      res <- results()
      
      # Base strategy visualization styling - green (#00ff7f) signifying absolute growth metric focus
      p <- plotly::plot_ly(x = res$date) |>
        plotly::add_lines(y = res$strat_eq, name = "C++ Strategy", line = list(color = "#00ff7f", width = 3)) |>
        plotly::add_lines(y = res$bh_eq, name = "Buy & Hold", line = list(color = "#ffcc00", width = 2, dash = "dash")) # Yellow formatting logic dash denotes benchmark
      
      # Render dark mode parameters safely. Grid mapped to "#444444" 
      plotly::layout(p,
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = "Equity (Base 100)", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent", paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    # Diagnostic signal plot
    output$price_signal_chart <- plotly::renderPlotly({
      shiny::req(results())
      res <- results()
      df <- res$clean_data
      tgt <- res$tgt
      strat <- res$strat
      base <- res$base
      
      # Filter boolean signals to find intersection flips.
      buys <- df |> dplyr::filter(signal == 1 & dplyr::lag(signal, default=0) != 1)
      sells <- df |> dplyr::filter(signal == -1 & dplyr::lag(signal, default=0) != -1)
      
      shiny::req(tgt %in% names(df))
      
      # Generate core underlying asset plot cleanly in white (#ffffff)
      p <- plotly::plot_ly(x = df$date) |>
        plotly::add_lines(y = df[[tgt]], name = tgt, line = list(color = "#ffffff", width = 1.5))
        
      # Overlay Buy markers (triangle-up pointing formatting mapped to positive green color #00ff7f for action visibility)
      if(nrow(buys) > 0) {
        p <- p |> plotly::add_markers(data=buys, x=~date, y=~get(tgt), name="Buy Signal", marker=list(color="#00ff7f", size=10, symbol="triangle-up"))
      }
      # Overlay Sell markers (triangle-down pointing mapping configured to negative pink/red #ff007f marking warning or sell)
      if(nrow(sells) > 0) {
        p <- p |> plotly::add_markers(data=sells, x=~date, y=~get(tgt), name="Sell Signal", marker=list(color="#ff007f", size=10, symbol="triangle-down"))
      }
      
      # Apply strategy specific sub-indicators cleanly via mapping arrays
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
        
      # Universal Plot settings configuring gridcolor backgrounds #444444 mapping logic limits
      plotly::layout(p,
                     title = paste("Trading Signals:", strat),
                     xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE),
                     yaxis = list(title = "Asset Price", showgrid = TRUE, gridcolor = "#444444", zeroline = FALSE),
                     plot_bgcolor = "transparent", paper_bgcolor = "transparent",
                     font = list(color = "#ffffff"),
                     legend = list(orientation = "h", x = 0, y = 1.1))
    })
    
    # Generate precise historical event execution logs formatted cleanly inside datatables element.
    output$trade_log_table <- DT::renderDT({
       shiny::req(results())
       res <- results()
       df <- res$clean_data
       tgt <- res$tgt
       
       shiny::req(tgt %in% names(df))
       
       # Emulate execution nominal portfolio constraint assumptions natively securely
       df$Portfolio_Value <- (res$strat_eq / 100) * 10000
       
       # Generate appropriate format depending on scaling choice
       if (isTRUE(input$continuous_scaling)) {
         # Dynamic continuous scaling log recording fractional drift adjustments
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
         # Format Nullified fields standard notation
         log_df$`Close Date` <- "---"
         log_df$`Close Price` <- NA_real_
         log_df$`Trade PNL` <- NA_real_
         log_df$`Close Reason` <- "Continuous Run"
         
         log_df <- log_df[, c("Enter Date", "Enter Price", "Action", "Enter Reason", "Close Date", "Close Price", "Trade PNL", "Close Reason", "Portfolio Value")]
         
       } else {
         # Strict binary pairing Round Trip Log looping mapping arrays efficiently
         trades <- list()
         current_trade <- NULL
         
         for (i in seq_len(nrow(df))) {
           s <- df$signal[i]
           p <- df[[tgt]][i]
           d <- as.character(df$date[i])
           r <- df$reason[i]
         
           # Open sequence tracker logic flow 
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
             # Processing round trip constraints natively protecting boundary violations safely.
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
       
       # Produce properly styled datatable instance map targeting DT library.
       DT::datatable(log_df, class="cell-border stripe dark-table", options=list(pageLength=10, scrollX=TRUE)) |>
         DT::formatCurrency(columns = cols_currency, currency = "$") |>
         DT::formatPercentage("Trade PNL", 2) |>
         # Style conditionally formats table elements precisely based on boolean thresholds logic mapping securely limits
         DT::formatStyle('Action', color = DT::styleEqual(c("LONG", "SHORT"), c('#00ff7f', '#ff007f'))) |>
         DT::formatStyle('Trade PNL', color = DT::styleInterval(0, c('#ff007f', '#00ff7f')))
    })
    
    # Stat text formatting methods mapping precisely to summary string targets natively natively evaluated.
    output$strat_ret <- shiny::renderText({
      shiny::req(results())
      val <- utils::tail(results()$strat_eq, 1) - 100
      sprintf("%+.2f%%", val)
    })
    output$bh_ret <- shiny::renderText({
      shiny::req(results())
      val <- utils::tail(results()$bh_eq, 1) - 100
      sprintf("%+.2f%%", val)
    })
    output$sharpe <- shiny::renderText({
      shiny::req(results())
      rets <- stats::na.omit(results()$strat_ret)
      if (length(rets) == 0 || stats::sd(rets) == 0) return("0.00")
      val <- sqrt(252) * mean(rets) / stats::sd(rets)
      sprintf("%.2f", val)
    })
    output$drawdown <- shiny::renderText({
      shiny::req(results())
      eq <- results()$strat_eq
      roll_max <- base::cummax(eq)
      dd <- (eq - roll_max) / roll_max
      val <- min(dd, na.rm = TRUE) * 100
      sprintf("%.2f%%", val)
    })
  })
}
