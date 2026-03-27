#' Screener UI
#' @param id NA
#' @import shiny bslib
#' @export
mod_screener_ui <- function(id) {
  ns <- NS(id)
  
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      tags$h5("Market Screener Setup"),
      shinyWidgets::pickerInput(ns("targets"), "Target Equities", choices = asset_choices, selected = c("SU.TO", "CNQ.TO", "CVE.TO", "IMO.TO", "XOM", "CVX"), multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE)),
      shinyWidgets::pickerInput(ns("bases"), "Base Commodities", choices = asset_choices, selected = c("CL=F", "GC=F", "SI=F", "HG=F", "NG=F"), multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE)),
      
      shinyWidgets::pickerInput(ns("strats"), "Strategies to Evaluate", 
                                choices = c("MA Crossover (Trend)", "RSI Mean Reversion", "Divergence (Z-Score Pairs)", "Lead-Lag (1-Day Momentum)", "Rolling OLS Beta (Hedging)"),
                                selected = c("MA Crossover (Trend)", "Divergence (Z-Score Pairs)", "Rolling OLS Beta (Hedging)"),
                                multiple = TRUE, options = list(`actions-box` = TRUE)),
                                
      shinyWidgets::radioGroupButtons(
        inputId = ns("lookback"),
        label = "Historical Data Window",
        choices = c("1Y", "2Y", "5Y", "10Y"),
        selected = "2Y",
        justified = TRUE,
        status = "primary"
      ),
      
      tags$hr(),
      tags$p("Evaluates standardized algorithmic models blindly across all N-dimensional combinations.", style="font-size: 0.85em; color: #a0a0a0;"),
      
      actionButton(ns("run_screen"), "Run Institutional Screen", class = "btn-success w-100")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Screener Execution Results (Ranked by Sharpe Ratio)"),
        DT::DTOutput(ns("screener_results")),
        actionButton(ns("send_to_opt"), "Load Selected Row in Optimizer", class = "btn-secondary mt-3", style = "border-radius: 10px; font-weight: bold;")
      )
    )
  )
}

#' Screener Server
#' @param id NA
#' @export
mod_screener_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    results <- eventReactive(input$run_screen, {
      req(input$targets, input$bases, input$strats, input$lookback)
      
      end_date <- Sys.Date()
      lookback_val <- switch(input$lookback,
                             "1Y" = lubridate::years(1),
                             "2Y" = lubridate::years(2),
                             "5Y" = lubridate::years(5),
                             "10Y" = lubridate::years(10))
      start_date <- end_date - lookback_val
      
      all_assets <- unique(c(input$targets, input$bases))
      raw_data <- fetch_asset_data(all_assets, start_date, end_date)
      
      wide <- raw_data |> 
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(date) |>
        tidyr::drop_na()
      
      combinations <- expand.grid(tgt = input$targets, base = input$bases, strat = input$strats, stringsAsFactors = FALSE)
      combinations <- combinations[combinations$tgt != combinations$base, ]
      
      res_list <- purrr::pmap_dfr(combinations, function(tgt, base, strat) {
        if (!(tgt %in% names(wide)) || !(base %in% names(wide))) return(NULL)
        
        w <- wide |> dplyr::select(date, all_of(tgt), all_of(base))
        w$tgt_ret <- c(NA, diff(w[[tgt]]) / head(w[[tgt]], -1))
        w$base_ret <- c(NA, diff(w[[base]]) / head(w[[base]], -1))
        w <- tidyr::drop_na(w)
        if(nrow(w) < 50) return(NULL)
        
        signal <- rep(0, nrow(w))
        
        if (strat == "MA Crossover (Trend)") {
          fast <- suppressWarnings(TTR::SMA(w[[tgt]], n = 50))
          slow <- suppressWarnings(TTR::SMA(w[[tgt]], n = 200))
          signal <- dplyr::case_when(fast > slow ~ 1, fast < slow ~ -1, TRUE ~ 0)
        } else if (strat == "RSI Mean Reversion") {
          rsi <- suppressWarnings(TTR::RSI(w[[tgt]], n = 14))
          signal <- dplyr::case_when(rsi < 30 ~ 1, rsi > 70 ~ -1, TRUE ~ 0)
        } else if (strat == "Divergence (Z-Score Pairs)") {
          r <- w[[tgt]] / w[[base]]
          rm <- suppressWarnings(TTR::runMean(r, n = 60))
          rsd <- suppressWarnings(TTR::runSD(r, n = 60))
          z <- (r - rm) / rsd
          signal <- dplyr::case_when(z > 2.0 ~ -1, z < -2.0 ~ 1, TRUE ~ 0)
        } else if (strat == "Lead-Lag (1-Day Momentum)") {
          thresh <- 1.0 / 100
          signal <- dplyr::case_when(w$base_ret > thresh ~ 1, w$base_ret < -thresh ~ -1, TRUE ~ 0)
        } else if (strat == "Rolling OLS Beta (Hedging)") {
          win <- 60
          cov_xy <- TTR::runCov(w$tgt_ret, w$base_ret, n = win)
          var_x <- TTR::runVar(w$base_ret, n = win)
          beta_vec <- cov_xy / var_x
          expected_ret <- beta_vec * w$base_ret
          alpha_vec <- w$tgt_ret - expected_ret
          alpha_mean <- TTR::runMean(alpha_vec, n = win)
          alpha_sd <- TTR::runSD(alpha_vec, n = win)
          alpha_z <- (alpha_vec - alpha_mean) / alpha_sd
          signal <- dplyr::case_when(alpha_z < -2.0 ~ 1, alpha_z > 2.0 ~ -1, TRUE ~ 0)
        }
        
        signal[is.na(signal)] <- 0
        
        strat_eq <- run_backtest(w$tgt_ret, signal)
        daily_ret <- c(NA, diff(strat_eq) / head(strat_eq, -1))
        
        dr <- na.omit(daily_ret)
        sharpe <- if (length(dr) == 0 || sd(dr) == 0) 0 else sqrt(252) * mean(dr) / sd(dr)
        
        bh_eq <- cumprod(1 + tidyr::replace_na(w$tgt_ret, 0))
        strat_final <- tail(strat_eq, 1) - 1
        bh_final <- tail(bh_eq, 1) - 1
        excess_ret <- strat_final - bh_final
        
        data.frame(
          `Target Asset` = tgt,
          `Base Asset` = base,
          Logic = strat,
          `Asset Return` = bh_final,
          `Strategy Return` = strat_final,
          `Excess Return` = excess_ret,
          `Sharpe Ratio` = sharpe,
          check.names = FALSE
        )
      })
      
      res_list |> dplyr::arrange(dplyr::desc(`Sharpe Ratio`))
    })
    
    output$screener_results <- DT::renderDT({
      req(results())
      df <- results()
      
      DT::datatable(df, selection = 'single', class="cell-border stripe dark-table", options=list(pageLength=15, scrollX=TRUE)) |>
        DT::formatPercentage(c("Asset Return", "Strategy Return", "Excess Return"), 2) |>
        DT::formatRound("Sharpe Ratio", 2) |>
        DT::formatStyle('Excess Return', color = DT::styleInterval(0, c('#ff007f', '#00ff7f'))) |>
        DT::formatStyle('Sharpe Ratio', color = DT::styleInterval(c(0, 1.0, 1.5), c('#ff007f', '#ffffff', '#00ff7f', '#00ccff')))
    })
    
    export_payload <- eventReactive(input$send_to_opt, {
      req(input$screener_results_rows_selected)
      df <- results()
      row_idx <- input$screener_results_rows_selected[1]
      selected_row <- df[row_idx, ]
      
      list(
        tgt = selected_row$`Target Asset`,
        base = selected_row$`Base Asset`,
        strat = selected_row$Logic
      )
    })
    
    return(export_payload)
  })
}
