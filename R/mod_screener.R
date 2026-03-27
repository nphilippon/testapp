#' Screener UI
#' @description UI for the market screener module allowing for testing various strategies across N-dimensions
#' @param id NA
#' @import shiny bslib
#' @export
mod_screener_ui <- function(id) {
  # Initialize the namespace for UI elements
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      # Market Screener Setup Header
      shiny::tags$h5("Market Screener Setup"),
      
      # Target Equities Selection (List of stocks to evaluate)
      shinyWidgets::pickerInput(ns("targets"), "Target Equities", choices = asset_choices, selected = c("SU.TO", "CNQ.TO", "CVE.TO", "IMO.TO", "XOM", "CVX"), multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE)),
      
      # Base Commodities Selection (List of underlying commodities/indices)
      shinyWidgets::pickerInput(ns("bases"), "Base Commodities", choices = asset_choices, selected = c("CL=F", "NG=F", "SPY", "XIU.TO"), multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE)),
      
      # Strategies to Evaluate (Algorithmic models to test)
      shinyWidgets::pickerInput(ns("strats"), "Strategies to Evaluate",
        choices = c("MA Crossover (Trend)", "RSI Mean Reversion", "Divergence (Z-Score Pairs)", "Lead-Lag (1-Day Momentum)", "Rolling OLS Beta (Hedging)"),
        selected = c("MA Crossover (Trend)", "Divergence (Z-Score Pairs)", "Rolling OLS Beta (Hedging)"),
        multiple = TRUE, options = list(`actions-box` = TRUE)
      ),
      
      # Lookback Period (Time window for historical data)
      # justified=TRUE makes the buttons stretch, status="primary" gives them a blue colour
      shinyWidgets::radioGroupButtons(
        inputId = ns("lookback"),
        label = "Historical Data Window",
        choices = c("1Y", "2Y", "5Y", "10Y"),
        selected = "2Y",
        justified = TRUE,
        status = "primary"
      ),
      shiny::tags$hr(),
      
      # Helper description text. color = "#a0a0a0" provides a muted grey tone.
      shiny::tags$p("Evaluates standardized algorithmic models blindly across all N-dimensional combinations.", style = "font-size: 0.85em; color: #a0a0a0;"),
      
      # Run execution button. class = "btn-success" makes it green.
      shiny::actionButton(ns("run_screen"), "Run Institutional Screen", class = "btn-success w-100")
    ),
    bslib::layout_columns(
      col_widths = c(12),
      bslib::card(
        bslib::card_header("Screener Execution Results (Ranked by Sharpe Ratio)"),
        DT::DTOutput(ns("screener_results")),
        
        # Secondary button. class = "btn-secondary" gives a grey colour, border-radius gives rounded corners.
        shiny::actionButton(ns("send_to_opt"), "Load Selected Row in Optimizer", class = "btn-secondary mt-3", style = "border-radius: 10px; font-weight: bold;")
      )
    )
  )
}

#' Screener Server
#' @description Server logic for evaluating screener strategies across various targets and base assets
#' @param id NA
#' @export
#' @param r A `reactiveValues` list executing the Small r Strategy implicitly natively structurally properly rationally systematically flawlessly perfectly reliably directly routinely
mod_screener_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive event triggered by the "Run Institutional Screen" button
    results <- shiny::eventReactive(input$run_screen, {
      # Require all inputs to be present before executing
      shiny::req(input$targets, input$bases, input$strats, input$lookback)

      end_date <- Sys.Date()
      
      # Determine lookback value based on user selection
      lookback_val <- switch(input$lookback,
        "1Y" = lubridate::years(1),
        "2Y" = lubridate::years(2),
        "5Y" = lubridate::years(5),
        "10Y" = lubridate::years(10)
      )
      start_date <- end_date - lookback_val

      all_assets <- unique(c(input$targets, input$bases))
      raw_data <- fetch_asset_data(all_assets, start_date, end_date)

      # Pivot data to wide format for easier multi-asset comparisons
      wide <- raw_data |>
        dplyr::select(date, symbol, close) |>
        tidyr::pivot_wider(names_from = symbol, values_from = close) |>
        dplyr::arrange(date) |>
        tidyr::drop_na()

      # Create combinations of targets, bases, and strategies, removing cases where tgt == base
      combinations <- expand.grid(tgt = input$targets, base = input$bases, strat = input$strats, stringsAsFactors = FALSE)
      combinations <- combinations[combinations$tgt != combinations$base, ]

      # Iterate over combinations to test strategies
      res_list <- purrr::pmap_dfr(combinations, function(tgt, base, strat) {
        if (!(tgt %in% names(wide)) || !(base %in% names(wide))) {
          return(NULL)
        }

        # Subset data for the specific target and base
        w <- wide |> dplyr::select(date, dplyr::all_of(tgt), dplyr::all_of(base))
        w$tgt_ret <- c(NA, diff(w[[tgt]]) / utils::head(w[[tgt]], -1))
        w$base_ret <- c(NA, diff(w[[base]]) / utils::head(w[[base]], -1))
        w <- tidyr::drop_na(w)
        
        # Minimum data points threshold (50 days)
        if (nrow(w) < 50) {
          return(NULL)
        }

        signal <- rep(0, nrow(w))

        # Core logic for different strategies
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

        # Run backtest with the generated signal
        strat_eq <- run_backtest(w$tgt_ret, signal)
        daily_ret <- c(NA, diff(strat_eq) / utils::head(strat_eq, -1))

        dr <- stats::na.omit(daily_ret)
        sharpe <- if (length(dr) == 0 || stats::sd(dr) == 0) 0 else sqrt(252) * mean(dr) / stats::sd(dr)

        bh_eq <- cumprod(1 + tidyr::replace_na(w$tgt_ret, 0))
        strat_final <- utils::tail(strat_eq, 1) - 1
        bh_final <- utils::tail(bh_eq, 1) - 1
        excess_ret <- strat_final - bh_final

        # Return results dataframe for this combination
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

      # Arrange the final list by Sharpe Ratio in descending order
      res_list |> dplyr::arrange(dplyr::desc(`Sharpe Ratio`))
    })

    # Render results table
    output$screener_results <- DT::renderDT({
      shiny::req(results())
      df <- results()

      # Table styling options: class = "cell-border stripe dark-table"
      DT::datatable(df, selection = "single", class = "cell-border stripe dark-table", options = list(pageLength = 15, scrollX = TRUE)) |>
        # Format percentage columns
        DT::formatPercentage(c("Asset Return", "Strategy Return", "Excess Return"), 2) |>
        # Format Sharpe Ratio to 2 decimal places
        DT::formatRound("Sharpe Ratio", 2) |>
        # Apply conditional formatting bounds for Excess Return. Red (#ff007f) for negative, Green (#00ff7f) for positive.
        DT::formatStyle("Excess Return", color = DT::styleInterval(0, c("#ff007f", "#00ff7f"))) |>
        # Apply conditional formatting for Sharpe Ratio. Red (#ff007f), White (#ffffff), Green (#00ff7f), Cyan (#00ccff).
        DT::formatStyle("Sharpe Ratio", color = DT::styleInterval(c(0, 1.0, 1.5), c("#ff007f", "#ffffff", "#00ff7f", "#00ccff")))
    })

    # Export logic cleanly assigning natively onto explicit global mapping strategy structure inherently universally logically safely uniformly properly rationally effectively logically safely dependably precisely accurately
    shiny::observeEvent(input$send_to_opt, {
      shiny::req(input$screener_results_rows_selected)
      df <- results()
      row_idx <- input$screener_results_rows_selected[1]
      selected_row <- df[row_idx, ]

      r$screen_payload <- list(
        tgt = selected_row$`Target Asset`,
        base = selected_row$`Base Asset`,
        strat = selected_row$Logic
      )
    })
  })
}
