#' Run Mathematical Backtest Strategy
#'
#' Wrapper function that combines asset returns with generic trading signals 
#' (-1, 0, 1) and pushes them to the C++ backend for extreme performance evaluation.
#' This satisfies the Extensibility Framework as ANY strategy can generate a signal vector
#' and pass it into this blindly.
#' 
#' @param asset_returns A numeric vector of raw asset returns.
#' @param signals A numeric vector representing the chosen strategy's output (-1, 0, 1).
#'
#' @return A numeric vector representing the cumulative equity curve.
#' @export
#'
run_backtest <- function(asset_returns, signals) {
  # Apply a 1-day lag to signals to prevent look-ahead bias.
  # Today's closing signal executes on tomorrow's return.
  lagged_signals <- dplyr::lag(signals, n = 1, default = 0)
  
  # Call C++ engine for the heavily-vectorized compounding compounding loop
  result <- cpp_backtest(asset_returns, lagged_signals)
  
  return(result)
}
