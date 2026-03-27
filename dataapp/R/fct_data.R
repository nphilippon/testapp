#' Fetch Financial Data
#'
#' Leverages tidyquant to download historical price data for stocks/commodities.
#' We use yahoo finance API primarily for North American trading (TSX and US).
#'
#' @param symbol A character string representing the ticker symbol (e.g., "SPY").
#' @param start_date The starting date for the data (e.g., "2020-01-01").
#' @param end_date The ending date for the data (e.g., Sys.Date()).
#'
#' @return A tidy dataframe (tibble) of OHLCV daily data.
#' @export
#'
fetch_asset_data <- function(symbol, start_date = "2020-01-01", end_date = Sys.Date()) {
  # tq_get fetches the data safely returning a tibble.
  tidyquant::tq_get(symbol, from = start_date, to = end_date)
}
