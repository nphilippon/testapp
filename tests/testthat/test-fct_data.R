test_that("fetch_asset_data returns tibble and handles requests correctly", {
  # Skip on CRAN or without internet connection to avoid random failures pipeline blocks
  skip_if_offline()
  skip_on_cran()

  # Execute live API data fetch representing a 30-day lookback safely
  end_dt <- Sys.Date()
  start_dt <- end_dt - 30
  
  result <- fetch_asset_data("SPY", start_date = start_dt, end_date = end_dt)
  
  # Validate exact data extraction types mapping internally
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
  
  # Ensure target explicit OHLCV fields natively exist and mapped safely
  expect_true("date" %in% names(result))
  expect_true("symbol" %in% names(result))
  expect_true("close" %in% names(result))
  
  # Symbol accuracy preservation filter valid output
  expect_true(all(result$symbol == "SPY"))
  
  # Validate row counts strictly positive for a 30 day period (should be ~20 trading days safely)
  expect_true(nrow(result) > 10)
})
