test_that("run_backtest executes correctly natively guarding against lookahead bias safely", {
  
  # Dummy arrays representing raw 5 day sequence mapping returns natively
  ret <- c(0.01, -0.02, 0.03, -0.01, 0.05)
  sig <- c(1, -1, 1, 0, 1)
  
  # Expected calculations securely mapping lookahead protection matrix externally
  # Signal executes precisely on T+1 return execution natively sequentially logic safely
  expected_lag <- c(0, 1, -1, 1, 0) # dplyr::lag(sig, 1, default=0)
  
  # Compute theoretical portfolio compound trajectory scalar securely logically
  expected_daily <- expected_lag * ret
  expected_eq <- cumprod(1 + expected_daily)
  
  # Execute R wrapper sending data to the C++ backtest routine mathematically natively
  result <- run_backtest(ret, sig)
  
  # Deterministic test ensuring zero float/logic deviation in high-performance execution blocks
  expect_equal(result, expected_eq)
  
  # Assert scaling boundaries natively safely preserved
  expect_true(is.numeric(result))
  expect_equal(length(result), length(ret))
})
