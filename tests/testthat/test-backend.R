# test-backend.R

test_that("C++ backtest calculates cumulative equity correctly", {
  rets <- c(0.01, 0.02, -0.01, 0.05)
  sigs <- c(1, -1, 0, 1) # Note: these are passed post-lag
  
  # eq[1] = 1 * (1 + 0.01*1) = 1.01
  # eq[2] = 1.01 * (1 + 0.02*-1) = 1.01 * 0.98 = 0.9898
  # eq[3] = 0.9898 * (1 + -0.01*0) = 0.9898
  # eq[4] = 0.9898 * (1 + 0.05*1) = 0.9898 * 1.05 = 1.03929
  
  expected <- c(1.01, 0.9898, 0.9898, 1.03929)
  actual <- cpp_backtest(rets, sigs)
  
  expect_equal(actual, expected)
})

test_that("run_backtest correctly lags signals to prevent lookahead bias", {
  rets <- c(0.01, 0.02, -0.01, 0.05)
  # Pre lag:
  # Day 1: buy
  # Day 2: short
  # Day 3: hold
  # Day 4: buy
  sigs <- c(1, -1, 0, 1)
  
  # Expected Lagged sigs sent to c++: c(0, 1, -1, 0)
  # eq[1] = 1 * (1 + 0.01*0) = 1
  # eq[2] = 1 * (1 + 0.02*1) = 1.02
  # eq[3] = 1.02 * (1 + -0.01*-1) = 1.02 * 1.01 = 1.0302
  # eq[4] = 1.0302 * (1 + 0.05*0) = 1.0302
  
  expected <- c(1.0000, 1.0200, 1.0302, 1.0302)
  actual <- run_backtest(rets, sigs)
  
  expect_equal(actual, expected)
})
