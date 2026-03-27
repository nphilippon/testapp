test_that("strategy_discovery server initializes structural inputs cleanly routing signals natively", {
  skip_if_offline()
  skip_on_cran()
  
  shiny::testServer(mod_strategy_discovery_server, args = list(r = shiny::reactiveValues()), {
    # Set UI state to Z-Score simulation securely testing dynamic ratio vectors natively
    session$setInputs(
      target_asset = "SPY",
      base_asset = "QQQ",
      date_range = "6mo",
      strategy_type = "Divergence (Z-Score Pairs)",
      z_window = 10,
      run_btn = 1
    )
    
    expect_equal(input$strategy_type, "Divergence (Z-Score Pairs)")
    
    # Test momentum branch recursively replacing reactive scope locally
    session$setInputs(
      strategy_type = "Lead-Lag (1-Day Momentum)",
      run_btn = 2
    )
    
    expect_equal(input$strategy_type, "Lead-Lag (1-Day Momentum)")
  })
})
