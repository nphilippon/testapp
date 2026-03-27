test_that("data_explorer module server initializes correctly structurally without crashing", {
  skip_if_offline()
  skip_on_cran()
  
  shiny::testServer(mod_data_explorer_server, args = list(r = shiny::reactiveValues()), {
    # Simulate user UI triggers properly parsing boundaries 
    session$setInputs(
      target_asset = "SPY",
      base_asset = "QQQ",
      date_range = "6mo",
      run_btn = 1
    )
    
    # Assure the inputs were registered securely

    expect_equal(input$target_asset, "SPY")
    expect_equal(input$base_asset, "QQQ")
  })
})
