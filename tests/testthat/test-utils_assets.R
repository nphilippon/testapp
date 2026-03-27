test_that("asset_choices maintains valid UI mapping structure safely", {
  # Test the nested naming parameters format correctly targeting shiny inputs
  expect_type(asset_choices, "list")
  expect_true(length(asset_choices) > 0)
  
  # Mapping assertion validations
  expect_true("S&P 500 (SPY)" %in% names(asset_choices[["Broad Market Indices"]]))
  
  # Scan through universally mapping nested inputs ensuring strictly designated naming logic
  for (category in names(asset_choices)) {
    items <- asset_choices[[category]]
    
    expect_type(items, "character")
    # Validate every choice internally possesses an explicit visible name label internally natively natively natively appropriately correctly structured natively appropriately safely cleanly robustly
    expect_true(length(names(items)) == length(items))
    expect_true(!any(names(items) == ""))
  }
})
