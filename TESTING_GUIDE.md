# Commodity Strategy Discovery - Testing Guide

## Test Suite Overview

This application leverages the `testthat` package to ensure safe, reproducible builds and verify algorithmic integrity of backtesting math and Shiny module bindings.

The tests are physically separated into two categories inside the `tests/testthat/` folder:

1. **Unit Tests (`test-fct_*` / `test-utils_*`)**: These test native R execution logic decoupled from the UI. Mathematical strategies, signal processing, and lookahead-bias protections are verified here deterministically.
2. **Server/Module Tests (`test-mod_*`)**: These utilize `shiny::testServer()` to mock the execution scope of individual UI components. They programmatically populate UI `inputs` and assert expected reactive behaviors and output payload integrity mappings.

## Running Tests Locally

To run the entire test suite locally, open the R console in the project root and execute:

```R
devtools::test()
```

If you wish to test a specific file independently without triggering the full suite:

```R
devtools::test_active_file("tests/testthat/test-fct_engine.R")
```

## Adding New Tests

When contributing new features or modules:
1. If writing a pure R utility function, place the file in `R/` and create a corresponding `test-*.R` prefix file in `tests/testthat`.
2. Ensure mathematical tests map back to ground-truth expected vectors (manually calculated) rather than mirroring the internal logic directly.
3. Keep test scope isolated. Test modules independent of parent applications.

## API Mocking Disclaimer
Tests querying live external financial APIs (such as `tidyquant` fetching Yahoo Finance data) may occasionally fail if you hit rate limits. If this occurs, verify network connections and run `devtools::test()` again after a short suspension.
