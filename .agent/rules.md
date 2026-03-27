# DataApp R Project Rules

## Tech Stack & Architecture
- **Language**: R 4.4+
- **Framework**: Shiny with `golem` package structure
- **Mock Data**: Use `shinipsum` to generate prototype visualizations and data structures.

## Coding Standards & Best Practices
1. **Style**: Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code.
2. **Modules**: Write UI and Server logic in modular files (`R/mod_*.R`). Never let `app_ui.R` and `app_server.R` grow too large.
3. **Business Logic**: Keep complex data transformations and computational logic separate from Shiny server logic. Place these functions in `R/fct_*.R` and `R/utils_*.R`.
4. **Dependencies**: All package dependencies must be explicitly declared in the `DESCRIPTION` file using `usethis::use_package()`. Do not use `library()` or `require()` calls inside application scripts. Instead, use the format `package::function()`.
5. **Testing**: Add tests using the `testthat` framework via `usethis::use_test()`.
6. **Documentation**: Add documentation using the `roxygen2` framework via `usethis::use_roxygen_tag()`. Ensure documentation is clear, concise, and easy to understand. Add comments to all formatting lines (such as colour tags, graph parameters, etc.) to ensure that the code is easy to modify.
7. **Phases**: All completed phases should result in a running app with noticeable and testable changes.
8. **Comments**: Add comments to all sections of code, formatting lines, and any line that may be modified in the future. Ensure comments are as detailed and clear as possible.
9. **Small r Strategy (SrS)**: Do not use `return(reactive({...}))` to pass data between modules. Instead, initialize a global `r <- shiny::reactiveValues()` dictionary in `app_server.R` and unconditionally pass `r` as an argument to every module server. Modules communicate purely by mutating and observing fields within `r` natively (e.g., `r$payload <- data`, `observeEvent(r$payload, {...})`). Test environments should follow suit structurally using `shiny::testServer(args = list(r = shiny::reactiveValues()))`.
10. **Explicit Namespacing**: Enforce the `package::function()` syntax unconditionally for all external package functions (e.g., `shiny::reactive()`, `dplyr::filter()`, `bslib::card()`). Do not attach libraries globally via `library()` or `require()`.

## Workflows
- **Running the App**: Use `devtools::load_all()` followed by `run_app()` to run locally during development.
- **Checking**: Treat this app like an R package. Code must pass `devtools::check()` without errors before being merged.
