context("Code style coherence")

skip_if("style" %in% skipped_tests, message = "Tested only in development")

if (requireNamespace("lintr", quietly = TRUE)) {
  test_that("Package has lintr-compliant code style", {
    lintr::expect_lint_free(
      linters = lintr::with_defaults(
        line_length_linter = lintr::line_length_linter(120),
        lintr::object_name_linter("snake_case")
      ),
      exclusions = list("../../R/derivatives_stochastic_adjustment.R")
    )
  })
}
