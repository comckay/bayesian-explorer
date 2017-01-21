library(testthat)

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Code is Lint Free", {
    lintr::expect_lint_free()
  })
}