library(testthat)
library(lintr)

if (requireNamespace("lintr", quietly = TRUE)) {
  context("lints")
  test_that("Code is Lint Free", {
    x <- list.files(path = "../", pattern = ".R$",
                    full.names = T, recursive = T, include.dirs = T) %>%
      lapply(function(x) lintr::lint(x)) %>%
      {
        lint.count <<- purrr::map(., length)
        return(.)
      }
    if (sum(unlist(x)) > 0) {
      print(c("Please lint and fix the following files:", unique(x$filename)))
    }
    expect_equal(sum(unlist(x)), 0)
  })
}
