# unit tests for bradford repo functions
library(testthat)

source("bayesian-explorer/functions/bayesian_functions.R")

context("Bayesian Functions")
test_that("betaPosterior outputs the correct vector", {
  x <- readRDS(file = "bayesian-explorer/tests/test_data/test_post_df.RDS")
  expect_equal_to_reference(betaPosterior(x, prior.mean = .5, prior.n = 10, sample.n = "sample.n", affirm.n = "affirm.n"),
                            "bayesian-explorer/tests/test_data/test_posterior.RDS")
})

test_that("betaPosteriorMean outputs the correct scalar", {
  x <- readRDS(file = "bayesian-explorer/tests/test_data/test_post_df.RDS")
  expect_equal_to_reference(betaPosteriorMean(x, prior.mean = .5, prior.n = 10, sample.n = "sample.n", affirm.n = "affirm.n"),
                            "bayesian-explorer/tests/test_data/test_posterior_mean.RDS")
})
