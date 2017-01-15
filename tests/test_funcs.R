# unit tests for bradford repo functions
library(testthat)

source("../functions/bayesian_functions.R")

context("Bayesian Functions")
test_that("betaPosterior outputs the correct vector", {
  expect_equal_to_reference(betaPosterior(prior.mean = .5, prior.n = 10, sample.n = 100, affirm.n = 50), 
                            "test_data/beta_posterior.RDS")
})
  expect_equal_to_reference(betaPosterior(prior.mean = .5, prior.n = 10, sample.n = 100, affirm.n = 50), 
                            "test_data/beta_posterior.RDS")
})

test_that("betaPosteriorMean outputs the correct scalar", {
  expect_equal(betaPosteriorMean(prior.mean = .5, prior.n = 10, sample.n = 100, affirm.n = 50), .5)
})
