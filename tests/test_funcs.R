# unit tests for bradford repo functions
library(testthat)

source("../functions/bayesian_functions.R")

context("Bayesian Functions")

if (!file.exists("test_data/mean_sample_value_grid.RDS")) {
  x <- expand.grid(prior.mean = seq(.1, 1, by = .1),
                   prior.n = seq(0, 20, by = 1),
                   sample.n = seq(1, 20, by = 1),
                   affirm.n = seq(0, 20, by = 1)) %>%
    dplyr::filter(affirm.n <= sample.n)
    saveRDS(x, "test_data/mean_sample_value_grid.RDS")
} else {
  x <- readRDS("test_data/mean_sample_value_grid.RDS")
}

test_that("betaPosterior outputs the correct vector", {
  expect_equal_to_reference(betaPosterior(prior.mean = .5,
                                          prior.n = 10,
                                          sample.n = 100,
                                          affirm.n = 50),
                            "test_data/beta_posterior.RDS")
})

test_that("betaPosteriorMean outputs the correct scalar", {
  expect_equal(betaPosteriorMean(prior.mean = .5, prior.n = 10, sample.n = 100, affirm.n = 50), .5)
})

test_that("betaPosteriorMean returns non-negative results", {
  x %>%
    apply(MARGIN = 1, FUN = function(x) betaPosteriorMean(prior.mean = x[["prior.mean"]],
                                                          prior.n = x[["prior.n"]],
                                                          sample.n = x[["sample.n"]],
                                                          affirm.n = x[["affirm.n"]]))
  expect_false(any(x < 0))
})

#test_that("betaVariance returns non-negative results", {
#  x %>%
#    apply(MARGIN = 1, FUN = function(x) betaVariance(prior.mean = x[["prior.mean"]],
#                                                        prior.n = x[["prior.n"]],
#                                                        sample.n = x[["sample.n"]],
#                                                        affirm.n = x[["affirm.n"]], prior = F))
#
#  expect_false(any(x < 0))
#})

test_that("betaVariance errors on incorrect input", {
  expect_error(betaVariance(prior.mean = "1", prior.n = 2, sample.n = 3, affirm.n = 2, prior = F))
  expect_error(betaVariance(prior.mean = 1, prior.n = NA, sample.n = 3, affirm.n = 2, prior = F))
  expect_error(betaVariance(prior.mean = 1, prior.n = NA, prior = T))
  expect_error(betaVariance(prior.mean = 1, prior.n = 2, sample.n = 3, affirm.n = "2", prior = F))
  expect_error(betaVariance(prior.mean = 1, prior.n = 2, sample.n = 3, affirm.n = 20, prior = F))
})
