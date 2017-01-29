# Bayesian funcs
# perhaps create a calculateAlpha/calculateBeta function
betaVariance <- function(prior.mean, prior.n, sample.n = "", affirm.n = "", prior = T) {
  # calculates the variance of a beta given a mean and "n".
  # Can handle priors as well as posteriors given additional input
  # Args:
  #   prior.mean = the a priori prior mean
  #   prior.n = the a priori prior "n" or sample size
  #   df = (only for posterior)data frame containing sample.n and affirm.n for posterior calculation
  #   sample.n = (only for posterior) name of vector containing the sample size as character
  #   affirm.n = (only for posterior) name of vector containing the n affirming as character
  #   prior = boolean whether we are calculating the variance of a prior or posterior
  if (prior) {
    if (!any(is.numeric(prior.mean) & is.numeric(prior.n))) {
      stop("args must be numeric")
    }
    prior.mean * (1 - prior.mean) / (1 + prior.n)
  } else {
    if (!any(purrr::map(list(prior.mean, prior.n, affirm.n, sample.n), is.numeric ))) {
      stop("args must be numeric")
    } else if (affirm.n > sample.n) {
      stop("affirm.n > sample.n")
    }
    a = affirm.n + (prior.n * prior.mean) - 1
    b = sample.n - affirm.n + (prior.n * (1 - prior.mean)) - 1
    a * b / ((a + b) ^ 2 * (a + b + 1))
  }
}

betaPosterior <- function(prior.mean, prior.n, sample.n = "", affirm.n = "") {
  # calculates an approximate beta posterior given the mean and n of a beta prior as well as
  # the n and n sucesses from a binomial
  # Args:
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution
  #   prior.n = support for the prior
  #   sample.n = n observations in the treatment population
  #   affirm.n = n successes in treatment population
  # Returns: a data frame which approximates the posterior distribution

  if (sample.n < affirm.n) {  # if sample.n is less than n affirming
    stop("sample.n < affirm.n")
  }
  a = affirm.n + (prior.n * prior.mean) - 1
  b = sample.n - affirm.n + (prior.n * (1 - prior.mean)) - 1
  domain = seq(0, 1, 0.005)
  val = dbeta(domain, a, b)
  data.frame("domain" = domain,
             "prob_dens" = val
    )
}

betaPosteriorMean <- function(prior.mean, prior.n, sample.n = "", affirm.n = "") {
  # calculates the mean of the beta posterior given the mean and n of a beta prior as well as
  # the n and n sucesses from a binomial
  # Args:
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution
  #   prior.n = support for the prior
  #   sample.n = n observations in the treatment population
  #   affirm.n = n successes in treatment population
  # Returns: a float, the mean of the posterior
  a = affirm.n + (prior.n * prior.mean) - 1
  b = sample.n - affirm.n + (prior.n * (1 - prior.mean)) - 1
  a / (a + b)
}
