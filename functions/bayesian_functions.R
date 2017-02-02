# Bayesian funcs
# perhaps create a calculateAlpha/calculateBeta function
betaVariance <- function(prior.mean, prior.conc, sample.n = "", affirm.n = "", prior = T) {
  # calculates the variance of a beta given a mean and concentration k = (a + b)
  # Can handle priors as well as posteriors given additional input
  # Args:
  #   prior.mean = the a priori prior mean
  #   prior.conc = the a priori prior concentration k = (a + b)
  #   df = (only for posterior) data frame containing sample.n and affirm.n for posterior calculation
  #   sample.n = (only for posterior) number of sample observations
  #   affirm.n = (only for posterior) number of successes in sample observations
  #   prior = boolean whether we are calculating the variance of a prior or posterior
  if (prior) {
    if (!any(is.numeric(prior.mean) & is.numeric(prior.conc))) {
      stop("args must be numeric")
    }
    prior.mean * (1 - prior.mean) / (1 + prior.conc)
  } else {
    if (!any(is.numeric(prior.mean) & is.numeric(prior.conc) & is.numeric(affirm.n) & is.numeric(sample.n))) {
      stop("args must be numeric")
    } else if (affirm.n > sample.n) {
      stop("affirm.n > sample.n")
    }
    a = affirm.n + (prior.conc * prior.mean)
    b = sample.n - affirm.n + (prior.conc * (1 - prior.mean))
    a * b / ((a + b) ^ 2 * (a + b + 1))
  }
}

# possible approximation to factorial(), but still breaks down for large n
#gosper <- function(n) {
#  sqrt((2*n + (1/3))*pi) * (n^n) * exp(-n)
#}

betaPosteriorDensity <- function(a, b, n, k, domain) {
  # Calculates the probability density values for the beta posterior (adjusted by appropriate constant).
  # Args:
  # a = (positive real number) beta shape parameter alpha
  # b = (positive real number) beta shape parameter beta
  # n = number of observations in the treatment population
  # k = number of successes in the treatment population
  # domain = ordered row vector containing points in the domain
  beta.sum = 0
  beta.dens = matrix(0, length(domain), 1)
  
  # While beta cdf max is less than 1, keep updating pdf of beta
  for (i in 1:length(beta.dens)) {
    # New constant: will need a better alternative to gamma for large parameter values
    constant = choose(n, k) * ( gamma(a + b) * gamma(a + k) * gamma(b - k + n) /
      ( gamma(a) * gamma(b) * gamma(a + b + n) ) )
    
    # beta.dens[i] = constant * x^(k + a - 1) * (1 - x)^(n - k + b - 1)
    beta.dens[i] = constant * dbeta(domain[i], a, b)
    beta.sum = beta.sum + beta.dens[i]
    
    if (beta.sum >= 1) {
      beta.dens[i] = beta.sum - 1
      return(beta.dens)
    }
  }
}

betaPosterior <- function(prior.mean, prior.conc, sample.n = "", affirm.n = "") {
  # calculates an approximate beta posterior given the mean and concentration k = (a + b) of a beta prior as well as
  # the n (# observations) and k sucesses from a binomial
  # Args:
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution
  #   prior.conc = prior concentration k = (a + b)
  #   sample.n = number of observations in the treatment population
  #   affirm.n = number of successes in treatment population
  # Returns: a data frame which approximates the posterior distribution

  if (sample.n < affirm.n) {  # if sample.n is less than n affirming
    stop("sample.n < affirm.n")
  }
  a = affirm.n + (prior.conc * prior.mean)
  b = sample.n - affirm.n + (prior.conc * (1 - prior.mean))
  domain = seq(0, 1, 0.005)
  val = betaPosteriorDensity(a, b, sample.n, affirm.n, domain)  
  data.frame("domain" = domain,
             "prob_dens" = val)
}

betaPosteriorMean <- function(prior.mean, prior.conc, sample.n = "", affirm.n = "") {
  # calculates the mean of the beta posterior given the mean and n of a beta prior as well as
  # the n (# observations) and k sucesses from a binomial
  # Args:
  #   df = a data frame which contains vectors of sample  sizes and number of successful trials
  #   prior.mean = mean of the prior distribution
  #   prior.conc = prior concentration k = (a + b)
  #   sample.n = number of observations in the treatment population
  #   affirm.n = number of successes in treatment population
  # Returns: a float, the mean of the posterior
  a = affirm.n + (prior.conc * prior.mean)
  b = sample.n - affirm.n + (prior.conc * (1 - prior.mean))
  a / (a + b)
}
