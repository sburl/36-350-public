# **2a.** Write a function `generate_data(n, p)` which returns a list with the following elements: 
# `covariates` which is a n-by-p matrix of draws from the standard normal distribution, and `responses`
# which is a vector of length n of draws from the standard normal.

generate_data = function(n, p) {
  
  covariates = matrix(rnorm(n * p), nrow = n, ncol = p)
  responses = rnorm(n)
  
  results = list(coveriates = covariates, responses = responses)
  return (results)
  
}