generate_data = function(n, p) {
  
  covariates = matrix(rnorm(n * p), nrow = n, ncol = p)
  responses = rnorm(n)
  
  results = list(coveriates = covariates, responses = responses)
  return (results)
  
}