generate_data = function(n, p) {
  
  covariates = matrix(rnorm(n * p), nrow = n, ncol = p)
  responses = rnorm(n)
  
  results = list(coveriates = covariates, responses = responses)
  return (results)
  
}

# Write a function `model_select(covariates, responses, cutoff)` which fits the linear regression 
# `responses ~ covariates` and retains only those covariates whose coefficient p-values are less 
# than or equal to `cutoff`. Then fit another regression using only the retained covariates and 
# return the p-values from this reduced model. If there are no retained covariates return an empty
# vector. *HINT*: You can use indexing inside of formulas: `lm(responses ~ covariates[, c(1, 2)])` 
# will fit a regression with only the first two covariates.

model_select = function(covariates, responses, cutoff) {
  
  coefs = summary(lm(responses ~ covariates))["coefficients"]
  pvals = coefs[,4]
  keep = which(pvals <= cutoff)
  
  if (length(keep) == 0) { return (keep) }
  
  keep.coefs = summary(lm(responses ~ covariates[, keep]))["coefficients"]
  keep.vals = keep.coefs[,4]
  
  return (keep.vals)
    
}