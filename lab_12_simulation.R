# **2a.** Write a function `generate_data(n, p)` which returns a list with the following elements: 
# `covariates` which is a n-by-p matrix of draws from the standard normal distribution, and `responses`
# which is a vector of length n of draws from the standard normal.

generate_data = function(n, p) {
  
  covariates = matrix(rnorm(n * p), nrow = n, ncol = p)
  responses = rnorm(n)
  
  results = list(coveriates = covariates, responses = responses)
  return (results)
  
}

# **2b** Write a function `model_select(covariates, responses, cutoff)` which fits the linear regression 
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

# **2c.** Write a function `run_simulation(n_trials, n, p, cutoff)` which uses the previous two functions to 
# run `n_trials` simulations which uses data from `generate_data` in `model_select`, collects the returned 
# p-values and displays a histogram of the p-values. Under the null hypothesis (that the regression coefficients 
# are zero) these p-values should be uniformly distributed between 0 and 1; does this seem to be the case? 
# Display figures for all combinations of `n = c(100, 1000, 10000)`, `p = 10, 20, 50` and set `cutoff = 0.05`.

run_simulation = function(n_trials, n, p, cutoff) {
  
  pvals = vector(mode = "list", length = n_trials)
  
  # run trials 
  for (i in 1:n_trials) {
    
    run = generate_data(n, p)
    pvals[i] = model_select(run$covariates, run$responses, run$cutoff)
    
  }
  
  save(pvals, file = "pvalues.rdata")
  
}

# **2d.** One problem with simulations is that they can take a long time to run. This can be a problem if you 
# merely want to change the resulting figure (maybe there was a typo). Thus a better strategy is to separate 
# the simulation and plotting functions. Edit your previous code to save the p-values to a file and add a 
# function `make_plot(datapath)` which reads the data from `datapath` and makes the plot.

make_plot = function (datapath) {
  
  load(datapath)
  hist(x = pvals, main = "P-Value Distribution", xlab = "P-Values", ylab = "Frequency")
  
}



