Error: neg_binomial_2 is not a supported family. Supported families are:

'acat', # Error: Family 'acat' requires either positive integers or ordered factors as responses.
'asym_laplace', # Residual issues
'bernoulli', # Error: Family 'bernoulli' requires responses to contain only two different values.
'beta', # Error in family() : argument "a" is missing, with no default
'binomial', # Residual issues
'categorical', # Residual issues
'com_poisson', # Not found
'cox', # Needs splines2 package
'cratio', # requires either positive integers or ordered factors as responses.
'cumulative', # requires either positive integers or ordered factors as responses.
'custom', # Not found
'dirichlet', # Categorical
'discrete_weibull', # Not found
'exgaussian', # Horrible residuals
'exponential', # Requires response greater than 0
'frechet', # Requires response greater than 0
'gamma', # Error in family() : 0 arguments passed to 'gamma' which requires 1
'gaussian', # Horrible resids
'gen_extreme_value', # Took too long to run
'geometric', # Residual issues
'hurdle_gamma', # Did not converge
'hurdle_lognormal', # Best so far
'hurdle_negbinomial', # WOuldn't work
'hurdle_poisson', # Did not converge
'info', #???
'inverse.gaussian', # Requires response greater than 0
'lognormal', # Requires response greater than 0
'multinomial', # Categorical data
'negbinomial', #resd issues
'poisson',  # Resid issues
'shifted_lognormal', # Requires response greater than 0
'skew_normal', # Terrible residuals
'sratio', # Requires either positive integers or ordered factors as responses
'student', # Resid issues but not as bad as some
'von_mises', # Requires response smaller than or equal to 3.14159265358979 !!
'weibull', # Requires response greater than 0
'wiener', # Requires additional argument?
'zero_inflated_asym_laplace', # Doesn't work
'zero_inflated_beta', # Requires response smaller than 1
'zero_inflated_binomial', # Resid issue
'zero_inflated_negbinomial', # Resid issue
'zero_inflated_poisson', # Resid issue
'zero_one_inflated_beta' # Resid issue

