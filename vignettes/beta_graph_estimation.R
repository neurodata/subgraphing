## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  # A function to estimate a beta distribution for each edge in a graph.
#  # Inputs
#  #   sample: a [n x m x s] element array, where we have s observations of nxm graphs.
#  # Outputs
#  #   alpha: a [n x m] matrix denoting the alpha parameter per edge.
#  #   beta: a [n x m] matrix denoting the beta parameter per edge.
#  sg.beta.graph_estimator(sample):
#    for u in 1:n:
#      for v in 1:m:
#        alpha[u, v], beta[u, v] = beta_est(sample[u, v, :])  # estimate the parameters given the u, v edge from all samples
#    return alpha, beta
#  
#  # a function to estimate a beta distribution for a sample.
#  # Inputs
#  #   sample: a [s] element vector that is between 0 and 1.
#  # Outputs
#  #   alpha: the alpha parameter of the method of moments estimate given the sample.
#  #   beta: the beta parameter of the method of moments estimate given the sample.
#  sg.beta.estimator(sample):
#    alpha = ((1 - mu)/sig^2 - 1/mu)*mu^2
#    beta = alpha*(1/mu - 1)
#    return alpha, beta
#  
#  # a function to generate random beta-distributed samples.
#  # Inputs
#  #   alpha: a [n x m] matrix of the alpha parameters per edge.
#  #   beta: a [n x m] matrix of the beta parameters per edge.
#  #   p: the number of samples.
#  # Outputs
#  #   samp: a [n x m x p] array sampling from the [n x m] graph RV p times.
#  sg.beta.sample_graph(alpha, beta, p):
#    for (i in 1:n) {
#      for (j in 1:m) {
#        samp[i,j,] <- rbeta(s, alpha[i, j], beta[i, j]) # samples from random beta distribution with alpha[i,j] beta[i,j]
#      }
#    }
#    return samp

