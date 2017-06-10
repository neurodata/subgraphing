#' A function to fit a beta distribution to the edges of the matrices in a graph.
#'
#' \code{beta_graph_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a collection of graphs.
#'
#' @param object a list or array of graphs with arbitrary labelling.
#'     - if object is a list, then it should have p elements of dimensions
#'         [n x m].
#'    - if object is an array, then it should be of dimensions [n x m x p].
#' @return params$alpha the alpha parameter per edge. [n x m]
#' @return params$beta the beta parameter per edge. [n x m]
#' @examples
#' @export
#' @seealso \code{\link{beta_graph_estimator}}
#'
beta_graph_estimator <- function(sample) {
  if(!is.list(object)) {
    sample <- list2array(sample)  # convert to a list
  }
  dims <- dim(object)
  n <- dims[1]
  m <- dims[2]
  p <- dims[3]
  alpha <- array(NaN, dim=c(n, m))
  beta <- array(NaN, dim=c(n, m))
  for (i in 1:n) {
    for (j in 1:m) {
      edge_params <- beta_estimator(sample[i,j,])
      alpha[i, j] <- edge_params$alpha
      beta[i, j] <- edge_params$beta
    }
  }
  return(list(alpha=alpha, beta=beta))
}

