#' A function to fit a beta distribution to the edges of the matrices in a graph.
#'
#' \code{sg.beta.graph_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a collection of graphs.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if samp is an array, then it should be of dimensions [n x m x s].
#' @return params$alpha the alpha parameter per edge. [n x m]
#' @return params$beta the beta parameter per edge. [n x m]
#' @examples
#' @export
#' @seealso \code{\link{sg.beta.estimator}}
#'
sg.beta.graph_estimator <- function(samp) {
  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }
  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]
  alpha <- array(NaN, dim=c(n, m))
  beta <- array(NaN, dim=c(n, m))
  for (i in 1:n) {
    for (j in 1:m) {
      if ((max(samp[i,j,]) > 1) || (min(samp[i,j,]) < 0)){
        stop(sprintf('Your samp is not between 0 and 1 for edge: (%d, %d)', i, j))
      }
      edge_params <- sg.beta.estimator(samp[i,j,])
      alpha[i, j] <- edge_params$alpha
      beta[i, j] <- edge_params$beta
    }
  }
  return(list(alpha=alpha, beta=beta))
}

