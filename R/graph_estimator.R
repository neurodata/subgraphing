#' A function to fit a beta distribution to the edges of the matrices in a graph.
#'
#' \code{sg.beta.graph_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a collection of graphs.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if samp is an array, then it should be of dimensions [n x m x s].
#' @return alpha [n x m] the alpha parameter per edge.
#' @return beta [n x m] the beta parameter per edge.
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

#' A function to fit a bernoulli distribution to the edges of the matrices in a graph.
#'
#' \code{sg.bern.graph_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a collection of graphs.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if samp is an array, then it should be of dimensions [n x m x s].
#' @param thresh=0: is the threshold below which we set edges to disconnected, and above which we set edges to connected.
#' @param smooth=TRUE: whether to smooth p to avoid undesirable limits.
#' @return p [n x m] the p parameter per edge representing the probability of an edge existing.
#' @examples
#' @export
#' @seealso \code{\link{sg.bern.estimator}}
#'
sg.bern.graph_estimator <- function(samp, thresh=0, smooth=TRUE) {
  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }

  samp <- 1*samp
  samp <- ifelse(samp > thresh, 1, 0)  # binarize to 1 if greater than thresh; 0 else

  dims <- dim(samp)
  s <- dims[3]
  P <- apply(samp, c(1,2), sum)/s  # sum over third dimension and normalize by number of els for p per edge
  # smooth if desired
  if (smooth) {
    np <- 1/(10*s)
    P[P == 0] <- np
    P[P == 1] <- (1 - np)
  }
  return(list(p=P))
}
