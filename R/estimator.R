#' A function to fit a beta distribution to a given univariate samp of data.
#'
#' \code{sg.beta.estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a given samp.
#'
#' @param samp a univariate samp of values with arbitrary class labels.
#' @return alpha [n x m] the alpha parameter per edge.
#' @return beta [n x m] the beta parameter per edge.
#' @examples
#' @export
#' @seealso \code{\link{sg.beta.graph_estimator}}
#'
sg.beta.estimator <- function(samp) {
  if ((max(samp) > 1) || (min(samp) < 0)){
    stop('Your samp is not between 0 and 1.')
  }
  mu_samp <- mean(samp)
  sig_samp <- sd(samp)
  alpha <- ((1 - mu_samp)/sig_samp^2 - 1/mu_samp)*mu_samp^2
  beta <- alpha*(1/mu_samp - 1)
  return(list(alpha=alpha, beta=beta))
}

#' A function to fit a bernoulli distribution to a given univariate samp of data.
#'
#' \code{sg.bern.estimator} estimate the parameter of a bernoulli distribution, p, for a given samp.
#'
#' @param samp a univariate samp of values with arbitrary class labels.
#' @param thresh=0: is the threshold below which we set edges to disconnected, and above which we set edges to connected.
#' @param smooth=TRUE: whether to smooth p to avoid undesirable limits.
#' @return p the p parameter per edge.
#' @examples
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}
#'
sg.bern.estimator <- function(samp, thresh=0, smooth=TRUE) {
  samp <- ifelse(samp > thresh, 1, 0)  # force binarize
  s = length(samp)
  p=sum(samp)/s  # p is the number of 1s divided by number of samples total by defn
  if (smooth) {  # smooth if we only have 1 category so that we don't have p=0 or p=1, which has poor asymptotic behavior
    np <- 1/(10*s)
    p <- ifelse(p == 0, np, p)
    p <- ifelse(p == 1, 1 - np, p)
  }
  return(list(p=p))
}
