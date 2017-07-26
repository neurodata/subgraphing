#' A function to fit a beta distribution to a given univariate samp of data.
#'
#' \code{sg.beta.estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a given samp.
#'
#' @param samp a univariate samp of values with arbitrary class labels.
#' @return params$alpha the alpha parameter per edge. [n x m]
#' @return params$beta the beta parameter per edge. [n x m]
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
