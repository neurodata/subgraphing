#' A function to fit a beta distribution to a given univariate sample of data.
#'
#' \code{beta_estimator} uses the method of moments to estimate the parameters of a beta
#' distribution, alpha and beta, for a given sample.
#'
#' @param sample a univariate sample of values with arbitrary class labels.
#' @return params$alpha the alpha parameter per edge. [n x m]
#' @return params$beta the beta parameter per edge. [n x m]
#' @examples
#' @export
#' @seealso \code{\link{beta_graph_estimator}}
#'
beta_estimator <- function(sample) {
  me_samp <- mean(sample)
  sig_samp <- sd(sample)
  alpha <- ((1 - me_samp)/sig_samp^2 - 1/me_samp)*me_samp^2
  beta <- alpha*(1/me_samp - 1)
  return(list(alpha=alpha, beta=beta))
}
