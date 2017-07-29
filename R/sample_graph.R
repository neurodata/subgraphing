#' Sample Graphs from Beta Distribution
#'
#' \code{sg.beta.sample_graph} A function to sample from a graph-valued RV where each edge follows a beta distribution.
#'
#' @param alpha a [n x n] matrix indicating the alphas of each edge.
#' @param beta a [n x n] matrix indicating the betas of each edge.
#' @param s the number of graphs to sample.
#' @param type an option of "list" or "array" (default) for the format of the output.
#' @return sample the graph observations from the given beta distribution.
#'     - if type == "list", returns a p element list of [n x n] observations.
#'     - if type == "array" (default), returns a [n x n x s] element array where the 3rd dimension indexes the observations.
#' @examples
#' @export
#' @seealso \code{\link{list2array}} \code{\link{array2list}}
#'
sg.beta.sample_graph <- function(alpha, beta, s=10, type="array") {
  dims <- dim(alpha)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (any(dim(alpha) != dim(beta))) {
    stop('Your alpha and beta do not have the same dimensions.')
  }
  samp <- array(NaN, dim=c(n, m, s))
  for (i in 1:n) {
    for (j in 1:m) {
      samp[i, j,] <- rbeta(s, alpha[i, j], beta[i, j])
    }
  }
  if (type == "array") {
    return(samp)
  } else if (type == "list") {
    return(array2list(samp))
  } else {
    stop(sprintf(paste("You have entered an invalid type %s.",
                       "Choices are \"list\" or \"array\"."), type))
  }
}
#' Sample Graphs from Bernoulli Distribution
#'
#' \code{sg.bern.sample_graph} A function to sample from a graph-valued RV where each edge follows a bernoulli distribution.
#'
#' @param p a [n x n] matrix indicating the alphas of each edge.
#' @param n the number of graphs to sample.
#' @param type an option of "list" or "array" (default) for the format of the output.
#' @return sample the graph observations from the given beta distribution.
#'     - if type == "list", returns a p element list of [n x m] observations.
#'     - if type == "array" (default), returns a [n x m x s] element array where the 3rd dimension indexes the observations.
#' @examples
#' @export
#' @seealso \code{\link{list2array}} \code{\link{array2list}}
#'
sg.bern.sample_graph <- function(p, s=10, type="array") {
  dims <- dim(p)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  samp <- array(runif(n=n*m*s), dim=c(n, m, s))

  # if x[,,i] < p, edge gets a 1, 0 otherwise
  # multiply by 1 to cast the logical array to numeric
  # apply over the third dimension (number of subjects) and reshape
  samp <- array(apply(samp, 3, function(x) 1*(x < p)), dim=c(n, m, s))

  if (type == "array") {
    return(samp)
  } else if (type == "list") {
    return(array2list(samp))
  } else {
    stop(sprintf(paste("You have entered an invalid type %s.",
                       "Choices are \"list\" or \"array\"."), type))
  }
}
