#' Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.subgraph_classifier} classifies arbitrary data given the estimators of a subgraph.
#'
#' @param test the test array to generate labels for.
#'     - if test is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if test is an array, then it should be of dimensions [n x m x s].
#' @param test_stats [n x m] the test-statistic results.
#' @param p [n x m x c] the probability per edge of being connected per class for c classes.
#' @param pi [c] the pi vector probability of each class occuring.
#' @param e the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE a logical indicating whether to approximate a coherent, or incoherent, subgraph.
#' @return edges [e] the edges present in the subgraph.
#' @return Yhat [s] the predicted class for each test example.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_classifier <- function(test, test_results, p, pi, e, coherent=FALSE) {

  if(is.list(test)) {
    test <- fmriu.list2array(test)  # convert to a array for standardization
  }

  if (identical(coherent, FALSE)) {
    # incoherent estimator
    edges <- sort(test_results, index.return=TRUE)$ix[1:e]  # sort test statistics in ascending order; take the first e of them
  } else {
    stop("Coherent estimator not implemented yet.")
  }

  s <- dim(test)[3]
  c <- dim(p)[3]

  hyp <- array(NaN, dim=c(s, e, c))
  for (k in 1:c) {
    pclass <- p[,,k]  # current probability matrix
    for (i in 1:s) {
      csamp <- test[,,i]  # current sample
      for (j in 1:length(edges)) {  # iterate over subgraph edges
        hyp[i, j, k] <- pclass[j]^csamp[j]*(1 - pclass[j])^(1 - csamp[j])*pi[k]
      }
    }
  }

  h <- apply(hyp, c(1, 3), prod)  # product over the edges in the subgraph, the 3rd dimension

  Yhat <- apply(h, c(1), FUN = function(x) {
    sort(x, decreasing=TRUE, index.return=TRUE)$ix[1]
  })

  return(list(edges=edges, Yhat=Yhat))
}
