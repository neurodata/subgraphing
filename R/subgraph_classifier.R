#' Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.subgraph_classifier} classifies arbitrary data given the estimators of a subgraph.
#'
#' @param test the test array to generate labels for.
#'     - if test is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if test is an array, then it should be of dimensions [n x n x s].
#' @param p [n x n x c] the probability per edge of being connected per class for c classes.
#' @param pi [c] the pi vector probability of each class occuring.
#' @param classes the labels for the classes as ordered in the pi and p arrays.
#' @return edges [e] the edges present in the subgraph.
#' @return Yhat [s] the predicted class for each test example.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_classifier <- function(test, edges, p, pi, classes) {

  s <- dim(test)[3]
  c <- dim(p)[3]
  e <- length(edges)

  hyp <- array(NaN, dim=c(s, e, c))
  for (k in 1:c) {
    pclass <- p[,,k]  # current probability matrix
    for (i in 1:s) {
      csamp <- test[,,i]  # current sample
      for (j in 1:e) {  # iterate over subgraph edges
        eix <- edges[j]  # id of the edge
        hyp[i, j, k] <- pclass[eix]^csamp[eix]*(1 - pclass[eix])^(1 - csamp[eix])*pi[k]
      }
    }
  }

  h <- apply(hyp, c(1, 3), prod)  # product over the edges in the subgraph, the 3rd dimension

  Yhat <- apply(h, c(1), FUN = function(x) {
    classes[sort(x, decreasing=TRUE, index.return=TRUE)$ix[1]]
  })

  return(list(Yhat=Yhat))
}
