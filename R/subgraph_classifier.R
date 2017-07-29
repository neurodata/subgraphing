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
#' @param e [1] the number of edges in the subgraph.
#' @param classes the labels for the classes as ordered in the pi and p arrays.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @return edges [e] the edges present in the subgraph.
#' @return Yhat [s] the predicted class for each test example.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_classifier <- function(test, stats, p, pi, e, classes, coherent=FALSE) {

  s <- dim(test)[3]
  c <- dim(p)[3]

  # estimate the ordering of edges depending on the test statistic results
  edges <- sg.bern.subgraph_edge_estimation(stats, e, coherent=coherent)

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

  # return class label whwere Yhat indicates the position of the respective label
  Yhat <- sapply(Yhat, function(x) classes[x], simplify=TRUE)

  return(list(edges=edges, Yhat=Yhat))
}
