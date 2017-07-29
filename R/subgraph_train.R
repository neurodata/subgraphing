#' Bernoulli Subgraph Train
#'
#' \code{sg.bern.xval_classifier} Trains a Model for identification of the edges in a subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @return edges [n x n] an ordering of the edges in the subgraph by test statistic results.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @return pi [c] the probability of seeing a given class.
#'
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_train <- function(samp, Y, tstat="fisher") {
  # estimate basic parameters for the subgraph
  sg_ests <- sg.bern.subgraph_estimator(samp, Y)
  # compute test statistics per edge from the contingency matrix
  tstats <- sg.bern.edge_test(sg_ests$cont_matrix, test_stat=tstat)
  return(list(stats=tstats, p=sg_ests$p, pi=sg_ests$pi, classes=sg_ests$classes))
}
