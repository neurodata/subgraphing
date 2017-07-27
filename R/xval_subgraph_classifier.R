#' Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.compute_graph_statistics} estimates the graph statistics necessary to compute a subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if samp is an array, then it should be of dimensions [n x m x s].
#' @param Y [s] the class labels.
#' @param  p [n x m x c] the probability per edge of being connected per class for c classes.
#' @param es [z] the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE a logical indicating whether to approximate a coherent, or incoherent, subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @return subgraph [n x m] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x m x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.xval_classifier <- function(samp, Y, test_stats, p, pi, es, coherent=FALSE, tstat="fisher") {

}
