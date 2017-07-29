#' Bernoulli Subgraph Computation
#'
#' \code{sg.bern.compute_subgraph} estimates the edges for a subgraph in a given set of samples.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param e the number of edges for the subgraph.
#' @param coherent=FALSE a logical indicating whether to approximate a coherent, or incoherent, subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @return subgraph [n x n] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}  \code{\link{sg.bern.subgraph_estimator}} \code{\link{sg.bern.subgraph_estimator}}
#'
sg.bern.compute_graph_statistics <- function(samp, Y, e, coherent=FALSE, tstat="fisher") {
  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }
  dims <- dim(samp)


  if (tstat == "fisher") {
    tfunc <- fisher.test
  } else if (tstat == "chisq") {
    tfunc <- chisq.test
  } else {
    stop(sprintf(paste("You have entered an invalid test function. Options are:\n",
                       "\"fisher\", \"chisq\".", sep="")))
  }

  contingency <- sg.bern.contingency_matrix(samp, Y)
  cont_matrix <- contingency$cont_matrix
  p <- contingency$p
  pi <- contingency$pi

  test_results <- sg.bern.test_statistic(cont_matrix, tfunc)

  if (is_false(coherent)) {  # incoherent subgraph requested
    edge_idx <- sort(test_results, index.return=TRUE)$ix
  } else {
    stop("Coherent subgraph not implemented yet.")
  }

  return(list(subgraph=edge_idx, p=p, pi=pi))
}
