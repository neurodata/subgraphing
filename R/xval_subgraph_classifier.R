#' Cross-Validated Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.xval_classifier} Bernoulli Subgraph Classifier with Cross Validation to determine the optimal subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x m].
#'    - if samp is an array, then it should be of dimensions [n x m x s].
#' @param Y [s] the class labels.
#' @param es [z] the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE a logical indicating whether to approximate a coherent, or incoherent, subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @param xval="loo" the cross-validation options to use. Options are "loo" (leave-one-out).
#' @return subgraph [n x m] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x m x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.xval_classifier <- function(samp, Y, es, coherent=FALSE, tstat="fisher", xval="loo") {

  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }
  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]

  if (s != length(Y)) {
    stop(sprintf(paste('The number of samples, ', s, "is not equal to\n",
                       " the number of class labels, ", length(Y), '.', sep="")))
  }

  if (xval == "loo") {
    for (i in 1:s) {
      # split into training and validation sets
      splits <- xval_split_inputs(samp, Y, i)
      train_dat <- splits$train_dat
      train_yhat <- splits$train_yhat
      val_dat <- splits$val_dat
      val_yhat <- splits%val_yhat

      # estimators for graph
      sg_ests <- sg.bern.subgraph_estimator(train_dat, train_yhat)
      # compute test statistics per edge from the contingency matrix
      tstats <- sg.bern.edge_test(sg_ests$cont_matrix, test_stat="fisher")

    }
  } else {
    stop('You have passed an unsupported cross-validatioon method.')
  }
}
