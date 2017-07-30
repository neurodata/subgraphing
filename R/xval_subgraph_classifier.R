#' Cross-Validated Bernoulli Subgraph Classifier
#'
#' \code{sg.bern.xval_classifier} Bernoulli Subgraph Classifier with Cross Validation to determine the optimal subgraph.
#'
#' @param samp a list or array of graphs with arbitrary labelling.
#'     - if samp is a list, then it should have s elements of dimensions
#'         [n x n].
#'    - if samp is an array, then it should be of dimensions [n x n x s].
#' @param Y [s] the class labels.
#' @param es [z] an array where each element is the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @param tstat="fisher" the test statistic to use. options are fisher's exact ("fisher") and chi-squared ("chisq").
#' @param xval="loo" the cross-validation options to use. Options are "loo" (leave-one-out) and "kfold" (K-fold).
#' @param folds=NaN the number of folds to do if xval is set to kfold.
#' @return subgraph [n x n] an array indicating whether an edge is present or not present in the subgraph.
#' @return p [n x n x c] the probability per edge of being connected per class for c classes.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.xval_classifier <- function(samp, Y, es, coherent=FALSE, tstat="fisher", xval="loo", folds=NaN) {

  if(is.list(samp)) {
    samp <- fmriu.list2array(samp)  # convert to a array for standardization
  }

  dims <- dim(samp)
  n <- dims[1]
  m <- dims[2]
  s <- dims[3]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (s != length(Y)) {
    stop(sprintf(paste('The number of samples, ', s, "is not equal to\n",
                       " the number of class labels, ", length(Y), '.', sep="")))
  }

  train_acc <- c()
  test_acc <- c()
  edges <- c()
  if (xval == "loo") {
    # iterate over the element that is going to be held out
    for (i in 1:s) {
      # split into training and validation sets
      splits <- sg.xval_split_data(samp, Y, i)
      train_set <- splits$train_set
      train_y <- splits$train_y
      test_set <- splits$test_set
      test_y <- splits$test_y

      # estimators for graph
      sg_ests <- sg.bern.subgraph_train(samp = train_set, Y = train_y, 6, coherent=coherent, tstat="fisher")
      for (e in es) {
        # classify the training data and produce accuracy summary
        train_pred <- sg.bern.subgraph_classifier(train_set, sg_ests$edges, sg_ests$p, sg_ests$pi, sg_ests$classes)
        tr_ac <- sg.prediction_accuracy(train_y, train_pred)
        train_acc <- c(train_acc, tr_ac)

        # classify the testing data and produce accuracy summary
        test_pred <- sg.bern.subgraph_classifier(train_set, sg_ests$p, sg_ests$pi, sg_ests$edges,
                                                 sg_ests$classes)
        test_ac <- sg.prediction_accuracy(test_y, test_pred)

        test_acc <- c(test_acc, test_ac)
        # record the edges used
        edges <- c(edges, e)
      }
    }
  } else {
    stop('You have passed an unsupported cross-validatioon method.')
  }
}
