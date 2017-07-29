#' Bernoulli Subgraph Estimator
#'
#' \code{sg.beta.subgraph_estimator} A function to produce estimators for bernoulli graphs needed to compute subgraphs.
#'
#' @param samp  [n x n x s] an array of graph samples. Should be binary (0 or 1).
#' @param Y [s] the class labels.
#' @return cont_matrix [n, n, c, 2] a contingency table for each edge, where each contingency
#'         table has either connected or unconnected for each class.
#' @export
#' @seealso \code{\link{sg.bern.graph_estimator}}
#'
sg.bern.subgraph_estimator <- function(samp, Y) {
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

  classes <- unique(Y)  # classes are our unique class labels
  C <- length(classes)

  cont_matrix <- array(NaN, dim=c(n, n, C, 2))
  p = array(NaN, dim=c(n, n, C))
  pi = array(NaN, dim=c(C))

  # make the contingency matrix
  for (k in 1:C) {
    class <- classes[k]
    idx_class <- (Y == class)
    # class sample is where the class label is the current class
    class_samp <- samp[,, idx_class]
    # add current clas to the contingency table
    cont_matrix[,, k, 1] <- apply(class_samp, c(1, 2), sum)  # connected edges are the number of 1-valued edges
    cont_matrix[,, k, 2] <- sum(idx_class) - cont_matrix[,, k, 1]  # disconnected edges

    # estimate p for the current class
    p[,, k] <- sg.bern.graph_estimator(class_samp, smooth=TRUE)$p
    pi[k] <- sum(1*idx_class)/s
  }

  return(list(cont_matrix=cont_matrix, p=p, pi=pi, classes=classes))
}
