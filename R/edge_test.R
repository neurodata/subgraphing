#' Edge Test
#'
#' \code{sg.bern.edge_test} A function to compute the test statistic per edge from a contingency matrix.
#'
#' @param cont_matrix [n x n x c x 2] a contingency table for each edge, where each contingency
#'         table has either connected or unconnected for each class.
#' @param test the test statistic function object to use. Options are "fisher" or "chisq".
#' @return test_stat [n x n] the test statistic per edge.
#' @export
#' @seealso \code{\link{sg.beta.subgraph_estimator}}
#'
sg.bern.edge_test <- function(cont_matrix, test_stat="fisher") {

  if (test_stat == "fisher") {
    tfunc <- fisher.test
  } else if (test_stat == "chisq") {
    tfunc <- chisq.test
  } else {
    print(paste("You have entered an invalid test function. Options are:\n",
                "\"fisher\", \"chisq\".", sep=""))
  }
  dims <- dim(cont_matrix)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  test_stats <- array(NaN, dim=c(n, n))
  for (i in 1:n) {
    for (j in 1:n) {
      # compute given test statistic on the contingency matrix for that edge
      test_stats[i, j] <- do.call(tfunc, list(cont_matrix[i, j,,]))$p
    }
  }

  return(test_stats)
}
