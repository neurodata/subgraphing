#' Bernoulli Subgraph Edge Estimation
#'
#' \code{sg.bern.subgraph_classifier} orders the edges by test statistic results depending on the estimator type specified.
#'
#' @param test_results [n x n] the test-statistic results.
#' @param e the number of edges to look for, arbitrarily breaking ties as necessary.
#' @param coherent=FALSE if FALSE, estimate an incoherent subgraph, otherwise an integer indicating the number of vertices in the coherent subgraph.
#' @return edges [n*n] the edges in the graph ordered by test statistic depending on the estimator type.
#' @export
#' @seealso \code{\link{sg.bern.compute_graph_statistics}}
#'
sg.bern.subgraph_edge_estimation <- function(test_results, e, coherent=FALSE) {

  dims <- dim(test_results)
  n <- dims[1]
  m <- dims[2]

  if (n != m) {
    stop(sprintf("You have passed an invalid graph, as dim1 is %d and dim2 is %d, while dim1 should be == dim2.", n, m))
  }

  if (identical(coherent, FALSE)) {
    # incoherent estimator
    edges <- sort(test_results, index.return=TRUE)$ix[1:e]  # sort test statistics in ascending order and take first e of them
  } else if (is.numeric(coherent)) {
    if (coherent < 1) {
      stop(sprintf("The number of signal vertices you have requested, %d, is less than 1.", coherent))
    }
    coherent = ceiling(coherent)  # in case it is a float we ceil it
    # get the possible edge-wise unique values of significance we can have so we can increment over them
    # start with the lowest and add edges as we go
    unique_sig <- sort(unique(c(test_results)))
    for (sig in unique_sig) {
      vless <- 1*(test_results <= sig)  # 1s where lower than significance
      # score per vertex is the number of edges per vertex less than the threshold
      vertex_sig <- apply(vless, 1, sum)
      # see if we have (coherent #) vertices whose scores sum to greater than or equal the goal subgraph size
      vsig_ids_topc <- sort(vertex_sig, decreasing=TRUE, index.return=TRUE)$ix[1:coherent]
      if (sum(vertex_sig[vsig_ids_topc]) >= e) {
        # define a subgraph on the vertices with the most edges by setting the rest of the matrix to 1
        vertex_subgraph <- array(1, dim=c(n, n))
        # significance scores are btwn 0 and 1 and smaller is what we want, so fill in our subgraph with
        # the true values which will be lower than 1 assuming any entries have a chance to reject the null
        vertex_subgraph[vsig_ids_topc,] <- test_results[vsig_ids_topc,]
        # find the top edges from the vertex subgraph as the ones with lowest e significances
        edges <- sort(vertex_subgraph, index.return=TRUE)$ix[1:e]
        break
      }
    }
  } else {
    stop("You have entered an invalid number of signal vertices.")
  }
  return(edges)
}
