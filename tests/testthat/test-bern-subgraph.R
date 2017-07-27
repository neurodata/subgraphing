library(subgraphing)
context("Bernoulli Subgraph Estimator and Classifier Tests")

n = 1000  # number of samples per class just so we get a lot of data

xdim <- 4
ydim <- 5
c <- 2  # number of classes
p <- array(runif(xdim*ydim), dim=c(xdim, ydim, c))

# edges are 1,1 (1) and 2,1 (5) and 3,1 (9) and 4, 1 (13)
edges <- c(1, 5, 9, 13, 17)
for (i in 1:length(edges)) {
  p[1, i, 1] <- 0
  p[1, i, 2] <- 1
}

samp <- array(NaN, dim=c(xdim, ydim, n*2))
samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

Y <-array(NaN, dim=c(n*2))
Y[1:n] <- 0
Y[(n+1):(2*n)] <- 1

# approximate estimators and contingency table
results <- sg.bern.subgraph_estimator(samp, Y)

test_that("bernoulli subgraph estimator runs properly for easy-to-fit data", {
  expect_equal(norm(results$p[,,1] - p[,,1], 'F'), 0, tolerance=0.1)
  expect_gt(norm(results$p[,,1] - p[,,2], 'F'), norm(results$p[,,1] - p[,,1], 'F'))
  expect_equal(norm(results$p[,,2] - p[,,2], 'F'), 0, tolerance=0.1)
  expect_gt(norm(results$p[,,2] - p[,,1], 'F'), norm(results$p[,,2] - p[,,2], 'F'))
  expect_equal(results$pi[1], results$pi[2], .5, tolerance=.001)
})

test <- array(NaN, dim=c(xdim, ydim, n*2))
test[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
test[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

# approximate test-statistic results per edge
tstats <- sg.bern.edge_test(results$cont_matrix, test_stat="fisher")

classifier_res <- sg.bern.subgraph_classifier(test, tstats, p = results$p, pi=results$pi,
                                              e = 5, coherent = FALSE)
test_that("bernoulli subgraph classifier runs properly for simple test data", {
  # tests constructed directly from the model
  expect_equal(classifier_res$Yhat[1:n], rep(classifier_res$Yhat[1], n))
  expect_equal(classifier_res$Yhat[(n+1):(2*n)], rep(classifier_res$Yhat[n+1], n))
})
