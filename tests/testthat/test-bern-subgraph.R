library(subgraphing)
context("Bernoulli Subgraph Estimator and Classifier Tests")

#------- Incoherent Estimator Tests ---------------------------------------------------------#

n = 1000  # number of samples per class just so we get a lot of data

xdim <- 5
ydim <- 5
c <- 2  # number of classes
p <- array(NaN, dim=c(xdim, ydim, c))

# should end up with 1, 11, 16, 3, 8, 18 since vertices 1 and 3 will be our signal vertices
# edges 19 and 20 are noise to make sure that our method works appropriately
edges <- c(1, 6, 11, 16, 21)
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p2 <- p1
for (i in 1:length(edges)) {
  e <- edges[i]
  if (i %% 2 == 0) {
    p1[e] <- 0
    p2[e] <- 1
  } else {
    p1[e] <- 1
    p2[e] <- 0
  }
}
p[,,1] <- p1
p[,,2] <- p2

samp <- array(NaN, dim=c(xdim, ydim, n*2))
samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

Y <-array(NaN, dim=c(n*2))
Y[1:n] <- 0
Y[(n+1):(2*n)] <- 1

# approximate estimators and contingency table
results <- sg.bern.subgraph_train(samp, Y, 5, coherent=FALSE, tstat = "fisher")

test_that("bernoulli subgraph estimator approximates valid probabilities", {
  expect_equal(norm(results$p[,,1] - p[,,1], 'F'), 0, tolerance=0.1)
  expect_gt(norm(results$p[,,1] - p[,,2], 'F'), norm(results$p[,,1] - p[,,1], 'F'))
  expect_equal(norm(results$p[,,2] - p[,,2], 'F'), 0, tolerance=0.1)
  expect_gt(norm(results$p[,,2] - p[,,1], 'F'), norm(results$p[,,2] - p[,,2], 'F'))
})

test_that("bernoulli subgraph estimator approximates valid pi values", {
  expect_equal(results$pi[1], results$pi[2], .5, tolerance=.001)
})

test_that("bernoulli incoherent subgraph estimator approximates the correct set of edges", {
  expect_true(all(c(1, 6, 11, 16, 21) %in% results$edges[1:5]))
})

test <- array(NaN, dim=c(xdim, ydim, n*2))
test[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
test[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

classifier_res <- sg.bern.subgraph_classifier(test, results$edges, results$p, results$pi, results$classes)

test_that("bernoulli incoherent subgraph classifier runs properly for simple test data", {
  # tests constructed directly from the model
  # verify that each "cluster" has the same label
  expect_equal(classifier_res$Yhat[1:n], rep(classifier_res$Yhat[1], n))
  expect_equal(classifier_res$Yhat[(n+1):(2*n)], rep(classifier_res$Yhat[n+1], n))
  # verify that the labels are different for our two classes of testing data
  expect_false(all(classifier_res$Yhat[1:n] == classifier_res$Yhat[(n+1):(2*n)]))
})

#------- Coherent Estimator Tests ---------------------------------------------------------#

n = 1000  # number of samples per class just so we get a lot of data

xdim <- 5
ydim <- 5
c <- 2  # number of classes
p <- array(NaN, dim=c(xdim, ydim, c))

# should end up with 1, 3, 6, 8 since vertices 1 and 3 will be our signal vertices
# edges 19 and 20 are noise to make sure that our method works appropriately and only
# ends up with edges from signal vertices
signal_edges <- c(1, 13, 3)
noise_edges <- c(20)
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p1[upper.tri(p1, diag=FALSE)] <- 0
p2 <- p1
for (e in signal_edges) {
  p1[e] <- .2
  p2[e] <- .8
}

for (e in noise_edges) {
  p1[e] <- 0
  p2[e] <- 1
}

p1 <- p1 + t(p1) - diag(diag(p1)); p2 <- p2 + t(p2) - diag(diag(p2))
p[,,1] <- p1
p[,,2] <- p2

samp <- array(NaN, dim=c(xdim, ydim, n*2))
samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

Y <-array(NaN, dim=c(n*2))
Y[1:n] <- 0
Y[(n+1):(2*n)] <- 1

# approximate estimators and contingency table
results <- sg.bern.subgraph_train(samp, Y, 4, coherent=2, tstat = "fisher")

test_that("bernoulli coherent subgraph estimator approximates the correct set of edges", {
  # the signal edges we placed manually should be in the resulting edges, since the noise edge
  # we added was not at a signal vertex
  expect_true(all(c(1, 3, 13, 11) %in% results$edges[1:4]))
})

test <- array(NaN, dim=c(xdim, ydim, n*2))
test[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
test[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)

classifier_res <- sg.bern.subgraph_classifier(test, results$edges, results$p, results$pi,
                                              results$classes)


