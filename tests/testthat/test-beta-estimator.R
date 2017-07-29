library(subgraphing)
context("Beta Graph Estimator Tests")

test_that("beta estimation functions properly", {
  n = 100
  ns <- 5
  a <- runif(ns, min=0.5, max=5)
  b <- runif(ns, min=0.5, max=5)
  params <- rbind(a, b)
  for (i in 1:dim(params)[2]) {
    samp <- rbeta(n, params[1, i], params[2, i])
    predicted <- sg.beta.estimator(samp)
    expect_equal(predicted$alpha, as.numeric(params[1, i]), tolerance=.5)
    expect_equal(predicted$beta, as.numeric(params[2, i]), tolerance=.5)
  }
})

test_that("beta estimator throws error if sample has values greater than 1.", {
  samp <- seq(0, 1.1, .1)
  samp <- samp[sample(length(samp))]
  expect_that(sg.beta.estimator(samp), throws_error())
})

test_that("beta estimator throws error if sample has values less than 0.", {
  samp <- seq(-.1, 1, .1)
  samp <- samp[sample(length(samp))]
  expect_that(sg.beta.estimator(samp), throws_error)
})
n = 100
xdim <- 3
ydim <- 3
alpha <- array(runif(xdim*ydim, min=1, max=5), dim=c(xdim, ydim))
beta <- array(runif(xdim*ydim, min=1, max=5), dim=c(xdim, ydim))
test_that("beta graph estimation and random sampling functions properly", {

  samp <- sg.beta.sample_graph(alpha, beta, s=n)
  est <- sg.beta.graph_estimator(samp)
  expect_equal(est$alpha, alpha, tolerance=.5)
  expect_equal(est$beta, beta, tolerance=.5)
})

test_that("random sampling function throws error when alpha and beta not of same shape", {
  beta <- array(runif(16, min=1, max=5), dim=c(4, 4))
  expect_error(sg.beta.sample_graph(alpha, beta, s=n))
})

