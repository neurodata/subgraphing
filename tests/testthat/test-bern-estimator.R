library(subgraphing)
context("Bernoulli Graph Estimator Tests")

n = 1000  # number of samples

test_that("bernoulli estimation functions properly for basic inputs", {
  ps <- runif(1)
  for (p in ps) {
    samp <- ifelse(runif(n) > p, 0, 1)
    predicted <- sg.bern.estimator(samp)
    expect_equal(abs(predicted$p - p), 0, tolerance=0.05)
  }
})

test_that("bernoulli estimation properly smooths", {
  samp <- array(0, dim=c(n))  # sample only has zeros so it should be smoothed
  params <- sg.bern.estimator(samp)
  np <- 1/(10*n)
  expect_equal(params$p, np)

  samp <- array(1, dim=c(n))  # sample only has 1s so it should be smoothed
  params <- sg.bern.estimator(samp)
  expect_equal(params$p, 1-np)
})

test_that("bernoulli estimation functions given inputs that need to be thresholded", {
  # inputs are uniform RVs with a threshold at .5 should have p of .5
  samp <- runif(n)  # n uniform RVs from 0 to 1
  params <- sg.bern.estimator(samp, thresh = .5, smooth = FALSE)
  expect_equal(params$p, .5, tolerance=0.05)

  # inputs are binary RVs, but 1 and -1 instead of 0 and 1
  samp <- runif(n)
  p <- 0.5
  samp <- ifelse(samp > p, 1, -1)
  params <- sg.bern.estimator(samp, thresh=0, smooth=FALSE)
  expect_equal(params$p, .5, tolerance=0.05)

  # inputs are uniform RVs with odd limits should have p of .5 when thresh=(max + min)/2
  minval = 2
  maxval = 12
  thresh = mean(c(minval, maxval))
  samp <- runif(n, min=minval, max=maxval)  # n uniform RVs from minval to maxval
  params <- sg.bern.estimator(samp, thresh = thresh, smooth = FALSE)
  expect_equal(params$p, .5, tolerance=0.05)
})

test_that("bernoulli graph estimation and random sampling functions properly", {
  xdim <- 2
  ydim <- 3
  p <- array(runif(6, min=0, max=1), dim=c(xdim, ydim))
  samp <- sg.bern.sample_graph(p, s=n)
  est <- sg.bern.graph_estimator(samp)
  expect_equal(est$p, p, tolerance=.05)
})

