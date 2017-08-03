## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ---- fig.width=8, fig.height=2, warning=FALSE, message=FALSE------------
require(subgraphing)
require(ggplot2)
require(reshape2)
require(fmriutils)
require(Rmisc)

lseq <- function(from, to, n) {
  return(round(exp(seq(log(from), log(to), length.out = n))))
}

n = 4

p <- array(runif(n^2), dim=c(n, n))  # p is initially random
edges <- sample(1:n^2, n, replace = FALSE)  # select 4 random edges
p1 <- p; p2 <- p  # initialize p1 and p2 to the same array

for (edge in edges) {
  p1[edge] <- .25
  p2[edge] <- .75
}

p <- array(NaN, dim=c(n, n, 2))
p[,,1] <- p1
p[,,2] <- p2

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(n, n))
sg[edges] <- 1
plot_sg <- fmriu.plot.plot_square(sg, title="Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_sg, cols = 3)

## ---- fig.width=8, fig.height=2, warning=FALSE, message=FALSE------------
ns = 100

samp <- array(NaN, dim=c(n, n, ns*2))
samp[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns)
samp[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns)

Y <-array(NaN, dim=c(ns*2))
Y[1:ns] <- 0
Y[(ns+1):(2*ns)] <- 1

# approximate estimators and contingency table
train <- sg.bern.subgraph_train(samp, Y, 4, coherent=FALSE, tstat = "fisher")

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(train$p[,,1], title="Est P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(train$p[,,2], title="Est P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
estsg <- array(0, dim=c(n, n))
estsg[train$edges] <- 1
plot_sg <- fmriu.plot.plot_square(estsg, title="Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_sg, cols = 3)


test <- array(NaN, dim=c(n, n, ns*2))
test[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns)
test[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns)
test_y <- array(0, dim=c(2*ns))
test_y[(ns+1):2*ns] <- 1

classifier_res <- sg.bern.subgraph_classifier(test, train$edges, train$p, train$pi, train$classes)

## ---- fig.height=2, warning=FALSE, message=FALSE, fig.width=8------------
ns <- lseq(10, 300, 8)
nes <- c(3, 6, 9)

dim <- 4

p <- array(runif(dim^2), dim=c(dim, dim))  # p is initially random
edges <- sample(1:dim^2, 6, replace = FALSE)  # select 6 random edges
p1 <- p; p2 <- p # initialize p1 and p2 to the same array with some noise

for (edge in edges) {
  p1[edge] <- .3
  p2[edge] <- .7
}

p1 <- p1 + rnorm(dim^2, mean=0, sd=.1); p2 <- p2 + rnorm(dim^2, mean=0, sd=.1)

p1[p1 > 1] <- 1; p1[p1 < 0] <- 0; p2[p2 > 1] <- 1; p2[p2 < 0] <- 0

p <- array(NaN, dim=c(dim, dim, 2))
p[,,1] <- p1
p[,,2] <- p2

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(dim, dim))
sg[edges] <- 1
plot_sg <- fmriu.plot.plot_square(sg, title="Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_sg, cols = 3)

## ---- fig.height=2, warning=FALSE, message=FALSE, fig.width=8------------
results <- data.frame(n=c(), nedges=c(), error=c(), miss_edge=c())
for (sim in 1:10) {
  
  p <- array(runif(dim^2), dim=c(dim, dim))  # p is initially random
  edges <- sample(1:dim^2, 6, replace = FALSE)  # select 6 random edges
  p1 <- p; p2 <- p # initialize p1 and p2 to the same array with some noise
  
  for (edge in edges) {
    p1[edge] <- .3
    p2[edge] <- .7
  }
  p1 <- p1 + rnorm(dim^2, mean=0, sd=.1); p2 <- p2 + rnorm(dim^2, mean=0, sd=.1)
  
  p1[p1 > 1] <- 1; p1[p1 < 0] <- 0; p2[p2 > 1] <- 1; p2[p2 < 0] <- 0
  
  p <- array(NaN, dim=c(dim, dim, 2))
  p[,,1] <- p1
  p[,,2] <- p2
  for (n in ns) {
    samp <- array(NaN, dim=c(dim, dim, n*2))
    samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n)
    samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n)
    
    Y <-array(NaN, dim=c(n*2))
    Y[1:n] <- 0
    Y[(n+1):(2*n)] <- 1
    for (ne in nes) {
      class_res <- sg.bern.xval_classifier(samp=samp, Y=Y, nedge=ne, tstat="fisher", coherent = FALSE, xval="loo")
      miss_edge <- 1 - 1/length(edges)*sum(edges %in% class_res$edges)
      results <- rbind(results, data.frame(n=n, nedges=ne, error=class_res$error, miss_edge=miss_edge))    
    }
  }
}

## ---- fig.height=2, warning=FALSE, message=FALSE, fig.width=8------------
results$nedges <- factor(results$nedges)
me_plot <- ggplot(results, aes(x=n, y=miss_edge, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ggtitle("Proportion of Subgraph Edges Missed by Subgraph Estimator") +
  xlab("Number of Training Examples per Class") +
  ylab("Missed-Edge Rate")

xv_plot <- ggplot(results, aes(x=n, y=error, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ggtitle("Error of Model Estimated with Leave-One-Out Cross Validation") +
  xlab("Number of Training Examples per Class") +
  ylab("Cross-Validated Error")

multiplot(me_plot, xv_plot, cols=2)

## ---- fig.height=2, warning=FALSE, message=FALSE, fig.width=8------------
xdim <- 9
ydim <- 9
c <- 2  # number of classes
p <- array(NaN, dim=c(xdim, ydim, c))

signal_edges <- c(1, 4, 7, 9, 23, 26, 21)
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p1[upper.tri(p1, diag=FALSE)] <- 0

p2 <- p1
for (e in c(signal_edges)) {
  p1[e] <- .3
  p2[e] <- .7
}

p1 <- p1 + t(p1) - diag(diag(p1)) + rnorm(xdim^2, mean=0, sd=.05)
p2 <- p2 + t(p2) - diag(diag(p2)) + rnorm(xdim^2, mean=0, sd=.05)
p1[p1 > 1] <- 1; p1[p1 < 0] <- 0; p2[p2 > 1] <- 1; p2[p2 < 0] <- 0
p[,,1] <- p1
p[,,2] <- p2

ns = 100

samp <- array(NaN, dim=c(xdim, xdim, ns*2))
samp[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns, rewire=.25)
samp[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns, rewire=.25)

Y <-array(NaN, dim=c(ns*2))
Y[1:ns] <- 0
Y[(ns+1):(2*ns)] <- 1


plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(xdim, xdim))
sg[signal_edges] <- 1
sg <- sg + t(sg) - diag(diag(sg))
plot_sg <- fmriu.plot.plot_square(sg, title="True Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_sg, cols = 3)

## ---- fig.width=8, fig.height=2, warning=FALSE, message=FALSE------------
# approximate estimators and contingency table
train <- sg.bern.subgraph_train(samp, Y, 12, coherent=FALSE, tstat = "fisher")

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(train$p[,,1], title="Est P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(train$p[,,2], title="Est P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
estsg <- array(0, dim=c(xdim, xdim))
estsg[train$edges] <- 1
plot_sg <- fmriu.plot.plot_square(estsg, title="Estimated Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_sg, cols = 3)

## ---- fig.width=8, fig.height=2, warning=FALSE, message=FALSE------------
ns <- lseq(10, 300, 8)
nes <- c(6, 12, 18)
results <- data.frame(n=c(), nedges=c(), error=c(), miss_edge=c())
for (sim in 1:10) {
  print(sim)
  xdim <- 9
  ydim <- 9
  c <- 2  # number of classes
  p <- array(NaN, dim=c(xdim, ydim, c))
  
  signal_edges <- c(1, 4, 7, 9, 23, 26, 21)
  p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
  p1[upper.tri(p1, diag=FALSE)] <- 0
  
  p2 <- p1
  for (e in c(signal_edges)) {
    p1[e] <- .3
    p2[e] <- .7
  }
  
  p1 <- p1 + t(p1) - diag(diag(p1)) + rnorm(xdim^2, mean=0, sd=.05)
  p2 <- p2 + t(p2) - diag(diag(p2)) + rnorm(xdim^2, mean=0, sd=.05)
  p1[p1 > 1] <- 1; p1[p1 < 0] <- 0; p2[p2 > 1] <- 1; p2[p2 < 0] <- 0
  p[,,1] <- p1
  p[,,2] <- p2

  for (n in ns) {
    samp <- array(NaN, dim=c(xdim, ydim, n*2))
    samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n, rewire=.25)
    samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n, rewire=.25)
    
    Y <-array(NaN, dim=c(n*2))
    Y[1:n] <- 0
    Y[(n+1):(2*n)] <- 1
    for (ne in nes) {
      class_res <- sg.bern.xval_classifier(samp=samp, Y=Y, nedge=ne, tstat="fisher", coherent = FALSE, xval="loo")
      true_edges <- c(1, 4, 7, 9, 28, 55, 73, 19, 37, 64)
      miss_edge <- 1 - 1/length(true_edges)*sum(true_edges %in% class_res$edges)
      results <- rbind(results, data.frame(n=n, nedges=ne, error=class_res$error, miss_edge=miss_edge))    
    }
  }
}

## ---- fig.width=8, fig.height=2, warning=FALSE, message=FALSE------------
results$nedges <- factor(results$nedges)
me_plot <- ggplot(results, aes(x=n, y=miss_edge, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size=2) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Proportion of Edges Missed by Incoherent Subgraph Estimator") +
  xlab("Number of Training Examples per Class") +
  ylab("Missed-Edge Rate")

xv_plot <- ggplot(results, aes(x=n, y=error, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size=2) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Error of Model Estimated with Leave-One-Out Cross Validation") +
  xlab("Number of Training Examples per Class") +
  ylab("Cross-Validated Error")

multiplot(me_plot, xv_plot, cols=2)

