## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ---- fig.width=6, fig.height=4, warning=FALSE, message=FALSE------------
require(subgraphing)
require(ggplot2)
require(reshape2)
require(fmriutils)
require(Rmisc)

lseq <- function(from, to, n) {
  return(round(exp(seq(log(from), log(to), length.out = n))))
}
xdim <- 9
ydim <- 9
c <- 2  # number of classes
p <- array(NaN, dim=c(xdim, ydim, c))

ns = 100

# define the signal edges for our signal vertices
signal_edges_1 <- c(2,3,4,5,7,9)  # signal edges for vertex 3
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p1[upper.tri(p1, diag=FALSE)] <- 0
p2 <- p1  # we will change our signal edges in vertices 1 and 3 to have a class-conditional difference
# add signal to vertex 1 of random magnitude
p2[1,signal_edges_1] <- p2[1, signal_edges_1] + rnorm(length(signal_edges_1), mean=0, sd=.2)

p1 <- p1 + t(p1) - diag(diag(p1))
p2 <- p2 + t(p2) - diag(diag(p2))
# fix the limits to be valid probabilities and smooth
p1[p1 > 1] <- 1 - 1/(10*ns); p1[p1 < 0] <- 1/(10*ns); p2[p2 > 1] <- 1 - 1/(10*ns); p2[p2 < 0] <- 1/(10*ns)
p[,,1] <- p1
p[,,2] <- p2


samp <- array(NaN, dim=c(xdim, xdim, ns*2))
samp[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns)
samp[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns)

Y <-array(NaN, dim=c(ns*2))
Y[1:ns] <- 0
Y[(ns+1):(2*ns)] <- 1


plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(xdim, xdim))

plot_diff <- fmriu.plot.plot_square(abs(p[,,1] - p[,,2]), title="P, class 1 - P, class 2", xlabel="vertex", ylabel="vertex", legend="p1 - p2")
sg[1, signal_edges_1] <- 1
sg <- sg + t(sg) - diag(diag(sg))
plot_sg <- fmriu.plot.plot_square(sg, title="True Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_diff, plot_sg, cols = 2)

## ---- fig.width=6, fig.height=4, warning=FALSE, message=FALSE------------
# approximate estimators and contingency table
train <- sg.bern.subgraph_train(samp, Y, 12, coherent=1, tstat = "fisher")

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(train$p[,,1], title="Est P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(train$p[,,2], title="Est P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
plot_diff <- fmriu.plot.plot_square(abs(train$p[,,1] - train$p[,,2]), title="P, class 1 - P, class 2", xlabel="vertex", ylabel="vertex", legend="p1 - p2")
estsg <- array(0, dim=c(xdim, xdim))
estsg[train$edges] <- 1
plot_sg <- fmriu.plot.plot_square(estsg, title="Estimated Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_diff, plot_sg, cols = 2)

## ---- fig.width=6, fig.height=4, warning=FALSE, message=FALSE------------
require(subgraphing)
require(ggplot2)
require(reshape2)
require(fmriutils)
require(Rmisc)

lseq <- function(from, to, n) {
  return(round(exp(seq(log(from), log(to), length.out = n))))
}

# define the signal edges for our signal vertices
signal_edges_1 <- c(2,3, 4,5, 6, 7,9)  # signal edges for vertex 3
signal_edges_3 <- c(4,5, 6, 7, 8, 9)  # signal edges for vertex 1
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p1[upper.tri(p1, diag=FALSE)] <- 0
p2 <- p1  # we will change our signal edges in vertices 1 and 3 to have a class-conditional difference
# add signal to vertex 1 of random magnitude
p2[1,signal_edges_1] <- p2[1, signal_edges_1] + rnorm(length(signal_edges_1), mean=0, sd=.15)
# add signal to vertex 3 of random magnitude
p2[3, signal_edges_3] <- p2[3, signal_edges_3] + rnorm(length(signal_edges_3), mean=0, sd=.15)
p1 <- p1 + t(p1) - diag(diag(p1))
p2 <- p2 + t(p2) - diag(diag(p2))
# fix the limits to be valid probabilities and smooth
p1[p1 > 1] <- 1 - 1/(10*ns); p1[p1 < 0] <- 1/(10*ns); p2[p2 > 1] <- 1 - 1/(10*ns); p2[p2 < 0] <- 1/(10*ns)
p[,,1] <- p1
p[,,2] <- p2

ns = 100

samp <- array(NaN, dim=c(xdim, xdim, ns*2))
samp[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns, rewire=.1)
samp[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns, rewire=.1)

Y <-array(NaN, dim=c(ns*2))
Y[1:ns] <- 0
Y[(ns+1):(2*ns)] <- 1


plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(xdim, xdim))

plot_diff <- fmriu.plot.plot_square(abs(p[,,1] - p[,,2]), title="P, class 1 - P, class 2", xlabel="vertex", ylabel="vertex", legend="p1 - p2")
sg[1, signal_edges_1] <- 1
sg[3, signal_edges_3] <- 1
sg <- sg + t(sg) - diag(diag(sg))
plot_sg <- fmriu.plot.plot_square(sg, title="True Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_diff, plot_sg, cols = 2)

## ---- fig.width=6, fig.height=4, warning=FALSE, message=FALSE------------
ns <- lseq(10, 500, 8)
nes <- c(20, 26, 30)
results <- data.frame(n=c(), nedges=c(), error=c(), miss_edge=c())
for (sim in 1:10) {
  print(sim)
  # define the signal edges for our signal vertices
  signal_edges_1 <- c(2, 3, 4, 5, 7, 9)  # signal edges for vertex 3
  signal_edges_3 <- c(4,5, 6, 8, 9)  # signal edges for vertex 1
  p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
  p1[upper.tri(p1, diag=FALSE)] <- 0
  p2 <- p1  # we will change our signal edges in vertices 1 and 3 to have a class-conditional difference
  # add signal to vertex 1 of random magnitude
  p2[1,signal_edges_1] <- p2[1, signal_edges_1] + rnorm(length(signal_edges_1), mean=0, sd=.15)
  # add signal to vertex 3 of random magnitude
  p2[3, signal_edges_3] <- p2[3, signal_edges_3] + rnorm(length(signal_edges_3), mean=0, sd=.15)
  p1 <- p1 + t(p1) - diag(diag(p1))
  p2 <- p2 + t(p2) - diag(diag(p2))
  # fix the limits to be valid probabilities and smooth
  p1[p1 > 1] <- 1 - 1/(10*ns); p1[p1 < 0] <- 1/(10*ns); p2[p2 > 1] <- 1 - 1/(10*ns); p2[p2 < 0] <- 1/(10*ns)
  p[,,1] <- p1
  p[,,2] <- p2
  
  for (n in ns) {
    samp <- array(NaN, dim=c(xdim, ydim, n*2))
    samp[,,1:n] <- sg.bern.sample_graph(p[,,1], s=n, rewire=.1)
    samp[,,(n+1):(2*n)] <- sg.bern.sample_graph(p[,,2], s=n, rewire=.1)
    
    Y <-array(NaN, dim=c(n*2))
    Y[1:n] <- 0
    Y[(n+1):(2*n)] <- 1
    for (ne in nes) {
      class_res <- sg.bern.xval_classifier(samp=samp, Y=Y, nedge=ne, tstat="fisher", coherent = 2, xval="loo")
      true_edges <- c(2, 3, 4, 5, 6, 7, 9, 10, 19, 28, 37, 46, 55, 73, 21, 22, 23, 24, 25, 26, 27, 30, 39, 48, 57, 66, 75)
      miss_edge <- 1 - 1/length(true_edges)*sum(true_edges %in% class_res$edges)
      results <- rbind(results, data.frame(n=n, nedges=ne, error=class_res$error, miss_edge=miss_edge))    
    }
  }
}

## ---- fig.width=7, fig.height=6, warning=FALSE, message=FALSE------------
results$nedges <- factor(results$nedges)
me_plot <- ggplot(results, aes(x=n, y=miss_edge, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size=2) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Proportion of Edges Missed by Incoherent Subgraph Estimator") +
  xlab("Number of Training Examples per Class") +
  ylab("Missed-Edge Rate") +
  theme(text=element_text(size=12))

xv_plot <- ggplot(results, aes(x=n, y=error, color=nedges, group=nedges)) +
  geom_point() +
  stat_summary(fun.y = mean, geom = "line", size=2) +
  stat_summary(fun.data = mean_se, geom = "errorbar", size=2) +
  scale_y_continuous(limits = c(0, 1)) +
  ggtitle("Error of Model Estimated with Leave-One-Out Cross Validation") +
  xlab("Number of Training Examples per Class") +
  ylab("Cross-Validated Error") +
  theme(text=element_text(size=12))

multiplot(me_plot, xv_plot, cols=1)

## ---- fig.width=6, fig.height=4, warning=FALSE, message=FALSE------------

# define the signal edges for our signal vertices
signal_edges_1 <- c(2,3,4,5, 6, 7,9)  # signal edges for vertex 3
signal_edges_3 <- c(3, 4,5, 6, 7, 8, 9)  # signal edges for vertex 1
p1 <- array(runif(xdim*ydim), dim=c(xdim, ydim))
p1[upper.tri(p1, diag=FALSE)] <- 0
p2 <- p1  # we will change our signal edges in vertices 1 and 3 to have a class-conditional difference
# add signal to vertex 1 of random magnitude
p2[1,signal_edges_1] <- p2[1, signal_edges_1] + rnorm(length(signal_edges_1), mean=0, sd=.15)
# add signal to vertex 3 of random magnitude
p2[3, signal_edges_3] <- p2[3, signal_edges_3] + rnorm(length(signal_edges_3), mean=0, sd=.15)
p1 <- p1 + t(p1) - diag(diag(p1))
p2 <- p2 + t(p2) - diag(diag(p2))
# fix the limits to be valid probabilities and smooth
p1[p1 > 1] <- 1 - 1/(10*ns); p1[p1 < 0] <- 1/(10*ns); p2[p2 > 1] <- 1 - 1/(10*ns); p2[p2 < 0] <- 1/(10*ns)
p[,,1] <- p1
p[,,2] <- p2

ns = 100

samp <- array(NaN, dim=c(xdim, xdim, ns*2))
samp[,,1:ns] <- sg.bern.sample_graph(p[,,1], s=ns, rewire=.1)
samp[,,(ns+1):(2*ns)] <- sg.bern.sample_graph(p[,,2], s=ns, rewire=.1)

Y <-array(NaN, dim=c(ns*2))
Y[1:ns] <- 0
Y[(ns+1):(2*ns)] <- 1


plot_p1 <- fmriu.plot.plot_square(p[,,1], title="True P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(p[,,2], title="True P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
sg <- array(0, dim=c(xdim, xdim))

plot_diff <- fmriu.plot.plot_square(abs(p[,,1] - p[,,2]), title="P, class 1 - P, class 2", xlabel="vertex", ylabel="vertex", legend="p1 - p2")
sg[1, signal_edges_1] <- 1
sg[3, signal_edges_3] <- 1
sg <- sg + t(sg) - diag(diag(sg))
plot_sg <- fmriu.plot.plot_square(sg, title="True Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_diff, plot_sg, cols = 2)

## ---- fig.width=6, fig.height=6, warning=FALSE, message=FALSE------------
# approximate estimators and contingency table, given the prior that there are 23 signal vertices
train <- sg.bern.subgraph_train(samp, Y, 27, coherent=9, tstat = "fisher")

# visualize the two probability matrices
plot_p1 <- fmriu.plot.plot_square(train$p[,,1], title="Est P, class 1", xlabel="vertex", ylabel="vertex", legend="p")
plot_p2 <- fmriu.plot.plot_square(train$p[,,2], title="Est P, class 2", xlabel="vertex", ylabel="vertex", legend="p")
plot_diff <- fmriu.plot.plot_square(abs(train$p[,,1] - train$p[,,2]), title="P, class 1 - P, class 2", xlabel="vertex", ylabel="vertex", legend="p1 - p2")
estsg <- array(0, dim=c(xdim, xdim))
estsg[train$edges] <- 1
plot_sg <- fmriu.plot.plot_square(estsg, title="Estimated Coh Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
# approximate estimators and contingency table, given the prior that there are 27 signal vertices
train <- sg.bern.subgraph_train(samp, Y, 27, coherent=FALSE, tstat = "fisher")
estsg <- array(0, dim=c(xdim, xdim))
estsg[train$edges] <- 1
plot_sg_inc <- fmriu.plot.plot_square(estsg, title="Estimated Inc Subgraph", xlabel="vertex", ylabel="vertex", legend="edge")
multiplot(plot_p1, plot_p2, plot_diff, plot_sg, plot_sg_inc, cols = 2)

