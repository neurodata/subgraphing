## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, cache=TRUE)

## ----eval=FALSE----------------------------------------------------------
#  # A function to estimate a beta distribution for each edge in a graph.
#  # Inputs
#  #   sample: a [n x m x s] element array, where we have s observations of nxm graphs.
#  # Outputs
#  #   alpha: a [n x m] matrix denoting the alpha parameter per edge.
#  #   beta: a [n x m] matrix denoting the beta parameter per edge.
#  sg.bern.graph_estimator(sample, smooth=TRUE):
#    p = sum(sample, 3)/s  # sum over the p dimension
#    if (smooth) {
#      np = 1/(10*s)
#      p[p == 0] = np
#      p[p == 1] = 1 - np
#    }
#    return p
#  
#  # a function to estimate a beta distribution for a sample.
#  # Inputs
#  #   sample: a [s] element vector that is between 0 and 1.
#  # Outputs
#  #   alpha: the alpha parameter of the method of moments estimate given the sample.
#  #   beta: the beta parameter of the method of moments estimate given the sample.
#  sg.bern.estimator(sample, smooth=TRUE):
#    p = sum(sample)/s
#    if (smooth) {
#      np = 1/(10*s)
#      p = ifelse(p == 0, np, p)
#      p = ifelse(p == 1, 1 - np, p)
#    }
#    return p
#  
#  # a function to generate random beta-distributed samples.
#  # Inputs
#  #   p: a [n x m] matrix of the p parameters per edge.
#  #   s: the number of samples.
#  # Outputs
#  #   samp: a [n x m x p] array sampling from the [n x m] graph RV p times.
#  sg.beta.sample_graph(p, s):
#    # [n x m x s] array of random uniforms btwn 0 and 1
#    rvals = uniform(n*m*s, from=0, to=1, dim=c(n, m, s))
#    # for each i=1:s, set the value of an edge to 1 if our random value is less than the p,
#    # and 0 otherwise
#    for i=1:s {
#      samp[,,i] = (rvals[,,i] < p)
#    }
#    return samp

## ----fig.width=8---------------------------------------------------------
require(subgraphing)
require(ggplot2)
require(reshape2)
library(MASS)
library(scales)
require(Rmisc)

lseq <- function(from, to, n) {
  return(round(exp(seq(log(from), log(to), length.out = n))))
}

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
                                 "yellow", "#FF7F00", "red", "#7F0000"))
plot_param <- function(mtx, title='Parameter', lims=c(0, 1)) {
  meltobj <- melt(mtx)
  colnames(meltobj) <- c('X', 'Y', 'value')
  plt <- ggplot(meltobj, aes(x=X, y=Y, fill=value)) +
    geom_tile() +
    scale_fill_gradientn(colors = jet.colors(7), limits=lims, oob=squish) +
    xlab('X dimension') +
    ylab('Y dimension') +
    ggtitle(title)
  return(plt)
}
xdim <- 3
ydim <- 3
p <- array(runif(xdim*ydim, min=0, max=1), dim=c(xdim, ydim))  # true p
n_vec <- c(50, 1000)
n_spaces <- length(n_vec)
n_bin <- 15
edge_err <- array(NaN, dim=c(xdim, ydim, n_spaces))  # the error per edge per iteration
err <- array(NaN, dim=c(n_spaces))  # the average error per iteration
hist_breaks <- seq(0, 1, length.out=n_bin)  # beta distr btwn 0 and 1

for (k in 1:length(n_vec)) {
  edge_distr_plots <- list()
  n <- n_vec[k]
  samp <- sg.bern.sample_graph(p, n)
  counter <- 1
  est <- sg.bern.graph_estimator(samp)
  params <- plot_param(est$p, title=paste('Estimated p, n=', n, sep=""))
  print(params)
}
params <- plot_param(p, title='True p')
print(params)

## ----fig.width=8---------------------------------------------------------
n_vec <- lseq(50, 5000, 20)
n_spaces <- length(n_vec)
diff <- list(p=array(NaN, dim=c(n_spaces)))

for (k in 1:length(n_vec)) {
  n <- n_vec[k]
  samp <- sg.bern.sample_graph(p, n)

  est <- sg.bern.graph_estimator(samp)
  diff$p[k] <- norm(est$p - p, 'F')
}

base_breaks <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }
}

param_results <- data.frame(n=n_vec, p=diff$p)
param_results <- as.data.frame.array(melt(param_results, id="n", variable.name="parameter"))
param_err_plot <- ggplot(param_results, aes(x=n, y=value, group=parameter, color=parameter)) +
  geom_line(size=3) +
  scale_x_log10(labels=trans_format("log10", math_format(10^.x)), breaks=trans_breaks("log10", function(x) 10^x, n=4)) +
  xlab('Number of Samples') +
  ylab('Norm of Difference') +
  ggtitle('Analalyzing Consistency of Parameters to Truths') +
  theme(text=element_text(size=12)) +
  annotation_logticks(base = 10)

multiplot(param_err_plot, cols = 1)

