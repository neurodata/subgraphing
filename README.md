# subgraphing

The `subgraphing` package is a reliable package for estimating the signal subgraphs that most capture class-conditional variation in multi-class graphs. Unlike traditional approaches that simply vectorize the graph, analyzing subgraphs allows us to reliably reduce the dimensionality of our data, removing artifacts such as noise, while preserving the inherent structure and labelling of the vertices in the original graph. 

## Installation

The following installation has been tested on Ubuntu 16.04 with R version 3.4.1. 

```
R -e "install.packages('devtools')"
R -e "devtools::install_github('neurodata/fmriutils')"  # contains utilities for plotting data in our demos
R -e "devtools::install_github('neurodata/subgraphing')"
```

For tutorials on usage, see our html documents:

[bernoulli graph estimators](http://docs.neurodata.io/subgraphing/vignettes/bern_graph_estimation.html)  
[incoherent bernoulli subgraphs](http://docs.neurodata.io/subgraphing/vignettes/bern_incoherent_subgraph_estimation.html)  
[coherent bernoulli subgraphs](http://docs.neurodata.io/subgraphing/vignettes/bern_coherent_subgraph_estimation.html)
