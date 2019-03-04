installsource("MGCSampleStat.R")
source("Dcorr.R")
source("RVcorr.R")
library(energy)
library(stats)
library(SDMTools)

# library(MGC)
# n<-100
# m<-10
# Y <- c(rep(0,m/2),rep(1,m/2))
# A<-list()
# for (i in 1:m){
#   Ai<-matrix(runif(n^2,0,1),n,n)
#   A[[i]]<-(Ai+t(Ai))/2
#   A[[i]][1:(n/2),1:(n/2)] <- A[[i]][1:(n/2),1:(n/2)] + Y[i]/10
# }
# subgraph_search(A, Y, 'mgc', 'double')



subgraph_search <- function(Alist, Y, mod, cen){
  m <- length(Alist)
  n <- dim(Alist[[1]])[1]
  cors <- rep(0,n)
  for (i in 1:n){
    X <- matrix(0,m,n)
    for (j in 1:m){
      X[j,] <- Alist[[j]][i,]
    }

    if (mod == 'mcor') {
      cors[i] <- dcor.ttest(X, Y)$estimate
      #cors[i] <- dcorr(X,Y, mod, cen)
    } else  if (mod == 'cca') {
      cors[i] <- cancor(X,Y, ycenter=TRUE)$cor
    }  else  if (mod == 'rv') {
        cors[i] <- RVcorr(X,Y)
    } else if (mod == 'mgc'){
      pd1 <- as.matrix(dist(X,upper=TRUE))
      pd2 <- as.matrix(dist(Y,upper=TRUE))
      if(is.factor(Y) ){
        pd2 <- 0 + (pd2 != 0)
      }
      cors[i] <- mgc.sample(pd1,pd2)[[1]]
    }
  }
  return(cors)
}



subgraph_search_iter <- function(Alist, Y, mod, cen, decay = 0.8){
  m <- length(Alist)
  n <- dim(Alist[[1]])[1]
  cors <- rep(0,n)
  iter <- 1


  Atmp <- list()
  Sindex <- 1:n

  while( decay^iter * n  > 20  ) {
  for (i in 1:m){
    Atmp[[i]] <- Alist[[i]][Sindex,Sindex]
  }
  tmpcors <- subgraph_search(Atmp, Y, mod, cen)
  tmpq <- quantile(tmpcors, 1 - decay)
  cors[Sindex] <- tmpcors + iter
  Sindex <- Sindex[which(tmpcors > tmpq)]
  iter <- iter + 1
  }

  return(cors)
}
