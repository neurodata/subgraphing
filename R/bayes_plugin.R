## Bayes plugin classifier and predictor
SBMBayesEstimate <- function(Alist,Y,tau){
# estimate Bhat for Bayes plugin classifier Bhat
# Phat is Bhat rescaled to n*n
n = length(tau);
K = length(unique(tau));
C = length(unique(Y));

m <- dim(Alist[[1]])[1]

Al <- length(Alist)

Bhat = list();
Phat = list();
for(i in 1:C){
  Bhat[[i]] <- matrix(0,K,K)
  Phat[[i]] <- matrix(0,m,m)
}


for (i in 1:C) {
  
  Abar<-matrix(0,m,m)
  for (s in 1:Al){
    if( Y[s] == i) {
      Abar = Abar + Alist[[s]];
    }
  }
  Abar <- Abar / sum(Y == i)
  
  
  for (j in 1:K) {
    for (k in (j):K){
    Bhat[[i]][j,k] = mean(mean(Abar[tau==j,tau==k]));
    Bhat[[i]][k,j] = Bhat[[i]][j,k];
  }
  }
  
  
for (j in 1:K) {
sAbar =  Abar[tau==j,tau==j];
ns = sum(tau == j);
Bhat[[i]][j,j] = sum(sum(sAbar)) / ns / (ns-1);
}
}

for (i in 1:C){
for (j in 1:K) {
for (k in j:K) {
Phat[[i]][tau==j,tau==k] = Bhat[[i]][j,k];
if(j != k) {
  Phat[[i]][tau==k,tau==j] = Bhat[[i]][k,j];
}
}
}
}
result <- list()
result[[1]] <- Bhat
result[[2]] <- Phat
return(result)

}



SBMBayesEstimate_subgraph <- function(Alist,Y,tau,S){
  Atmp <- list()
  for(i in 1:length(Alist)){
    Atmp[[i]] <- Alist[[i]][S,S]
  }
  return(SBMBayesEstimate(Atmp,Y,tau[S]))
}



SBMBayesClassify <- function(A,Phat){
C <- length(Phat);
loglik = rep(0,C);

for (i in 1:C){
  diag(Phat[[i]]) <- 0
  Phat[[i]] [Phat[[i]] == 0] <- 0.001
  Phat[[i]] [Phat[[i]] == 1] <- 0.999
}


for (i in 1:C){
likmat = log(Phat[[i]]) * A + log(1-Phat[[i]]) *(1 - A);
loglik[i] = sum(sum(likmat[lower.tri(likmat, diag = FALSE)]));
}
Yhat = which.max(loglik);
return(Yhat)
}