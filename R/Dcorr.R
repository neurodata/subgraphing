dcorr <- function( X, Y, mod,cen ){
n = dim(as.matrix(X))[1];
m = dim(as.matrix(Y))[1];
if (m != n)
  print('Inputs must have the same number of rows')
end

# compute centered pairwise distance
if (dim(as.matrix(X))[1] != dim(as.matrix(X))[2]) {
pd1 = as.matrix(dist(X,upper=TRUE));
pd2 = as.matrix(dist(Y,upper=TRUE));
if(is.factor(Y)  ){
  pd2 <- 0 + (pd2 != 0)
}
} else {
  pd1 <- X
  pd2 <- Y
}

if(cen=='double') {
    cpd1 = pd1 - t(matrix(rep(colMeans(pd1),n),ncol=n))-matrix(rep(rowMeans(pd1),n),ncol=n)+mean(pd1)
    cpd2 = pd2 - t(matrix(rep(colMeans(pd2),n),ncol=n))-matrix(rep(rowMeans(pd2),n),ncol=n)+mean(pd2)
} else {
  cpd1 = pd1 - t(matrix(rep(colMeans(pd1),n),ncol=n))
  cpd2 = pd2 - t(matrix(rep(colMeans(pd2),n),ncol=n))
}


if (mod == 'mcor') {
mcpd1 = matrix(0,n,n);
mcpd2 = matrix(0,n,n);
mcpd1 = cpd1*n/(n-1)- pd1/(n-1);
mcpd1[as.logical(diag(n))] = (mean(pd1,1) - mean(pd1))*n/(n-1);
mcpd2 = cpd2*n/(n-1)-pd2/(n-1);
mcpd2[as.logical(diag(n))] = (mean(pd2,1) - mean(pd2))*n/(n-1);
covXY = (sum(sum(mcpd1*mcpd2))- n/(n-2)* sum(mcpd1[as.logical(diag(n))]*mcpd2[as.logical(diag(n))])) / (n*(n-3));
covXX = (sum(sum(mcpd1*mcpd1))- n/(n-2)* sum(mcpd1[as.logical(diag(n))]*mcpd1[as.logical(diag(n))])) / (n*(n-3));
covYY = (sum(sum(mcpd2*mcpd2))- n/(n-2)* sum(mcpd2[as.logical(diag(n))]*mcpd2[as.logical(diag(n))])) / (n*(n-3));
cor = covXY / sqrt(covXX * covYY);
} else {
# compute distance correlation
covXY = sum(sum(cpd1*cpd2)) / n^2;
covXX = sum(sum(cpd1*cpd1)) / n^2;
covYY = sum(sum(cpd2*cpd2)) / n^2;
cor = covXY / sqrt(covXX * covYY);
}
return(cor)
}


mmgc_k <- function( X, Y, k){
  n = dim(as.matrix(X))[1];
  m = dim(as.matrix(Y))[1];
  if (m != n)
    print('Inputs must have the same number of rows')
  end
  # compute centered pairwise distance
  if (dim(as.matrix(X))[1] != dim(as.matrix(X))[2]) {
    pd1 = as.matrix(dist(X,upper=TRUE));
    pd2 = as.matrix(dist(Y,upper=TRUE));
    if(is.factor(Y)  ){
      pd2 <- 0 + (pd2 != 0)
    }
  } else {
    pd1 <- X
    pd2 <- Y
  }
  
  r1 <- t(apply(pd1,1,rank))
  r2 <- t(apply(pd2,1,rank))
  diag(r1) <- Inf
  diag(r2) <- Inf
  
#   d1 <- matrix(0,n,k)
#   d2 <- matrix(0,n,k)
#   for(i in 1:n){
#     s <- sort(pd1[i,],index = T);
#     d1[i,] = pd1[i,s$ix[1:k]];
#     #s <- sort(pd2[i,],index = T);
#     d2[i,] = pd2[i,s$ix[1:k]];
#   }
#   d1 <- d1 - matrix(rep(rowMeans(d1),k),ncol=k)
#   d2 <- d2 - matrix(rep(rowMeans(d2),k),ncol=k)
  
  i1 <- (r1 < k) & (r2 < k)
  
  cors <- rep(0,n)
  
  for(i in 1:n){
    tmp1 <- pd1[i,i1[i,]]
    tmp2 <- pd2[i,i1[i,]]
    if(length(tmp1) <= 2){
      cors[i] <- 0
    }
    else {
      cors[i] <- cor(tmp1,tmp2)
    }
  }
  
#   covXY <- rowSums(d1 * d2) 
#   varXX <- rowSums(d1 * d1)
#   varYY <- rowSums(d2 * d2)
  mmgc <- mean(cors)
  return (mmgc)
}

mmgc_stat <- function(X,Y){
  pd1 = as.matrix(dist(X,upper=TRUE));
  pd2 = as.matrix(dist(Y,upper=TRUE));
  n = dim(pd1)[1];
  
  mmgck <- rep(0,n)
  
  for(k in 1:n){
    mmgck[k] <- mmgc_k(pd1,pd2,k)
  }
  
  return(max(mmgck))
}


