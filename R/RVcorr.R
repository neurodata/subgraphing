RVcorr <- function(X,Y){
  X <- as.matrix(X)
  Y <- as.matrix(Y)
  Xmean <- colMeans(X)
  Ymean <- colMeans(Y)
  Xc <- X - t(matrix(rep(Xmean,dim(X)[1]),dim(X)[2],dim(X)[1]))
  Yc <- Y - t(matrix(rep(Ymean,dim(Y)[1]),dim(Y)[2],dim(Y)[1]))
  
  Covv <- sum(diag(t(Xc) %*% Yc %*% (t(Yc) %*%  Xc)))
  varx <- sum(diag(t(Xc) %*% Xc %*% (t(Xc) %*%  Xc)))
  vary <- sum(diag(t(Yc) %*% Yc %*% (t(Yc) %*%  Yc)))
  return(Covv/sqrt(varx*vary))
}