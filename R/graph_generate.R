sbm_generate<-function(B,n,pi){
  P<-matrix(0,n,n)
  k<-length(pi)
  nc<-n*pi
  ncsum<-c(0,cumsum(nc))
  for(i in 1:k){
    for(j in 1:k){
      P[(ncsum[i]+1):ncsum[i+1],(ncsum[j]+1):ncsum[j+1]]<-B[i,j]
    }
  }
  A<-matrix(runif(n^2),n,n)<P
  A[upper.tri(A,T)]<-0
  A<-A+t(A)
  return(A)
}

ase<-function(A,d){
  s<-svd(A,d,0)
  X<-s$u%*%diag(sqrt(abs(s$d[1:d])))
  return(X)
}

leig<-function(A,d){
  Dsum<-colSums(A)
  D<-diag(Dsum)
  Dinv<-diag(1/Dsum)
  Dinv[is.infinite(Dinv)]<-0
  L<-sqrt(Dinv)%*%(D-A)%*%sqrt(Dinv)
  e<-eigen(L)
  n<-dim(A)[1]
  X<-sqrt(D)%*%e$vectors[,(n-d):(n-1)]
  return(X)
}