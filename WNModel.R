sx<-function(n,model,C){
  eps=runif(n,1,2)
  X = c()
  X[1] = eps[1]
  X[2] = eps[2]
  for (i in 2:n){
    X[i] = C*X[i-1]+eps[i]
  }
  #X=4+rnorm(n,0,1)
  r=rep(0,n)
  d=n/5
  if(model==1){
    for (i in 0:4) {
      #r[c(1:d)+i*d]=c[i+1]
      r[c(1:d)+i*d]=runif(1,0.2,0.8)
    }
  }
  if(model==2){
    for (j in 1:n) {
      r[j]=max(min(0.3*sin(j*2*pi/n)+0.5+1/n*rnorm(1),1),0.01)
    }
  }
  I=X*r
  return(list(X=X,I=I,r=r))
}
