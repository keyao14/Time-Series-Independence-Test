sx<-function(n,model,C){
  eps=runif(n,1,2)
  X = c()
  X[1] = eps[1]
  X[2] = eps[2]
  for (i in 3:n){
    X[i] = C^2*X[i-1]+C*X[i-2]+eps[i]
  }
  r=c()
  s=c()
  d=n/5
  for (i in 0:4) {
    s[c(1:d)+i*d]=runif(1,0.2,0.8)
  }
  for (j in 1:n) {
    r[j]=max(min(0.3*sin(j*2*pi/n)+0.5+0.1*runif(1,-0.25,0.25),1),0.01)
  }
  if(model==1){
    X=X
  }
  if(model==2){
    X=s*X
  }
  if(model==3){
    X=r*X
  }
  return(list(X=X,r=r))
}

