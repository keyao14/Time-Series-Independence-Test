FOD_dcov<-function(X,I,d,m,K,n.boot=500){
  perm=function(j){
    return(j*m*d+rep(sample(c(0:(m-1)),m,replace = TRUE),each=d)*d+rep(c(1:d),m))
  }
  n=length(X)
  s=floor(n/d)
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  Ifod=c(I[2:n]-I[1:(n-1)],I[1]-I[n])
  pc=rep(0,K+1)
  pc_fod=rep(0,K+1)
  for (i in 0:K) {
    pc[i+1]=abs(dcor2d(X[1:(n-i)],I[(i+1):n]))
    pc_fod[i+1]=abs(dcor2d(Xfod[1:(n-i)],Ifod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum(pc_fod^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K+1)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K+1)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(ceiling(n/(m*d))-1)),perm))
    X_b=c(X,X[1:(m*d)])[S[1:n]]
    Xfod_b=c(Xfod,Xfod[1:(m*d)])[S[1:n]]
    for (j in 0:K) {
      pc_b[i,j+1]=abs(dcor2d(X_b[1:(n-j)],I[(j+1):n]))
      pc_fod_b[i,j+1]=abs(dcor2d(Xfod_b[1:(n-j)],Ifod[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums(pc_fod_b^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}
FOD<-function(X,I,d,m,K,n.boot=500){
  perm=function(j){
    return(j*m*d+rep(sample(c(0:(m-1)),m,replace = TRUE),each=d)*d+rep(c(1:d),m))
  }
  n=length(X)
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  Ifod=c(I[2:n]-I[1:(n-1)],I[1]-I[n])
  pc=rep(0,K+1)
  pc_fod=rep(0,K+1)
  for (i in 0:K) {
    pc[i+1]=abs(cor(X[1:(n-i)],I[(i+1):n]))
    pc_fod[i+1]=abs(cor(Xfod[1:(n-i)],Ifod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum(pc_fod^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K+1)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K+1)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(ceiling(n/(m*d))-1)),perm))
    X_b=c(X,X[1:(m*d)])[S[1:n]]
    Xfod_b=c(Xfod,Xfod[1:(m*d)])[S[1:n]]
    for (j in 0:K) {
      pc_b[i,j+1]=abs(cor(X_b[1:(n-j)],I[(j+1):n]))
      pc_fod_b[i,j+1]=abs(cor(Xfod_b[1:(n-j)],Ifod[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums(pc_fod_b^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}
FOD_kendall<-function(X,I,d,m,K,n.boot=500){
  perm=function(j){
    return(j*m*d+rep(sample(c(0:(m-1)),m,replace = TRUE),each=d)*d+rep(c(1:d),m))
  }
  n=length(X)
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  Ifod=c(I[2:n]-I[1:(n-1)],I[1]-I[n])
  pc=rep(0,K+1)
  pc_fod=rep(0,K+1)
  for (i in 0:K) {
    pc[i+1]=abs(cor.fk(X[1:(n-i)],I[(i+1):n]))
    pc_fod[i+1]=abs(cor.fk(Xfod[1:(n-i)],Ifod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum(pc_fod^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K+1)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K+1)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(ceiling(n/(m*d))-1)),perm))
    X_b=c(X,X[1:(m*d)])[S[1:n]]
    Xfod_b=c(Xfod,Xfod[1:(m*d)])[S[1:n]]
    for (j in 0:K) {
      pc_b[i,j+1]=abs(cor.fk(X_b[1:(n-j)],I[(j+1):n]))
      pc_fod_b[i,j+1]=abs(cor.fk(Xfod_b[1:(n-j)],Ifod[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums(pc_fod_b^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}


