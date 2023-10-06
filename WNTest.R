wntest<-function(X,w,m,K,n.boot=1000){
  n=length(X)
  perm=function(j){
    return(j*m*w+rep(sample(c(0:(m-1)),m,replace = TRUE),each=w)*w+rep(c(1:w),m))
  }
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  pc=rep(0,K)
  pc_fod=rep(0,K)
  for (i in 1:K) {
    pc[i]=abs(cor(X[1:(n-i)],X[(i+1):n]))
    pc_fod[i]=abs(cor(Xfod[1:(n-i)],Xfod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum((pc_fod[2:K])^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(n/(w*m)-1)),perm))
    X_b=X[S]
    Xfod_b=Xfod[S]
    for (j in 1:K) {
      pc_b[i,j]=abs(cor(X[1:(n-j)],X_b[(j+1):n]))
      pc_fod_b[i,j]=abs(cor(Xfod[1:(n-j)],Xfod_b[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums((pc_fod_b[,2:K])^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}   

wntest_kendall<-function(X,w,m,K,n.boot=1000){
  n=length(X)
  perm=function(j){
    return(j*m*w+rep(sample(c(0:(m-1)),m,replace = TRUE),each=w)*w+rep(c(1:w),m))
  }
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  pc=rep(0,K)
  pc_fod=rep(0,K)
  for (i in 1:K) {
    pc[i]=abs(cor.fk(X[1:(n-i)],X[(i+1):n]))
    pc_fod[i]=abs(cor.fk(Xfod[1:(n-i)],Xfod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum((pc_fod[2:K])^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(n/(w*m)-1)),perm))
    X_b=X[S]
    Xfod_b=Xfod[S]
    for (j in 1:K) {
      pc_b[i,j]=abs(cor.fk(X[1:(n-j)],X_b[(j+1):n]))
      pc_fod_b[i,j]=abs(cor.fk(Xfod[1:(n-j)],Xfod_b[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums((pc_fod_b[,2:K])^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}

wntest_dcov<-function(X,w,m,K,n.boot=1000){
  n=length(X)
  perm=function(j){
    return(j*m*w+rep(sample(c(0:(m-1)),m,replace = TRUE),each=w)*w+rep(c(1:w),m))
  }
  Xfod=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
  pc=rep(0,K)
  pc_fod=rep(0,K)
  for (i in 1:K) {
    pc[i]=abs(dcor2d(X[1:(n-i)],X[(i+1):n]))
    pc_fod[i]=abs(dcor2d(Xfod[1:(n-i)],Xfod[(i+1):n]))
  }
  pc=sum(pc^2)
  pc_fod=sum((pc_fod[2:K])^2)
  pc_b=matrix(0,nrow = n.boot,ncol = K)
  pc_fod_b=matrix(0,nrow = n.boot,ncol = K)
  for (i in 1:n.boot) {
    S=c(sapply(c(0:(n/(w*m)-1)),perm))
    X_b=X[S]
    Xfod_b=Xfod[S]
    for (j in 1:K) {
      pc_b[i,j]=abs(dcor2d(X[1:(n-j)],X_b[(j+1):n]))
      pc_fod_b[i,j]=abs(dcor2d(Xfod[1:(n-j)],Xfod_b[(j+1):n]))
    }
  }
  pc_b=rowSums(pc_b^2)
  pc_fod_b=rowSums((pc_fod_b[,2:K])^2)
  p1=mean(pc_b>pc)
  p2=mean(pc_fod_b>pc_fod)
  return(c(p1,p2))
}
