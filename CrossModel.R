gts<-function(n,d,model,C){
  X=c()
  X[1]=runif(1,1,2)
  for (t in 2:n) {
    X[t]=0.5*X[t-1]+runif(1,1,2)
  }
  X=X+sin(c(1:n)/d*2*pi)
  Y=c()
  eps=rnorm(n,0,1)
  Y[1]=0.75*eps[1]+1
  if(Y[1]>0){
    Y[2]=0.35*Y[1]+0.75*eps[2]+0.5*eps[1]+2
  } else {
    Y[2]=-0.45*Y[1]+0.75*eps[2]+0.5*eps[1]+2
  }
  if(Y[2]>0){
    Y[3]=0.35*Y[2]+0.15*Y[1]+0.75*eps[3]+0.5*eps[2]+2
  } else {
    Y[3]=-0.45*Y[2]+0.15*Y[1]+0.75*eps[3]+0.5*eps[2]+2
  }
  for (k in 4:n) {
    if(Y[k-1]>0){
      Y[k]=0.35*Y[k-1]+0.15*Y[k-2]+0.75*eps[k]+0.5*eps[k-1]+C*X[k-3]+2
    } else {
      Y[k]=-0.45*Y[k-1]+0.15*Y[k-2]+0.75*eps[k]+0.5*eps[k-1]+C*X[k-3]+2
    }
  }
  r=c()
  for (j in 1:n) {
    r[j]=max(min(0.3*sin(j*2*pi/n)+0.5+0.1*runif(1, -0.25, 0.25),1),0.01)
  }
  s=c()
  for (j in 0:4) {
    s[c((1+j*n/5):((j+1)*n/5))]=runif(1,0.2,0.8)
  }
  if(model==1){
    I=r*X
    J=s*Y
  }
  if(model==2){
    I=s*X
    J=s*Y
  }
  if(model==3){
    I=r*X
    J=r*Y
  }
  if(model==4){
    I=s*X
    J=(1-s^2)*Y
  }
  if(model==5){
    I=r*X
    J=(1-r^2)*Y
  }
  if(model==6){
    I=s*X
    J=((s-0.8)^2+(s-0.2)^2)*Y
  }
  if(model==7){
    I=r*X
    J=((r-0.8)^2+(r-0.2)^2)*Y
  }
  if(model==8){
    I=X
    J=Y
  }
  return(list(X=X,Y=Y,I=I,J=J,r=r,s=s))
}
