gts<-function(n,d,model,C){
  X=c()
  X[1]=runif(1,1,2)
  for (t in 2:n) {
    X[t]=0.5*X[t-1]+runif(1,1,2)
  }
  X=X+sin(c(1:n)/d*2*pi)
  Y=c()
  if(model==1){
    Y[1]=0.15*rnorm(1)+C*X[1]+2
    Y[2]=0.35*Y[1]+0.15*rnorm(1)+C*X[2]+2
    Y[3]=0.35*Y[2]+0.2*Y[1]+0.15*rnorm(1)+C*X[3]+2
    for (i in 4:n) {
      Y[i]=0.35*Y[i-1]+0.2*Y[i-2]+0.15*rnorm(1)+C*X[i]+2
    }
  }
  if(model==2){
    eps=rnorm(n,0,1)
    delta=rnorm(n,0,1)
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
  }
  I=c()
  r=c()
  X=X[(2*d+1):n]
  Y=Y[(2*d+1):n]
  m=length(X)
  for (j in 1:m) {
    r[j]=max(min(0.3*sin(j*2*pi/n)+0.5+0.1*runif(1, -0.25, 0.25),1),0.01)
  }
  s=c()
#  for (j in 0:4){
 #   s[c(1:n/5)+j*n/5]=runif(1,0.35,0.65)
  #}
  for (j in 0:4) {
    s[c((1+j*m/5):((j+1)*m/5))]=runif(1,0.2,0.8)
  }
  #r=s
  #for (j in 0:4) {
    #r[c((1+j*m/5):((j+1)*m/5))]=runif(1,0.2,0.8)
  #}
  #r=s
  #s=(r-0.8)^2+(r-0.2)^2
  s=r
  I=r*X
  J=s*Y
  #return(list(X=X[(2*d+1):n],Y=Y[(2*d+1):n],I=I[(2*d+1):n],J=J[(2*d+1):n],r=r[(2*d+1):n],s=s[(2*d+1):n]))
  return(list(X=X,Y=Y,I=I,J=J,r=r,s=s))
}

a=gts(624,12,2,0)
X=a$X
Y=a$Y
I=a$I
J=a$J
r=a$r
s=a$s
n=length(X)
#plot(1:576,X,"l",ylab = "X")
#plot(1:576,Y,"l",ylab = "Y")
plot(X[1:597],Y[4:600],xlab = "X",ylab = "Y")
plot(I[1:597],J[4:600],xlab = "I", ylab = "J")
plot(r)
plot(s)
plot((I[2:n]-I[1:(n-1)])[1:596],(J[2:n]-J[1:(n-1)])[4:599],xlab = expression(I[t+1]-I[t]), ylab = expression(J[t+1]-J[t]))
#plot(X,Y,xlab = "X", ylab = 'Y')
#plot(I,J,xlab = "I", ylab = "J")
cor.test(I,J)
cor.test(X,Y)
cor.test(I[2:n]-I[1:(n-1)],J[2:n]-J[1:(n-1)])
