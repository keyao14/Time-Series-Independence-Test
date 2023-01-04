library(doParallel); cl <- makeCluster(50); registerDoParallel(cl)

source('WNTest.R')
source('WNModel.R')

depend_packages = c('energy','pcaPP')
for (i in c(1:3)) {
  for(C in (c(0:5)*0.06)){
    n = 600
    b = 10
    m = 6
    Err <- foreach(rep=1:500, .combine='cbind',.packages = depend_packages) %dopar%
      {
        X=sx(n,i,C)$X
        DX=c(X[2:n]-X[1:(n-1)],X[1]-X[n])
        pvalue=c(wntest(X,b,m,5,500),wntest_kendall(X,b,m,5,500),wntest_dcov(X,b,m,5,500),Box.test(X,lag=5,type="Ljung-Box")[["p.value"]],Box.test(DX,lag=5,type="Ljung-Box")[["p.value"]])
        pvalue
      }
    
    power = rowMeans(Err<0.05)
    print(c(i,C,power))
  }
}

