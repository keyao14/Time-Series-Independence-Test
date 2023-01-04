library(doParallel); cl <- makeCluster(50); registerDoParallel(cl)

source('CrossTest.R')
source('CrossModel.R')
depend_packages = c('energy','pcaPP','testcorr')
for(i in c(1:7)){
  for(j in (c(0:4)*0.2)){
    Err <- foreach(rep=1:500, .combine='cbind',.packages = depend_packages) %dopar%
      {
        
        simu = gts(600,12,i,j)
        I = simu$I
        J = simu$J
        pvalue=c(FOD(I,J,12,5,5,500),FOD_kendall(I,J,12,5,5,500),FOD_dcov(I,J,12,5,5,500), cc.test(I,J,max.lag=5)$pvqtilde[11])
        pvalue
      }
    
    power = rowMeans(Err<0.05)
    print(c(i,j,power))
  }
}

