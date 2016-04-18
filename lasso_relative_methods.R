
library(car)
library(grpreg)
library(leaps)
library(lars)
library(parcor)
library(ncvreg)
library(elasticnet)
data(birthwt.grpreg)
birthwt<-(birthwt.grpreg[,-1])

birth<-read.table("birthweight.txt",header=TRUE)
birthwt$bwt<-birth$bwt
birthwt$age1<-birth$age
birthwt$age2<-birth$age^2
birthwt$age3<-birth$age^3
birthwt$lwt1<-birth$lwt
birthwt$lwt2<-birth$lwt^2
birthwt$lwt3<-birth$lwt^3

#vif(birthwt[,-1])

birthwt<-as.matrix(birthwt)
birthwt[,-1]<-scale(birthwt[,-1])

n<-189*8/9
group<-c(1,1,1,2,2,2,3,3,4,5,5,6,7,8,8,8)


set.seed(1)
number<-1:189
subset<-c()
for(t in 1:8){
  id<-sample(number,21,replace=F)
  subset<-c(subset,id) 
  number<-c(1:189)[-subset] 
}
subset<-c(subset,number)

y<-birthwt[,1]
x<-birthwt[,-1]

coef.best<-c()
cv.best<-c()

coef.lasso<-c()
cv.lasso<-c()

coef.ada<-c()
cv.ada<-c()

coef.SCAD<-c()
cv.SCAD<-c()

coef.enet<-c()
cv.enet<-c()

coef.glasso<-c()
cv.glasso<-c()

coef.gSCAD<-c()
cv.gSCAD<-c()

for(times in 0:8){
  id<-subset[(21*times+1):(21*times+21)]
  y.train<-birthwt[-id,1]
  x.train<-birthwt[-id,-1]
  y.test<-birthwt[id,1]
  x.test<-birthwt[id,-1]
  
  #best subset
  out<-summary(regsubsets(x.train,y.train))
  co<-out$which[which.min(out$bic),]
  xx<-cbind(rep(1,n),x.train)
  xx[,!co]=0
  fit<-lm(y.train~xx[,-1])
  coef<-fit$coefficients
  coef[is.na(coef)]=0
  coef.best<-c(coef.best,coef)
  #matrix(coef.best,ncol=17,byrow=TRUE)
  fitted.value<-cbind(rep(1,(189-n)),x.test)%*%coef
  cv.best<-c(cv.best,sum((fitted.value-y.test)^2))
  
  
  #lasso
  cv.res<-cv.lars(x.train,y.train)
  s<-cv.res$index[which.min(cv.res$cv)]
  fit<-lars(x.train,y.train, type="lasso")
  lasso.fit<-predict.lars(fit,type="coefficients",mode="fraction",s=s)
  coef<-lasso.fit$coefficients
  coef.lasso<-c(coef.lasso,coef)
  fitted.value<-predict.lars(fit,newx=x.test,type="fit",mode="fraction",s=s)$fit
  cv.lasso<-c(cv.lasso,sum((fitted.value-y.test)^2))
  #matrix(coef.lasso,ncol=16,byrow=TRUE)
  
  
  #adaptive lasso
  fit<-adalasso(x.train,y.train)
  coef<-c(fit$intercept.adalasso,fit$coefficients.adalasso)
  fitted.value<-cbind(rep(1,(189-n)),x.test)%*%coef
  coef.ada<-c(coef.ada,coef)
  cv.ada<-c(cv.ada,sum((fitted.value-y.test)^2))
  #matrix(coef.ada,ncol=17,byrow=TRUE)
  
  #SCAD
  fit<-cv.ncvreg(x.train, y.train,penalty="SCAD")
  lambda<-fit$lambda.min
  coef<-coef(fit,lambda=lambda)
  fitted.value<-cbind(rep(1,(189-n)),x.test)%*%coef
  coef.SCAD<-c(coef.SCAD,coef)
  cv.SCAD<-c(cv.SCAD,sum((fitted.value-y.test)^2))
  #matrix(coef.SCAD,ncol=17,byrow=TRUE)
  
  #elastic net
  cv<-c()
  coef<-c()
  ss<-c()
  for(la in c(0,0.01,0.05,0.1,0.5,1,10,100)){
    cv.res<-cv.enet(x.train,y.train,lambda=la,s=seq(0,1,length=100),mode="fraction")
    s<-cv.res$s[which.min(cv.res$cv)]
    fit<-enet(x.train,y.train,lambda=la)
    coef<-rbind(coef,predict(model, s=s, type="coef", mode="fraction")$coefficients)
    cv<-c(cv,min(cv.res$cv))  
    ss<-c(ss,s)
  }
  coefficient<-coef[which.min(cv),]
  coef<-coefficient
  la<-c(0,0.01,0.05,0.1,0.5,1,10,100)[which.min(cv)]
  cv.res<-cv.enet(x.train,y.train,lambda=la,s=seq(0,1,length=100),mode="fraction")
  s<-cv.res$s[which.min(cv.res$cv)]
  fit<-enet(x.train,y.train,lambda=la)
  fitted.value<-predict.enet(fit,newx=x.test,type="fit",mode="fraction",s=s)$fit
  coef.enet<-c(coef.enet,coef)
  cv.enet<-c(cv.enet,sum((fitted.value-y.test)^2))
  #matrix(coef.enet,ncol=16,byrow=TRUE)
  
  #group lasso bic
  fit<-grpreg(x.train,y.train,group,penalty="gLasso")
  coef<-grpreg::select(fit, criterion="BIC")$beta
  lambda<-grpreg::select(fit, criterion="BIC")$lambda
  fitted.value<-predict(fit, x.test, type="response", lambda=lambda)
  coef.glasso<-c(coef.glasso,coef)
  cv.glasso<-c(cv.glasso,sum((fitted.value-y.test)^2))
  #matrix(coef.glasso,ncol=17,byrow=TRUE)
  
  #group SCAD bic
  fit<-grpreg(x.train,y.train,group,penalty="grSCAD")
  coef<-grpreg::select(fit, criterion="BIC")$beta
  lambda<-grpreg::select(fit, criterion="BIC")$lambda
  fitted.value<-predict(fit, x.test, type="response", lambda=lambda)
  coef.gSCAD<-c(coef.gSCAD,coef)
  cv.gSCAD<-c(cv.gSCAD,sum((fitted.value-y.test)^2))
  #matrix(coef.gSCAD,ncol=17,byrow=TRUE)
  
 
}



c(sum(cv.best)/189,sum(cv.lasso)/189,sum(cv.ada)/189,sum(cv.SCAD)/189,sum(cv.enet)/189,sum(cv.glasso)/189,sum(cv.gSCAD)/189)
#[1] 481715.6 473911.4 466932.4 484045.2 477870.2 472114.0 482476.9

[1] 481715.6 473911.4 466932.4 484045.2 477870.2 472114.0 482476.9
m1<-matrix(coef.best,ncol=17,byrow=TRUE)
m2<-matrix(coef.lasso,ncol=16,byrow=TRUE)
m3<-matrix(coef.ada,ncol=17,byrow=TRUE)
m4<-matrix(coef.SCAD,ncol=17,byrow=TRUE)
m5<-matrix(coef.enet,ncol=16,byrow=TRUE)
m6<-matrix(coef.glasso,ncol=17,byrow=TRUE)
m7<-matrix(coef.gSCAD,ncol=17,byrow=TRUE)

l1<-round(apply(m1,2,mean),2)[-1]
l2<-round(apply(m2,2,mean),2)
l3<-round(apply(m3,2,mean),2)[-1]
l4<-round(apply(m4,2,mean),2)[-1]
l5<-round(apply(m5,2,mean),2)
l6<-round(apply(m6,2,mean),2)[-1]
l7<-round(apply(m7,2,mean),2)[-1]
cbind(l1,l2,l3,l4,l5,l6,l7)

# round(apply(m1,2,mean),2)
# [1] 2927.08    0.00   -0.41    0.01    0.96    0.00    0.00  394.86    0.00 -367.60 -131.63    0.00 -463.42 -528.18    0.00
# [16]    0.00    0.00
# round(apply(m2,2,mean),2)
# [1]   32.61   -2.62    0.05   28.20   -0.17    0.00  265.10 -110.57 -253.98 -287.63  117.38 -475.93 -458.90   78.95   14.39
# [16]  -99.93
# round(apply(m3,2,mean),2)
# [1]   79.23   34.25   -3.72    0.08   59.06   -0.37    0.00  290.56 -144.99 -286.18 -299.56  192.63 -557.68 -493.56   86.43
# [16]   24.31 -130.67
# round(apply(m4,2,mean),2)
# [1] 2152.93   -2.13   -1.58    0.04   20.98   -0.12    0.00  325.02 -105.28 -315.91 -254.07  101.95 -516.94 -507.34   70.01
# [16]   10.37  -92.67
# round(apply(m5,2,mean),2)
# [1]  -12.57    0.00    0.01    3.06    0.00    0.00  177.62 -125.72 -222.00 -203.75   -3.44 -393.89 -367.52   61.89    3.98
# [16]  -24.73
# round(apply(m6,2,mean),2)
# [1]  836.09   46.78   -2.42    0.04   36.65   -0.23    0.00  147.84  -46.80 -158.34 -141.37   39.55 -233.96 -358.18    0.00
# [16]    0.00    0.00
 
  cbind(l1,l2,l3,l4,l5,l6)
# l1      l2      l3      l4      l5      l6
# [1,]    0.00   32.61   34.25   -2.13  -12.57   46.78
# [2,]   -0.41   -2.62   -3.72   -1.58    0.00   -2.42
# [3,]    0.01    0.05    0.08    0.04    0.01    0.04
# [4,]    0.96   28.20   59.06   20.98    3.06   36.65
# [5,]    0.00   -0.17   -0.37   -0.12    0.00   -0.23
# [6,]    0.00    0.00    0.00    0.00    0.00    0.00
# [7,]  394.86  265.10  290.56  325.02  177.62  147.84
# [8,]    0.00 -110.57 -144.99 -105.28 -125.72  -46.80
# [9,] -367.60 -253.98 -286.18 -315.91 -222.00 -158.34
# [10,] -131.63 -287.63 -299.56 -254.07 -203.75 -141.37
# [11,]    0.00  117.38  192.63  101.95   -3.44   39.55
# [12,] -463.42 -475.93 -557.68 -516.94 -393.89 -233.96
# [13,] -528.18 -458.90 -493.56 -507.34 -367.52 -358.18
# [14,]    0.00   78.95   86.43   70.01   61.89    0.00
# [15,]    0.00   14.39   24.31   10.37    3.98    0.00
# [16,]    0.00  -99.93 -130.67  -92.67  -24.73    0.00  
round(apply(m1,2,sd),2)
round(apply(m2,2,sd),2)
round(apply(m3,2,sd),2)
round(apply(m4,2,sd),2)
round(apply(m5,2,sd),2)
round(apply(m6,2,sd),2)

# round(apply(m1,2,sd),2)
# [1] 203.46   0.00   1.24   0.03   1.92   0.00   0.00  37.63   0.00  60.07 199.84   0.00 275.00  39.42   0.00   0.00   0.00
# > round(apply(m2,2,sd),2)
# [1] 131.14   5.40   0.07  50.45   0.33   0.00  46.84  88.24  57.08  77.61 163.39 107.87  61.62  63.54  56.99 132.29
# > round(apply(m3,2,sd),2)
# [1] 3494.74  136.95    5.06    0.06   55.75    0.36    0.00   59.61   80.19   53.04   66.94  139.60   91.61   45.75   68.03
# [16]   56.53   87.62
# > round(apply(m4,2,sd),2)
# [1] 1753.14   88.24    3.36    0.05   29.04    0.19    0.00   81.92  108.47   79.42  117.21  151.72  166.75   41.15   82.66
# [16]   35.46  125.80
# > round(apply(m5,2,sd),2)
# [1]  15.09   0.00   0.01   1.48   0.00   0.00  87.89  81.78 110.07  94.26  10.32 227.03 113.47  40.90  11.71  24.79
# > round(apply(m6,2,sd),2)
# [1] 883.22  40.60   1.40   0.02  19.42   0.12   0.00  40.91  31.96  53.67  67.15  53.59 125.62  29.82   0.00   0.00   0.00
  
