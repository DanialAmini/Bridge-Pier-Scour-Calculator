rm(list=ls())

setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/NN&SVM-r files(all)")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 (bpg/bpc)	x2 (bcol/bpc)	x3 (h0/y)	x4 (h1/y)	
#x5 (T/y)	x6 (bpc/y)	x7 (f1/bcol)	z (be/b*)

MyData = read.csv("NN-x-b3-all.txt",header=TRUE)
MyData2 = read.csv("z_all_data.txt",header=TRUE)
MyData3 = read.csv("test_data.txt",header=TRUE)


names_vec=c('x1','x2','x3','x4','x5','x6','x7')
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))

library(liquidSVM)

model=lsSVM(a,MyData,display=0,scale=TRUE,folds=1)
model=lsSVM(a,MyData,display=0,folds=1)
model=lsSVM(a,MyData,folds=1,scale=TRUE)
model=lsSVM(a,MyData,folds=1)
model=lsSVM(a,MyData,display=0,scale=TRUE,folds=1,adaptivity_control=3)
model=lsSVM(a,MyData,folds=1,scale=TRUE)
model=lsSVM(a,MyData,folds=1,scale=TRUE,type="gaussian.rbf")
model=lsSVM(a,MyData,folds=1,scale=1,gammas=c(2),c_values=c(0.1),lambdas=c(0.1))
model=lsSVM(a,MyData,folds=4,scale=TRUE)
result=test(model, MyData)
model=lsSVM(a,MyData,folds=1,scale=1,gammas=c(0.1),c_values=c(0.0001))
model=lsSVM(a,MyData,folds=1,scale=1,gammas=c(0.01),c_values=c(0.1),lambdas=c(0.2))
model=lsSVM(a,MyData,folds=2,scale=0,type = c("gaussian.rbf"),gammas=c(.01),c_values=c(4))
model=lsSVM(a,MyData,folds=1,scale=1,type = c("gaussian.rbf"),lambdas=c(.00001))
model=lsSVM(a,MyData,folds=4,scale=1,lambdas=c(0.0001))
mdel=lsSVM(a,MyData,folds=1,scale=1)
model=lsSVM(a,MyData,folds=1,scale=1,lambdas=c(0.000001))
model=lsSVM(a,MyData,folds=1,scale=1,lambdas=c(0.0001))
model=lsSVM(a,MyData,folds=1,scale=1,lambdas=c(0.001))
model=lsSVM(a,MyData,folds=1,scale=1,gammas=c(1/20000000))
model=lsSVM(a,MyData,folds=1,scale=0,gammas=c(1/6500000))
model=lsSVM(a,MyData,folds=3,scale=1,gammas=c(0.01),type = c("gaussian.rbf"))

model=lsSVM(a,MyData,folds=1,scale=0,gammas=c(0.5),type=c("guassian.rbf"))
model=lsSVM(a,MyData,folds=3,scale=1,gammas=c(0.25),c_values=c(100.100),type=c("guassian.rbf"))
model=lsSVM(a,MyData,folds=3,scale=1,type=c("guassian.rbf"))


model=lsSVM(a,MyData,folds=1,scale=1,type=c("guassian.rbf"),gammas=c(0.35))
model=lsSVM(a,MyData,folds=2,scale=1,type=c("guassian.rbf"),gammas=c(0.15))
model=lsSVM(a,MyData,folds=4,scale=1,type=c("guassian.rbf"),gammas=c(0.45))
model=lsSVM(a,MyData,folds=3,scale=1,type=c("guassian.rbf"),gammas=c(0.35))
model=lsSVM(a,MyData,folds=1,scale=1,type=c("guassian.rbf"),gammas=c(0.46),c_cost=c(0.005000))


sol=getSolution(model,fold=1)
length(sol$coeff)

result=test(model, MyData,fold=2)
plot(MyData$z,result)
rmse(MyData$z-result)

result=test(model, MyData3,fold=2)
plot(MyData3$z,result)
rmse(MyData3$z-result)

result=test(model, MyData2,fold=2)
plot(MyData2$z,result)
rmse(MyData2$z-result)



sol=getSolution(model,fold=3)
length(sol$coeff)
length(sol$sv)

#model

sol$coeff
sol$sv

#length(sol$labels)
#sol$labels = MyData$z
#result
#print(model)


write(sol$sv,paste0("LS_SVM_index.txt"),ncolumns=1)
write(sol$coeff,paste0("LS_SVM_coeff.txt"),ncolumns=1)


cat(paste( sol$coeff,collapse='\t'), file="LS_SVM_coeff.txt")
cat(paste( sol$sv,collapse="\t"), file="LS_SVM_index.txt")



MyData4 = read.csv("svm_regr_manual.txt",header=TRUE)
lm(z~.,data=MyData4)