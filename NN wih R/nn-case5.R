
#
#
#
#

rm(list=ls())

setwd("C:/Users/danial/Desktop/eliminated variables")  #set working folder



rmsee <- function(error)
{
  sqrt(mean(error^2))
}

printLM <- function(model)
{
	str1='z='
	str1=paste(str1,model$coeff[[1]],sep='')
	for (i in 2:length(model$coeff))
	{
		str1=paste(str1,' +',model$coeff[[i]],'*x',i,sep='')
	}
	cat("\n")
	cat("'rmse1=",rmse1,"\n")	
	cat(str1,"\n")
	cat("\n")
}

printNN <- function(model)
{
	cat("\n")
	cat("'rmse1=",rmse1,"\n")
	nodes=length(model$weights[[1]][[2]])-1
	vars=length(model$weights[[1]][[1]])/nodes-1
	str1='z='
	#print(vars)
	#print(nodes)
	str1=paste(str1,model$weights[[1]][[2]][1,1],sep='') 
	cat(str1,"\n")

	for (i in 1:nodes){
		str1=''
		str1=paste("z=z+",model$weights[[1]][[2]][i+1,1],"*1/(1+exp(-(",model$weights[[1]][[1]][1,i],sep='')
		for (j in 1:vars)
		{
			str1=paste(str1,"+",model$weights[[1]][[1]][j+1,i],"*x",j,sep='')
		}
		str1=paste(str1,")))",sep='')
		cat(str1,"\n")
	}
	cat("\n")
}

#print(net.sum$weights)




MyData = read.csv("case2.txt",header=TRUE)
MyData
cat("number of data = ",length(MyData[,1]),"\n")

model <- lm(z~x1+x2+x3,data=MyData)
#model <- lm(z~x1+x2+x3+x1*x2+x1*x3+x2*x3,data=MyData)
predictedY <- predict(model, MyData)
rmse1=rmsee(MyData$z-predictedY)
#model
printLM(model)

library("neuralnet")
#logistic = 1/(1+exp(-x)), tanh(x)

#model    linear extended linear   NN1     NN2     NN3     NN4
#rmse    

###
kkk=1
#############################################
i=0
while(i<250){
net.sum <- neuralnet(z~x1+x2+x3,data=MyData,hidden=4,act.fct="logistic");
pr.nn <- compute(net.sum,MyData[,1:3]);
if(rmsee(MyData$z-pr.nn[[2]])<kkk){
#print(rmsee(MyData$z-pr.nn[[2]]))
print(Sys.time())
kkk=rmsee(MyData$z-pr.nn[[2]])
rmse1=kkk
#print(net.sum$weights)
printNN(net.sum)
}
}
#pr.nn
#as.numeric(pr.nn)
#############################################










MyData = read.csv("case3.txt",header=TRUE)
MyData
cat("number of data = ",length(MyData[,1]),"\n")
model <- lm(z~x1+x2+x3+x4+x5,data=MyData)
#model <- lm(z~x1+x2+x3+x1*x2+x1*x3+x2*x3,data=MyData)
predictedY <- predict(model, MyData)
rmse1=rmsee(MyData$z-predictedY)
#model
printLM(model)

library("neuralnet")
#logistic = 1/(1+exp(-x)), tanh(x)

#model    linear extended linear   NN1     NN2     NN3     NN4
#rmse    

###
kkk=1
#############################################
i=0
while(i<250){
net.sum <- neuralnet(z~x1+x2+x3+x4+x5,data=MyData,hidden=5,act.fct="logistic");
pr.nn <- compute(net.sum,MyData[,1:5]);
if(rmsee(MyData$z-pr.nn[[2]])<kkk){
#print(rmsee(MyData$z-pr.nn[[2]]))
print(Sys.time())
kkk=rmsee(MyData$z-pr.nn[[2]])
rmse1=kkk
#print(net.sum$weights)
printNN(net.sum)
}
}
#pr.nn
#as.numeric(pr.nn)
#############################################





MyData = read.csv("case4.txt",header=TRUE)
MyData
cat("number of data = ",length(MyData[,1]),"\n")
model <- lm(z~x1+x2+x3+x4,data=MyData)
#model <- lm(z~x1+x2+x1*x2,data=MyData)
predictedY <- predict(model, MyData)
rmsee(MyData$z-predictedY)
model
printLM(model)

library("neuralnet")
#logistic = 1/(1+exp(-x)), tanh(x)

#model    linear extended linear   NN1     NN2     NN3     NN4
#rmse    

###
kkk=1
#############################################
i=0
while(i<250){
net.sum <- neuralnet(z~x1+x2+x3+x4,data=MyData,hidden=2,act.fct="logistic");
pr.nn <- compute(net.sum,MyData[,1:4]);
if(rmsee(MyData$z-pr.nn[[2]])<kkk){
print(rmsee(MyData$z-pr.nn[[2]]))
print(Sys.time())
kkk=rmsee(MyData$z-pr.nn[[2]])
#print(net.sum$weights)
printNN(net.sum)
}
}
#pr.nn
#as.numeric(pr.nn)
#############################################





MyData = read.csv("case5.txt",header=TRUE)
MyData
cat("number of data = ",length(MyData[,1]),"\n")
model <- lm(z~x1+x2+x3,data=MyData)
#model <- lm(z~x1+x2+x1*x2,data=MyData)
predictedY <- predict(model, MyData)
rmsee(MyData$z-predictedY)
model
printLM(model)

library("neuralnet")
#logistic = 1/(1+exp(-x)), tanh(x)

#model    linear extended linear   NN1     NN2     NN3     NN4
#rmse    

###
kkk=1
#############################################
i=0
while(i<250){
net.sum <- neuralnet(z~x1+x2+x3,data=MyData,hidden=1,act.fct="logistic");
pr.nn <- compute(net.sum,MyData[,1:3]);
if(rmsee(MyData$z-pr.nn[[2]])<kkk){
print(rmsee(MyData$z-pr.nn[[2]]))
print(Sys.time())
kkk=rmsee(MyData$z-pr.nn[[2]])
#print(net.sum$weights)
printNN(net.sum)
}
}
#pr.nn
#as.numeric(pr.nn)
#############################################




























