
#
#
#
#

rm(list=ls())

setwd("C:/Users/danial/Desktop/eliminated variables/NN1Sep")  #set working folder



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
		str1=paste(str1,' +',model$coeff[[i]],"*",names_vec[i-1],sep='')
	}
	cat("\n")
	cat("'rmse_training=",rmse1,"\n")	
	cat(str1,"\n")
	cat("\n")
}

printNN <- function(model)
{
	nodes=length(model$weights[[1]][[2]])-1
	vars=length(model$weights[[1]][[1]])/nodes-1

	cat("\n")
	cat("'input =",names_vec,"\n")
 	cat("'NN-hidden nodes =",nodes,"\n")
	cat("'rmse_training = ",rmse1,"\n\n")
	cat("'formula:\n")


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
			str1=paste(str1,"+",model$weights[[1]][[1]][j+1,i],"*",names_vec[j],sep='')
		}
		str1=paste(str1,")))",sep='')
		cat(str1,"\n")
	}
	cat("\n")
}

#print(net.sum$weights)

MyData = read.csv("NN1sep-case5.txt",header=TRUE)
MyData

cat("number of data = ",length(MyData[,1]),"\n")

MyData$x1=NULL
MyData$x3=NULL

MyData

names_vec=c('x2','x4','x5','x6','x7')
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))
a
model <- lm(a,data=MyData)
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

#for sensitivity analysis
#uncomment the following 3 lines in case of sensitivity analysis

del_var='x7'
names_vec=names_vec[!is.element(names_vec, c(del_var))]
MyData[which( colnames(MyData)==del_var )] <- NULL

length(names_vec)

a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))
#a

#############################################
i=0
while(i<250){
net.sum <- neuralnet(a,data=MyData,hidden=3,act.fct="logistic");
pr.nn <- compute(net.sum,MyData[,1:length(names_vec)]);
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


