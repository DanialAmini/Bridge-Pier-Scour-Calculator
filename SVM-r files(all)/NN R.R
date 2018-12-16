
#
#
#
#

rm(list=ls())

setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/NN&SVM-r files(all)")  #set working folder



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
	cat("'correlation=",cor(MyData$z,pr.nn[[2]]),"\n")
	cat("'rmse_training = ",rmse1,"\n")
	cat("'rmse_test = ",rmse2,"\n")
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

	axislimit_u= max( max(MyData$z),max(pr.nn[[2]]))
	axislimit_l= min( min(MyData$z),min(pr.nn[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData$z,pr.nn[[2]],xlim=xlim_,ylim=xlim_)
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")
}

#print(net.sum$weights)




MyData = read.csv("NN-x-b3-all.txt",header=TRUE)
MyData
cat("number of data = ",length(MyData[,1]),"\n")

names_vec=c('x1','x2','x3','x4','x5','x6','x7')
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
names_vec=c('x1','x2','x3','x4','x5','x6','x7')

#for sensitivity analysis
#uncomment the following 3 lines in case of sensitivity analysis


MyData2 = read.csv("test_data.txt",header=TRUE)


#############################################
i=0
while(i<-1*250){
	net.sum <- neuralnet(a,data=MyData,hidden=1,act.fct="logistic");
	pr.nn <- compute(net.sum,MyData[,1:length(names_vec)]);
	if(rmsee(MyData$z-pr.nn[[2]])<kkk){
		#print(rmsee(MyData$z-pr.nn[[2]]))
		print(Sys.time())
		kkk=rmsee(MyData$z-pr.nn[[2]])
		rmse1=kkk

		pr.nn2 <- compute(net.sum,MyData2[,1:length(names_vec)]);
		rmse2=rmsee(MyData2$z-pr.nn2[[2]])

		#print(net.sum$weights)
		printNN(net.sum)
	}
}

#pr.nn
#as.numeric(pr.nn)
#############################################


#hidden nodes	rmse_train	rmse_test
# 1 0.24 0.25
# 2 0.22 0.24
# 3 0.18 0.21
# 4 0.17 0.22
# 5 0.16 0.18
# 6 0.15 0.20
# 7 0.15 0.22
# 8 0.14 0.23
# 9 0.12 0.19
#10 0.12 0.20
#11 0.11 0.20
#12 0.10 0.20
#13 0.10 0.17

kkk=1
del_var='x1'
names_vec=names_vec[!is.element(names_vec, c(del_var))]
MyData[which( colnames(MyData)==del_var )] <- NULL
MyData2[which( colnames(MyData2)==del_var )] <- NULL
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))


#############################################
i=0
while(i<250){
	net.sum <- neuralnet(a,data=MyData,hidden=10,act.fct="logistic");
	pr.nn <- compute(net.sum,MyData[,1:length(names_vec)]);
	if(rmsee(MyData$z-pr.nn[[2]])<kkk){
		#print(rmsee(MyData$z-pr.nn[[2]]))
		print(Sys.time())
		kkk=rmsee(MyData$z-pr.nn[[2]])
		rmse1=kkk

		pr.nn2 <- compute(net.sum,MyData2[,1:length(names_vec)]);
		rmse2=rmsee(MyData2$z-pr.nn2[[2]])

		#print(net.sum$weights)
		printNN(net.sum)
	}
}

#pr.nn
#as.numeric(pr.nn)
#############################################

#without x7
#8 0.15 0.17
#13 0.12 0.16

#x7 #10 0.14 0.15
#x6 #10 0.14 0.25
#x5 #10 0.12 0.21
#x4 #10 0.13 0.24
#x3 #10 0.11 0.20
#x2 #10 0.12 0.19
#x1 #10 0.12 0.23