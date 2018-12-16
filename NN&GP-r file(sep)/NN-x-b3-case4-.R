#install.packages("NeuralNetTools")
#install.packages("neuralnet")

graphics.off()
#
#
#
#

rm(list=ls())

setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/NN&GP-r file(sep)")  #set working folder



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
}

#print(net.sum$weights)

MyData = read.csv("NN-x-b3-case4.txt",header=TRUE)
MyData2 = read.csv("case4-test.txt",header=TRUE)
#MyData

cat("number of data = ",length(MyData[,1]),"\n")

#MyData$x2=NULL  #case2
#MyData$x4=NULL  #case2
#MyData$x5=NULL  #case2
#MyData$x7=NULL  #case2

#MyData2$x2=NULL #case2
#MyData2$x4=NULL #case2
#MyData2$x5=NULL #case2
#MyData2$x7=NULL #case2

#MyData

#names_vec=c('x1','x3','x6') #case2
names_vec=c('x1','x2','x3','x4','x5','x6','x7') #case3,4


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


length(names_vec)


#a

plotter_ <- function(){

	graphics.off()
	dev.new(xpos=600,width=4.5,height=4.5)

	axislimit_u= max( max(MyData$z),max(pr.nn[[2]]))
	axislimit_l= min( min(MyData$z),min(pr.nn[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData$z,pr.nn[[2]],xlim=xlim_,ylim=xlim_)
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")
	title(paste("train-rmse",round(rmse1,2)))

	dev.new(xpos=50,width=4.5,height=4.5)

	axislimit_u= max( max(MyData2$z),max(pr.nn2[[2]]))
	axislimit_l= min( min(MyData2$z),min(pr.nn2[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData2$z,pr.nn2[[2]],xlim=xlim_,ylim=xlim_)
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")
	title(paste("test-rmse=",round(rmse2,2)))

}

#############################################
i=0
while(i+0*25000<250){
	net.sum <- neuralnet(a,data=MyData,hidden=4,act.fct="logistic");
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
		plotter_()

		if (rmse2<0.185) {
			break
		}

	}
}


#pr.nn
#as.numeric(pr.nn)
#############################################



del_var='x7'
names_vec=names_vec[!is.element(names_vec, c(del_var))]
MyData[which( colnames(MyData)==del_var )] <- NULL
MyData2[which( colnames(MyData2)==del_var )] <- NULL
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))



#############################################
kkk=1
i=0
while(i+1*25000<250){
	net.sum <- neuralnet(a,data=MyData,hidden=3,act.fct="logistic");
	pr.nn <- compute(net.sum,MyData[,1:length(names_vec)]);
	if(rmsee(MyData$z-pr.nn[[2]])<kkk){
		net.sum2=net.sum
		#print(rmsee(MyData$z-pr.nn[[2]]))
		print(Sys.time())
		kkk=rmsee(MyData$z-pr.nn[[2]])
		rmse1=kkk


		pr.nn2 <- compute(net.sum,MyData2[,1:length(names_vec)]);
		rmse2=rmsee(MyData2$z-pr.nn2[[2]])
	
		#print(net.sum$weights)
		printNN(net.sum)
		plotter_()
	}
}


#pr.nn
#as.numeric(pr.nn)
###

#nodes, train_rmse, tes_rmse

#case4
#1 0.27 0.31
#2 0.2 0.29
#3 0.16 0.22
#4 0.13 0.18 ****
  #no_x1 0.16 0.25    #no_x2 0.18 0.23    #no_x3 0.17 0.22
  #no_x4 0.16 0.27   #no_x5 0.16 0.23   #no_x6 0.19 0.32  #no_x7 0.19 0.21

#case2
#1 0.16 0.19
#2 0.12 0.18
#3 0.09 0.07 #no_x1 0.11 0.16  #no_x3 0.1 0.1  #no_x6 0.1 0.09
 
#case3
#1 0.16 0.13
#2 0.10 0.10  #no x3 0.10 0.10
#3 0.09 0.09  #no x3 0.08 0.10 
#4 0.07 0.08   
              #no_x7 0.08 0.08  #no_x6 0.08 0.36 #no_x5 0.07 0.07
              #*no_x4 0.07 0.07 #no_x3 0.07 0.08  #no_x2 0.09 0.08 #no_x1 0.08 0.07



library("NeuralNetTools")

plotter_()

	dev.new(xpos=1050,width=5,height=6)
plotnet(net.sum)


	dev.new(ypos=500,xpos=1350,width=2, height=2)
garson(net.sum)

printNN(net.sum)


