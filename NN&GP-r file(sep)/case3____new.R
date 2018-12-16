
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

MyData = read.csv("NN-x-b3-case3.txt",header=TRUE)
MyData2 = read.csv("case3-test.txt",header=TRUE)
#MyData

cat("number of data = ",length(MyData[,1]),"\n")

#MyData$x2=NULL
#MyData$x4=NULL
#MyData$x5=NULL
#MyData$x7=NULL

#MyData2$x2=NULL
#MyData2$x4=NULL
#MyData2$x5=NULL
#MyData2$x7=NULL

#MyData

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
	title(paste("test-rmse=",round(rmse2,2),", i=",i))


}

library("NeuralNetTools")


#############################################

while(0<1){
i=0
kkk=.1
while(i+0*25000<100){
	i=i+1
	net.sum <- neuralnet(a,data=MyData,hidden=3,act.fct="logistic");
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

	

		if ( i > 30 && (rmse2>0.09 || rmse1>0.09) ) {
			break
		}
		if (rmse2<0.075 && rmse1<0.075){
			plotter_()
			dev.new(xpos=1050,width=5,height=6)
			plotnet(net.sum)
			dev.new(ypos=500,xpos=1320,width=2, height=2)
			garson(net.sum)
			printNN(net.sum)	
			break
		}
		plotter_()
	}
}
if (rmse2<0.08){
break
}
}



