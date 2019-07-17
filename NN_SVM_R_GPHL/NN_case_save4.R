

#
#
#
#
	graphics.off()
rm(list=ls())

sink('NN_res/NN_res.txt')
cat(" ")
sink(NULL)
sink.reset()

library("neuralnet")
library("NeuralNetTools")

setwd("E:/scour_main/complex/softpaper/NN_SVM")


#functions

sink.reset <- function(){
    for(i in seq_len(sink.number())){
        sink(NULL)
    }
}

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
	#sink(paste('NN_res/NN_res_,case',case_,'hidden=',hidden_,'.txt'))
	sink('NN_res/NN_res.txt',append=TRUE)
	nodes=length(model$weights[[1]][[2]])-1
	vars=length(model$weights[[1]][[1]])/nodes-1

	cat("\n")
	cat("'case=",case_,"\n")
	cat("'input =",names_vec,"\n")
 	cat("'NN-hidden nodes =",nodes,"\n")
	cat("'rmse_training = ",rmse1,"\n")
	cat("'rmse_train_val = ",rmse2,"\n")
	cat("'rmse_test = ",rmse3,"\n")
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
	sink(NULL)
	sink.reset()
}


plotter_ <- function(){

	graphics.off()

	#train
	windows(width =10, height = 4, pointsize = 12,xpos=50,ypos=100)
	par(mfrow=c(1,3))

	axislimit_u= max( max(MyData$z),max(pr.nn[[2]]))
	axislimit_l= min( min(MyData$z),min(pr.nn[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData$z,pr.nn[[2]],xlim=xlim_,ylim=xlim_,main=paste("rmse_train=",round(rmse1,3)),xlab="",ylab="be/b* predicted")
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")

	#train_val
#	windows(width = 5, height = 3, pointsize = 12,xpos=500,ypos=500)
	axislimit_u= max( max(MyData2$z),max(pr.nn2[[2]]))
	axislimit_l= min( min(MyData2$z),min(pr.nn2[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData2$z,pr.nn2[[2]],xlim=xlim_,ylim=xlim_,main=paste("rmse_vali=",round(rmse2,3)),xlab="be/b* observed",ylab="")
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")

	#test
#	windows(width = 5, height = 3, pointsize = 12,xpos=1000,ypos=500)
	axislimit_u= max( max(MyData3$z),max(pr.nn3[[2]]))
	axislimit_l= min( min(MyData3$z),min(pr.nn3[[2]]),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData3$z,pr.nn3[[2]],xlim=xlim_,ylim=xlim_,main=paste("rmse_test=",round(rmse3,3)),xlab="",ylab="")
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")

	dev.print(pdf, paste('NN_res/NN_scatter,case',case_,'hidden=',hidden_,'.pdf'))

}


case_=2
for (case_ in 2:5){
for (hidden_ in 1:3){
#hidden_=3

#cat("number of data = ",length(MyData[,1]),"\n")



if (case_==2){
	names_vec=c('x1','x3','x6')  #case2

	MyData = read.csv("case2-train.csv",header=TRUE)
	MyData2 = read.csv("case2-trainVal.csv",header=TRUE)
	MyData3 = read.csv("case2-test.csv",header=TRUE)

	MyData$x2=NULL
	MyData$x4=NULL
	MyData$x5=NULL
	MyData$x7=NULL

	MyData2$x2=NULL
	MyData2$x4=NULL
	MyData2$x5=NULL
	MyData2$x7=NULL

	MyData3$x2=NULL
	MyData3$x4=NULL
	MyData3$x5=NULL
	MyData3$x7=NULL

} else if (case_==3){
	names_vec=c('x1','x2','x3','x4','x5','x6','x7')  #case 3

	MyData = read.csv("case3-train.csv",header=TRUE)
	MyData2 = read.csv("case3-trainVal.csv",header=TRUE)
	MyData3 = read.csv("case3-test.csv",header=TRUE)
} else if (case_==4){

	MyData = read.csv("case4-train.csv",header=TRUE)
	MyData2 = read.csv("case4-trainVal.csv",header=TRUE)
	MyData3 = read.csv("case4-test.csv",header=TRUE)

	names_vec=c('x1','x2','x3','x4','x5','x6','x7')
} else if (case_==5){
	MyData = read.csv("case5-train.csv",header=TRUE)
	MyData2 = read.csv("case5-trainVal.csv",header=TRUE)
	MyData3 = read.csv("case5-test.csv",header=TRUE)

	names_vec=c('x2','x4','x5','x6','x7')

	MyData$x1=NULL
	MyData$x3=NULL

	MyData2$x1=NULL
	MyData2$x3=NULL

	MyData3$x1=NULL
	MyData3$x3=NULL
}

a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))


model <- lm(a,data=MyData)
predictedY <- predict(model, MyData)
rmse1=rmsee(MyData$z-predictedY)
printLM(model)



length(names_vec)

kkk1=1
kkk2=1
kkk3=1

#############################################
i=0

#startweights=net.sum$weights

time_=as.numeric(Sys.time())
while(i<1000 && as.numeric(Sys.time())-time_<61*6){

	ii=20;

	mod2 <- neuralnet(a,data=MyData,hidden=hidden_,act.fct="logistic");

	pr.nn <- compute(mod2,MyData[,1:length(names_vec)]);
	kkk1N=rmsee(MyData$z-pr.nn[[2]])

	pr.nn2 <- compute(mod2,MyData2[,1:length(names_vec)]);
	kkk2N=rmsee(MyData2$z-pr.nn2[[2]])

	pr.nn3 <- compute(mod2,MyData3[,1:length(names_vec)]);
	kkk3N=rmsee(MyData3$z-pr.nn3[[2]])

	kkk1N
	kkk2N

#	if((i<ii && kkkN<kkk && kkk2N<2*kkk) || (kkkN<kkk && kkk2N<kkk2)){
#	if( kkk1N+kkk2N<kkk1+kkk2){
	if( kkk2N<kkk2){
#	if( kkk1N<kkk1){
#	if( kkk1N<kkk1 && kkk2N<kkk2){
#	if( (i<ii && kkkN<kkk) || (kkkN+kkk2N<kkk+kkk2)){


		rmse1=kkk1N
		rmse2=kkk2N
		rmse3=kkk3N

		mod=mod2

		print(Sys.time())
		kkk1=kkk1N
		kkk2=kkk2N
		kkk3=kkk3N

		cat("i=",i,",rmse_train=",rmse1,",rmse_test=",rmse2,",rmse_test=",rmse3,"\n")

		#printNN(mod)
		#plotter_()


		#dev.new(ypos=500,xpos=1320,width=4, height=4)

	}
	i=i+1
}

#pr.nn
#as.numeric(pr.nn)
#############################################

 

	printNN(mod)
	plotter_()


	dev.new(ypos=100,xpos=700,width=4,height=5)
	plotnet(mod)
	dev.print(pdf, paste('NN_res/NN_network,case',case_,'hidden=',hidden_,'.pdf'))
	

	dev.new(ypos=100,xpos=1200,width=2.5, height=2.5)
	abc=garson(mod,main="hi")
	print(abc)
	dev.print(pdf, paste('NN_res/NN_garson,case',case_,'hidden=',hidden_,'.pdf'))

}}