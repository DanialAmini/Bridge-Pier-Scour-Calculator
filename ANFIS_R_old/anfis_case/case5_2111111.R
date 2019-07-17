graphics.off()

setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/ANFIS-r files/anfis_case")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 log(bpg/bpc+.05)	x2 log(bcol/bpc+.05)	x3 log(h0/y+2.1)	x4 log(h1/y+1.55)	
#x5 log(T/y+.05)	x6 log(bpc/y)	x7 log(f1/bcol+.05)	z log(be/b*+.05)

MyData = read.csv("case5-train.txt",header=TRUE)
MyData2 = read.csv("case5-test.txt",header=TRUE)

MyData$x1=NULL  #case5
MyData$x3=NULL  #case5

MyData2$x1=NULL #case5
MyData2$x3=NULL #case5

#MyData$x2=NULL  #case2
#MyData$x4=NULL  #case2
#MyData$x5=NULL  #case2
#MyData$x7=NULL  #case2

#MyData2$x2=NULL #case2
#MyData2$x4=NULL #case2
#MyData2$x5=NULL #case2
#MyData2$x7=NULL #case2


library("anfis")
require("parallel")
if(.Platform$OS.type == "windows"){
options(mc.cores=5)	
}else{
options(mc.cores=5) 
}

membershipFunction<-list(
#x1m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=0.12,sigma=0.15)),
	 #new(Class="NormalizedGaussianMF",parameters=c(mu=0.35,sigma=0.15)),
       #new(Class="NormalizedGaussianMF",parameters=c(mu=0.69,sigma=0.15))),

x2m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=0.18,sigma=0.15)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=0.4 ,sigma=0.15)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=0.85,sigma=0.15))),

#x3m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-1.43,sigma=0.3)),
       #new(Class="NormalizedGaussianMF",parameters=c(mu=-.7  ,sigma=0.3)),
       #new(Class="NormalizedGaussianMF",parameters=c(mu=  0.00,sigma=0.3))),

x4m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-1.5,sigma=0.4)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=-.8 ,sigma=0.4)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=0.00,sigma=0.4))),

x5m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=0.01,sigma=0.2)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=0.4 ,sigma=0.2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=0.77 ,sigma=0.2))),

x6m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=0.20,sigma=1)),
      #new(Class="NormalizedGaussianMF",parameters=c(mu=1.00,sigma=.3)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=2.29,sigma=1))),

x7m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=0.0,sigma=0.4)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=0.8,sigma=0.4)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=1.83,sigma=0.4))))

X=MyData[,1:5]
Y=MyData[,6,drop=FALSE]

X2=MyData2[,1:5]
Y2=MyData2[,6,drop=FALSE]

X= as.matrix(as.data.frame(lapply(X, as.numeric)))
typeof(X)
typeof(Y)
Y= as.matrix(as.data.frame(lapply(Y, as.numeric)))

X2= as.matrix(as.data.frame(lapply(X2, as.numeric)))
Y2= as.matrix(as.data.frame(lapply(Y2, as.numeric)))

anfis3 <- new(Class="ANFIS",X,Y,membershipFunction)

trainOutput <- trainHybridJangOffLine(anfis3, epochs=20)


summary(anfis3)
coef(anfis3)

predictedY <- predict(anfis3,X)
err=rmse(MyData$z-predictedY)
err


predictedY2 <- predict(anfis3,X2)
err2=rmse(MyData2$z-predictedY2)
err2

	graphics.off()
	dev.new()

	axislimit_u= max( max(MyData$z),max(predictedY))
	axislimit_l= min( min(MyData$z),min(predictedY),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData$z,predictedY,xlim=xlim_,ylim=xlim_)
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")
	title(paste("train-rmse",round(err,2)))

	dev.new(xpos=100)

	axislimit_u= max( max(MyData2$z),max(predictedY2))
	axislimit_l= min( min(MyData2$z),min(predictedY2),0)
	xlim_=c(axislimit_l,axislimit_u)
	plot(MyData2$z,predictedY2,xlim=xlim_,ylim=xlim_)
	lines(c(0,axislimit_u),c(0,axislimit_u),col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*1.2),lty=2,col="red")
	lines(c(0,axislimit_u),c(0,axislimit_u*.8),lty=2,col="red")
	title(paste("test-rmse=",round(err2,2)))

	dev.new()



vars=5


list_coef=coef(anfis3)$consequents

no_functions=length(list_coef)/(vars+1)

for (i in 1:no_functions)
{
	if(i==1){cat("\n")}
	str=i
	str=paste('z',i,'=',list_coef[(i-1)*(vars+1)+vars+1], sep = '')
	for (j in 1:vars)
	{
		str=paste(str,'+',list_coef[(i-1)*(vars+1)+j],'*x',j, sep = '')
	}
	str=paste(str,"\n")
	cat(str)
	if(i==no_functions){cat("\n")}
}


list_coef


for (i in 1:vars){
	if(i==1){cat("\n")}
	#if(length(membershipFunction[[i]])>1){
	for (j in 1:length(membershipFunction[[i]])){
		str=""
		mu__=getPremises(anfis3)[[i]][[j]][1][[1]]
		stdev__=getPremises(anfis3)[[i]][[j]][2][[1]]
		str=paste(str,"w",i,'_',j,"=exp(-0.5*(x",i,"-",mu__,") ^ 2/",stdev__," ^ 2)",sep="")
		cat(str,"\n")
	}
	cat("\n")
	#}
}

for (i in 1:no_functions){
	str=paste('ww',i,'=',sep='')
	for (j in 1:vars){
		str=paste(str,'w',j,'_',getRules(anfis3)[i,j],sep='')
		if(j<vars){str=paste(str,'*',sep='')}
	}
	cat(str,"\n")
}

str='\nwwN='
str2='\nz='
for (i in 1:no_functions){
	str=paste(str,'ww',i,sep='')
	str2=paste(str2,'z',i,'*ww',i,sep='')
	if(i<no_functions){str=paste(str,'+',sep='')}
	if(i<no_functions){str2=paste(str2,'+',sep='')}
}
str=paste(str,'\n\n')
str2=paste(str2,'\n\n')

cat(str)
cat(str2)
	
fitted.values(anfis3)

plotMFs(anfis3)

#case3
#archi  rmsetrain test
#1111111 0.18 0.14
#2111111 0.18 0.12
#1211111 0.16 0.16
#1121111 0.17 0.13
#1112111 0.17 0.13
#1111211 0.17 0.15
#1111121 0.14 0.11
#1111112 0.18 0.13
#1211121 0.10 0.07

#case4
#archi  rmsetrain test
#1111111 0.31 0.30
#2111111 0.27 0.31
#1211111 0.26 0.28
#1121111 0.30 0.30
#1112111 0.28 0.27
#1111211 0.28 0.27
#1111121 0.26 0.26
#1111112 0.27 0.27

#1211121 0.24 0.28
#1111122 0.29 0.24
#1112112 0.21 0.28
#2111121 0.23 0.28
#1111221 0.21 0.21 **
#1121211 0.28 0.25
#1121121 0.29 0.28

#1111131 0.19 0.23

#1111222 0.19 0.23
#2211121 0.21 0.27

#case 5
#0101111 0.21 0.24
#0201111 0.18 0.26
#0102111 0.20 0.25
#0101211 0.19 0.21
#0101121 0.16 0.20 sigma=0.5
#0101112 0.18 0.26

#0101131 0.14 0.21
#0301111 0.16 0.18
#0103111 0.18 0.25
#0101311 0.16 0.20
#0101113 0.16 0.24

#0101221 0.19 0.20
#0201121 0.14 0.20
#0102121 0.17 0.24
#0101122 0.32 0.27