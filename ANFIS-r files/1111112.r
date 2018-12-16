
setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/ANFIS-r files")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 log(bpg/bpc+.05)	x2 log(bcol/bpc+.05)	x3 log(h0/y+2.1)	x4 log(h1/y+1.55)	
#x5 log(T/y+.05)	x6 log(bpc/y)	x7 log(f1/bcol+.05)	z log(be/b*+.05)

MyData = read.csv("anfis-log_x_b3.txt",header=TRUE)
MyData2 = read.csv("test_data.txt",header=TRUE)


library("anfis")
require("parallel")
if(.Platform$OS.type == "windows"){
options(mc.cores=5)
}else{
options(mc.cores=5) 
}

membershipFunction<-list(
x1m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-1.3,sigma=.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=-0.5,sigma=.3)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=-.10,sigma=.3))),

x2m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-1.3,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=-0.5,sigma=0.3)),
	new(Class="NormalizedGaussianMF",parameters=c(mu=0.00,sigma=0.3))),

x3m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-1.2,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=0   ,sigma=0.3)),
	new(Class="NormalizedGaussianMF",parameters=c(mu=0.5 ,sigma=0.3))),

x4m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-1  ,sigma=0.3)),
	new(Class="NormalizedGaussianMF",parameters=c(mu=-0.2,sigma=0.3)),
	new(Class="NormalizedGaussianMF",parameters=c(mu=.4  ,sigma=0.3))),

x5m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-1.2,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=-0.5,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=0.2 ,sigma=0.3))),

x6m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-.7,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu= 0 ,sigma=0.3)),
	new(Class="NormalizedGaussianMF",parameters=c(mu= .6,sigma=0.3))),

x7m=c(#new(Class="NormalizedGaussianMF",parameters=c(mu=-1.3,sigma=0.3)),
	#new(Class="NormalizedGaussianMF",parameters=c(mu=-.4 ,sigma=0.3)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=.3  ,sigma=0.3))))

X=MyData[,1:7]
Y=MyData[,8,drop=FALSE]

X2=MyData2[,1:7]
Y2=MyData2[,8,drop=FALSE]

X= as.matrix(as.data.frame(lapply(X, as.numeric)))
typeof(X)
typeof(Y)
Y= as.matrix(as.data.frame(lapply(Y, as.numeric)))

X2= as.matrix(as.data.frame(lapply(X2, as.numeric)))
Y2= as.matrix(as.data.frame(lapply(Y2, as.numeric)))

anfis3 <- new(Class="ANFIS",X,Y,membershipFunction)

trainOutput <- trainHybridJangOffLine(anfis3, epochs=10)


summary(anfis3)
coef(anfis3)

predictedY <--0.05+10^( predict(anfis3,X))
MyData$z=-0.05+10^(MyData$z)
err=rmse(MyData$z-predictedY)
err


predictedY2 <--0.05+10^( predict(anfis3,X2))
MyData2$z=-0.05+10^(MyData2$z)
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

vars=7


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