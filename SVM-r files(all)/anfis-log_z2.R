
setwd("C:/Users/danial/Desktop/eliminated variables/ANFIS")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 log(bpg/bpc+.05)	x2 log(bcol/bpc+.05)	x3 log(h0/y+2.1)	x4 log(h1/y+1.55)	
#x5 log(T/y+.05)	x6 log(bpc/y)	x7 log(f1/bcol+.05)	z log(be/b*+.05)

MyData = read.csv("anfis_log_data_z2.txt",header=TRUE)


library("anfis")
require("parallel")
if(.Platform$OS.type == "windows"){
options(mc.cores=5)
}else{
options(mc.cores=5) 
}

membershipFunction<-list(
x1m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-.91,sigma=.2)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=-.25,sigma=.2))),

x2m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-1.3,sigma=0.4)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=.02,sigma=0.4))),

x3m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=.29,sigma=0.80))),

x4m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=.00,sigma=0.1)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=.24,sigma=0.1)),
      new(Class="NormalizedGaussianMF",parameters=c(mu=.41,sigma=0.1))),

x5m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-.48,sigma=0.50))),

x6m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=-.13,sigma=0.78))),

x7m=c(new(Class="NormalizedGaussianMF",parameters=c(mu=.27,sigma=0.5))))

X=MyData[,1:7]
Y=MyData[,8,drop=FALSE]

X= as.matrix(as.data.frame(lapply(X, as.numeric)))
typeof(X)
typeof(Y)
Y= as.matrix(as.data.frame(lapply(Y, as.numeric)))

anfis3 <- new(Class="ANFIS",X,Y,membershipFunction)

trainOutput <- trainHybridJangOffLine(anfis3, epochs=10)


summary(anfis3)
coef(anfis3)

predictedY <- predict(anfis3,X)
err=rmse(MyData$z-predictedY)
err

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