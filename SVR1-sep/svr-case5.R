rm(list=ls())

setwd("C:/Users/danial/Desktop/eliminated variables/SVMsep")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 (bpg/bpc)	x2 (bcol/bpc)	x3 (h0/y)	x4 (h1/y)	
#x5 (T/y)	x6 (bpc/y)	x7 (f1/bcol)	z (be/b*)

MyData = read.csv("case5.txt",header=TRUE)

MyData




printLM <- function(model)
{
	str1='z='
	str1=paste(str1,model$coeff[[1]],sep='')
	for (i in 2:length(model$coeff))
	{
		str1=paste(str1,' +',model$coeff[[i]],"*",names_vec[i-1],sep='')
	}
	cat("\n")
	cat("'linear model,rmse_training=",rmse1,"\n")	
	cat("'vars=",names_vec,"\n")
	cat(str1,"\n")
	cat("\n")
}


print_SVM <- function()
{
	cat('\n\'rmse_train=',rmse1,'\n')
	cat('\'SVM - number of support vectors =',length(model$SV[,2]),', number of data = ',length(MyData[,1]),'\n')
	cat('\'percentage of data in model = ',round(length(model$SV[,2])/length(MyData[,1])*100,0),'%\n')
	cat('\'kernel No. = ',model$kernel,' (kernel Nos: 0=linear,2=radial)\n')
	cat('\'cost = ',model$cost,'\n')
	cat('\'epsilon = ',model$epsilon,'\n')
	cat("'vars=",names_vec,"\n")
	
	cat('\'scaling X with mean and stdev\n')
	str='\n'
	for (i in 1:vars){
		str=paste(str,names_vec[i],'s=(',names_vec[i],'-',mean(MyData[,i]),')/',sd(MyData[,i]),'\n',sep='')
	}
	cat(str,'\n')

	cat('z=-',model$rho,'\n')
	for (i in 1:length(model$SV[,2])){
		cat('z=z+',model$coefs[i],'*exp(-',model$gamma,'*(',sep='')
		for (j in 1:vars){
			cat('(',names_vec[j],'s-',model$SV[i,j],') ^ 2',sep='')
			if(j<vars){cat('+',sep='')}
		}
		cat(')) \'index=',model$index[i],'\n',sep='')
	}

	cat(paste('\nz=z*',sd(MyData[,vars+1]),'+',mean(MyData[,vars+1]),'\n\n',sep=''))
}



library(e1071)



names_vec=c('x2','x4','x5','x6','x7')
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))

MyData$x1=NULL
MyData$x3=NULL
data__=MyData



ncol_=length(MyData)-1
vars=ncol_

model=lm(a,data=MyData)
predictedZ <- predict(model, data__)
rmse1=rmse(data__$z-predictedZ)
rmse1
#write2_("razie",data__)

printLM(model)

predictedZ <- predict(model, data__)
plot(data__$z, predictedZ)

#target number of SVs
#model,sv_no
#1_3, 2_19, 3_28, 4_51, 5_74

######

#for sensitivity
del_var='x7'
names_vec_NEW=names_vec[!is.element(names_vec, c(del_var))]
if (length(names_vec_NEW)!=vars){vars=vars-1}
names_vec=names_vec_NEW
MyData[which( colnames(MyData)==del_var )] <- NULL
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))
data__=MyData


'	support vectors
'case	model1	model2	model3
'2	16	20	27
'3	13	36	52
'4	10	44	50
'5	14	38	55

#case
eps_=1.2
eps_=0.45
#eps_=0.33


cost_=500

model <- svm(a,data=MyData, kernel  = "radial", cost = cost_, gamma = 0.25, epsilon = eps_)

predictedZ <- predict(model, data__)
rmse1=rmse(data__$z-predictedZ)
rmse1
cat('sv = ',length(model$SV[,2]),'\n')
plot(data__$z, predictedZ)

print_SVM()