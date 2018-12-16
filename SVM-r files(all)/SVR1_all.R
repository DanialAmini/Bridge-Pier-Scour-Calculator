rm(list=ls())

setwd("C:/Users/Computer Emergency/Desktop/scourpaper/Complex-Bridge-Pier-Scour-Calculator-1.0/NN&SVM-r files(all)")  #set working folder

rmse <- function(error)
{
  sqrt(mean(error^2))
}
#x1 (bpg/bpc)	x2 (bcol/bpc)	x3 (h0/y)	x4 (h1/y)	
#x5 (T/y)	x6 (bpc/y)	x7 (f1/bcol)	z (be/b*)

MyData = read.csv("NN-x-b3-all.txt",header=TRUE)

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
	cat('\'correlation=',cor(data__$z,predictedZ),'\n')
	cat('\'SVM - number of support vectors =',length(model$SV[,2]),'\n')
	cat('\'kernel No. = ',model$kernel,' (kernel Nos: 0=linear,2=radial)\n')
	cat('\'cost = ',model$cost,'\n')
	cat('\'epsilon = ',model$epsilon,'\n')
	cat("'vars=",names_vec,"\n")
	cat('\'scaling X with mean and stdev\n')
	cat("'''''''''''''''''''''")
	str='\n'
	for (i in 1:vars){
		str=paste(str,names_vec[i],'s=(',names_vec[i],'-',mean(MyData[,i]),')/',sd(MyData[,i]),'\n',sep='')
	}
	cat(str,'')
	cat("''''''''''''''''''''''''''''''\n")

	cat('z=-',model$rho,'\n')
	for (i in 1:length(model$SV[,2])){
		cat('z=z+',model$coefs[i],'*exp(-',model$gamma,'*(',sep='')
		for (j in 1:vars){
			cat('(',names_vec[j],'s-',model$SV[i,j],') ^ 2',sep='')
			if(j<vars){cat('+',sep='')}
		}
		cat(')) \'index=',model$index[i],sep='')
		if(i<length(model$SV[,2])){cat('\n')}
	}
	cat("\n'''''''''''''''''''''''''''''''''''''''")
	cat(paste('\nz=z*',sd(MyData[,vars+1]),'+',mean(MyData[,vars+1]),'\n\n',sep=''))
}


#for the first time you have to instal e1071
library(e1071)

write_ = function(name_,data_){

write(model$index,paste0(name_,"_index.txt"),ncolumns=1)
write(model$coefs,paste0(name_,"_coefs.txt"),ncolumns=1)
write(model$rho,paste0(name_,"_rho.txt"),ncolumns=1)
write(model$gamma,paste0(name_,"_gamma.txt"),ncolumns=1)
write(t(model$SV),paste0(name_,"_SV.txt"),ncolumns=ncol_)
write(length(model$SV[,2]),paste0(name_,"_size.txt"),ncolumns=8)
write(length(model$SV[,2]),paste0(name_,"_size.txt"),ncolumns=8)
write.svm(model,svm.file=paste0(name_,"_Rdata.svm.txt"),scale.file=paste0(name_,"_xscale.txt"),yscale.file=paste0(name_,"_yscale.txt"))
length(model$SV[,2])
predictedZ <- predict(model, data_)
plot(data_$z, predictedZ)
rmse(data_$z-predictedZ)

write(rmse(data_$z-predictedZ),paste0(name_,"_rmse.txt"))

}

write2_ = function(name_,data_){
length(model$SV[,2])
predictedZ <- predict(model, data_)
rmse(data_$z-predictedZ)

write(rmse(data_$z-predictedZ),paste0(name_,"_rmse.txt"))
plot(data_$z, predictedZ)
}


data__=MyData

ncol_=length(MyData)-1
vars=ncol_


names_vec=c('x1','x2','x3','x4','x5','x6','x7')
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))

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
#1_3, 2_19, 3_28, 4_51, 5_74, 6_88, 7_106, 8_148, 
#9_165, 10_188, 11_193, 12_206, 13_223, 14_240

######




#                                                                                                 rmse_train,test
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.199) #sv=197,0.08,0.17
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.2392)#sv=191,0.086,0.174
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.29, epsilon = 0.29)  #sv=179,0.08,0.18
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.4)   #sv=145,0.1,0.18
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.5)   #sv=129,0.11,0.18
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.66)  #sv=96,0.13,0.19
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.72)  #sv=82, 14, 0.18
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.79)  #sv=74,0.15,0.19
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 1.138) #sv=41,0.19,0.21
model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.24)  #sv=191,0.086,0.174

cat('sv = ',length(model$SV[,2]),'\n')

predictedZ <- predict(model, data__)
rmse1=rmse(data__$z-predictedZ)
rmse1
plot(data__$z, predictedZ)

#print_SVM()

MyData2 = read.csv("test_data.txt",header=TRUE)
predictedZ2=predict(model,MyData2)
rmse2=rmse(MyData2$z-predictedZ2)
rmse2
plot(MyData2$z, predictedZ2)

write2_("SVMe1071",MyData)
write_("SVMe1071",MyData)


#for sensitivity
del_var='x7'
names_vec_NEW=names_vec[!is.element(names_vec, c(del_var))]
if (length(names_vec_NEW)!=vars){vars=vars-1}
names_vec=names_vec_NEW
MyData[which( colnames(MyData)==del_var )] <- NULL
MyData2[which( colnames(MyData2)==del_var )] <- NULL
a <- as.formula(paste('z ~ ' ,paste(names_vec,collapse='+')))
data__=MyData


model <- svm(a,data=MyData, kernel  = "radial", cost = 500, gamma = 0.25, epsilon = 0.24)  #sv=189,rmse=0.110,0.125 no7
                                                                                           #185, 0.1, 0.185 no 6
                                                                                           #183, 0.1, 0.171 no 5
                                                                                           #190, 0.09, 0.2 no 4
                                                                                           #194, 0.09, 0.19 no 3
                                                                                           #186, 0.09, 0.18 no 2
                                                                                           #184, 0.11, 0.18 no 1

cat('sv = ',length(model$SV[,2]),'\n')

predictedZ <- predict(model, data__)
rmse1=rmse(data__$z-predictedZ)
rmse1
plot(data__$z, predictedZ)

#print_SVM()

MyData2 = read.csv("test_data.txt",header=TRUE)
predictedZ2=predict(model,MyData2)
rmse2=rmse(MyData2$z-predictedZ2)
rmse2
plot(MyData2$z, predictedZ2)

write2_("SVMe1071",MyData)
write_("SVMe1071",MyData)