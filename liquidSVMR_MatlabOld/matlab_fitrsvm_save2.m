clc;
clear;
%rng 'default'  % For reproducibility

MyData=csvread('NN-x-b3-all.txt',1,0); %training data, 1 row offset
X=MyData(:,1:7);
Y=MyData(:,8);

%Mdl = fitrsvm(X,Y)
%MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf')
%MdlStd =
%fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf','ShrinkagePeriod',1000)
%%

%MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf','RemoveDuplicates',true)
%MdlStd = fitrsvm(X,Y,'KFold',10)
%MdlStd = fitrsvm(X,Y,'Standardize',false, 'KernelFunction','rbf')
%MdlStd = fitrsvm(X,Y,'Standardize',true, 'KernelFunction','rbf')
%MdlStd = fitrsvm(X,Y,'Leaveout','on')
%MdlStd = fitrsvm(X,Y,'Holdout',0.1)
%MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf','Epsilon',0.1,'BoxConstraint',10)
%MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf','BoxConstraint',0.01)
MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf','Epsilon',0.05)
MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf')
%MdlStd = fitrsvm(X,Y,'Standardize',true,'KernelFunction','rbf')
%MdlStd = fitrsvm(X,Y,'KernelFunction','rbf')
yfit=predict(MdlStd,X);
scatter(Y,yfit)

sum(MdlStd.IsSupportVector)

sqrt(mean((Y-yfit).^2))


MyData2=csvread('test_data.txt',1,0); %training data, 1 row offset
X2=MyData2(:,1:7);
Y2=MyData2(:,8);
yfit2=predict(MdlStd,X2);
figure();
scatter(Y2,yfit2)
sqrt(mean((Y2-yfit2).^2))