load carsmall
rng 'default'  % For reproducibility
X = [Horsepower,Weight];
Y = MPG;
Mdl = fitrsvm(X,Y)
MdlStd = fitrsvm(X,Y,'Standardize',true)


MyData=csvread('NN-x-b3-all.txt',1,0); %training data, 1 row offset
X=MyData(:,1:7);
Y=MyData(:,8);