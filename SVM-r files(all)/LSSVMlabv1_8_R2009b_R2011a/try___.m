X = linspace(-1,1,50);
Y = (15*(X.^2-1).^2.*X.^4).*exp(-X);
type = 'function estimation';

Yp = lssvm(X,Y,type);