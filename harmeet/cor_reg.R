
##user defined functions:

cor_reg= function(){
  rm(list = ls())
  cat("\014")
  harmeeton()
  cat("\n","In CORRELATION AND REGRESSION ","\n")
  cat("Enter the data for variable X : ","\n")
  x=scan(what = double())
  cat("Enter the data for variable Y: ","\n")
  y=scan(what = double())
  #cat("X","Y","X^2","Y^2","X*Y","\n")
  n=length(x)
  xbar=mean(x)
  ybar=mean(y)
  sum_xy=0
  sum_x_sqre=0
  sum_y_sqre=0
  print(data.frame("X" = x, "Y" = y, "X.sq" = x^2, "Y.sq" = y^2, "XY" = x*y, check.names = FALSE))
  cat("\n") 
  for(i in 1:n){
    #cat(x[i],"|",y[i],"|",(x[i])^2,"|",(y[i])^2,"|",x[i]*y[i],"\n")
    sum_xy=sum_xy+(x[i]*y[i])
    sum_x_sqre=sum_x_sqre+(x[i]^2)
    sum_y_sqre=sum_y_sqre+(y[i]^2)

  }
  cov_x_y=((sum_xy)/n)-(xbar*ybar)
  varx=((sum_x_sqre/n)-(xbar^2))
  vary=((sum_y_sqre/n)-(ybar^2))
  sx=sqrt(varx)
  sy=sqrt(vary)
  
  r=(cov_x_y)/(sx*sy)
  
  byx=(r*sy)/(sx)
  bxy=(r*sx)/(sy)
  cat("The correlation is :",r,"\n")
  cat("Variance of X is :",varx,"\n")
  cat("Variance of Y is :",vary,"\n")
  cat("Covarinace between X and Y is : ",cov_x_y,"\n")
  cat("The regression coefficient of Y on X is : ",byx,"\n")
  cat("The regression coefficient of X on Y is : ",bxy,"\n")
  cat("The regression line of Y on X is:","Y-",ybar,"=",byx,"(X-",xbar,")","\n")
  cat("The regression line of X on Y is:","X-",xbar,"=",bxy,"(Y-",ybar,")","\n")
  plot(x,y,main="Scatter diagram",xlab="X",ylab="Y",col="red")
  abline(lsfit(x, y))
  }
cor_reg()
carryon()