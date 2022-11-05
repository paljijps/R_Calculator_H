pdf_rv = function(){
  rm(list = ls())
  cat("\014")
  hitikaon()
  cat("\n ***** In ITFM *****\n")
  source("hitika/grv.R")
  var = pdf()
  f = var[[1]]
  n = var[[2]]
  show(f)
  no_of_rv = as.numeric(readline("Enter the no. of random variables to be generated: "))
  u = runif(no_of_rv)
  y=c()
  if(n==1){
    x=sqrt(u)
    p2x<-function(x){
      p_rv=x^2
      p_rv
    }
    rn_u=runif(no_of_rv,0,1)
    y=p2x(rn_u)
  }else if(n==2){
    b = as.numeric(readline("Enter the value of beta: "))
    x=-b*log(u)
    y=pexp(no_of_rv,b)
  }else if(n==3){
    m = as.numeric(readline("Enter the value of mean: "))
    s = as.numeric(readline("Enter the value of standard deviation: "))
    x=m-s*log(-log(u))
    pextreme<-function(x,m,s){
      p_rv=1-exp(-exp(-(x-m)/s))
      p_rv
    }
    rn_u=runif(no_of_rv,-Inf,Inf)
    y=pextreme(rn_u,m,s)
  }else if(n==4){
    b = as.numeric(readline("Enter the value of beta: "))
    a = as.numeric(readline("Enter the value of alpha: "))
    x=a-b*log((1-u)/u)
    plogistic<-function(x,a,b){
      p_rv=1/(1+exp(-(x-a)/b))
      p_rv
    }
    rn_u=runif(no_of_rv,-Inf,Inf)
    y=plogistic(rn_u,a,b)
  }
  cat("\nRequired random variables are: \n", x)
  library("dgof")
  results = ks.test(x,y)
  show(results)
}
pdf_rv()
carryon()