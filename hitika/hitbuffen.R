cat("\014")
hitikaon()
cat("\n***** IN BUFFEN NEEDLE PROBLEM *****\n")
buf_unif=function(){
  rm(list = ls())
  cat("\n Using runif() \n")
  cat("Processing, Please Wait...\n\n")
  l=2.5
  d=5
  N=1000000
  n=0
  deg_to_rad = function(x){
    pi*x/180
  }
  angle = deg_to_rad(180)
  for(i in 1:N){
    y = runif(1, 0, d/2)
    th = runif(1, 0, angle)
    if(y <= (l/2)*sin(th) ){
      n = n+1
    }
  }
  p = n/N
  pie = (2*l)/(p*d)
  
  cat("Value of pie:", pie)
  return(c(n,N,p,pie))
}

buf_ms=function(){
  rm(list=ls())
  cat("\n\n******************************************")
  cat("\n\nUsing Mid Square Method \n")
  cat("Processing, Please Wait...\n\n")

  
  mid_squni<-function(N,seed){
    rn<-c()
    for(i in 1:N){
      rn<-c(rn,seed)
      x <- seed^2
      x <- x %/% 100
      seed <- x %% 10000
    }
    return(rn/9999)
  }
  
  unif_rn=function(a,b,N,seed){
    x = a+(b-a)*mid_squni(N,seed)
    x
  }
  
  # t=N/8999
  # t=ceiling(n)
  
  deg_to_rad <- function(x) {
    pi * x / 180
  }
  angle = deg_to_rad(180)
  l=1.21791
  d=5
  N=1000000
  n=0
  ry=c()
  rth=c()
  
  for(i in 1111:9999){
    if( (i != 3792) || (i%%100 == 0) ){
      ry = c( ry, unif_rn(0, d/2, 120, i) )
      rth = c( rth, unif_rn(0, angle, 120, i) )
    }
  }

  for(i in 1:N){
    y<-ry[i]
    th<-rth[i]
    if( y <= ( l*sin(th) / 2 ) ) {
      n<-n+1
    }
  }
  p = n/N
  pie = (2*l)/(p*d)
  cat("Value of pie: ",pie,"\n")
  return(c(n,N,p,pie))
}

summbuffen = function(){
  rm(list=ls())
  x = buf_unif()
  y = buf_ms()
  cat("\n\n******************************************")
  cat("\n Summary \n\n")
  z = c("runif()", "Mid Square Method")
  df = data.frame("Generation Method" = z, n = c(x[1],y[1]), N = c(x[2], y[2]), p = c(x[3], y[3]),"PIE" = c(x[4], y [4]), check.names = FALSE )
  print(df)
}
summbuffen()
carryon()
