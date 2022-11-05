
muller = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ****** In Muller Method ****** \n")
  done = 0
  source("jaswinder/fun.R")
  funcs <- eq_select()
  f <- funcs[[1]]
  show(f)
  #fd <- funcs[[2]]
  #fdd <- funcs[[3]]
  #phi <- funcs[[4]]
  intervals(f)
  
  while (TRUE) {
    x0 <- as.numeric(readline("Enter first initial approximation: "))
    x1 <- as.numeric(readline("Enter second initial approximation: "))
    x2 <- as.numeric(readline("Enter third initial approximation: "))
    if (f(x0) * f(x2) < 0) {
      break()
    }
  }
  e <- as.numeric(readline("Enter error tolerance: "))
  maxitr <- 30
  x <- c()
  k <- c()
  x0k <- c()
  x1k <- c()
  x2k <- c()
  i <- 1
  while (i < maxitr) {
    h2 = x2 - x1
    h1 = x1 - x0
    l2 = h2/h1
    d2 = 1 + l2
    g2 = (l2^2)*f(x0) - (d2^2)*f(x1) + (l2+d2)*f(x2)
    c2 = l2*(l2*f(x0) - d2*f(x1) + f(x2))
    if (g2 < 0) {
      l3 = -2*d2*f(x2)/(g2 - sqrt(g2^2 - 4*d2*f(x2)*c2))
    }else{
      l3 = -2*d2*f(x2)/(g2 + sqrt(g2^2 - 4*d2*f(x2)*c2))
    }
    x3 = x2 + (x2 - x1)*l3
    x = c(x,x3)
    x0k <- c(x0k, x0)
    x1k <- c(x1k, x1)
    x2k <- c(x2k, x2)
    k <- c(k, i)
    x0 = x1
    x1 = x2
    x2 = x3
    if (abs(f(x3)) < e) {
      done = 1
      break()    
    }
    i <- i + 1
  }
  
  dfb2 = data.frame("x[i]" = x0k, "x[i+1]" = x1k, "x[i+2]" = x2k, "x[i+3]" = x ,"f(x[i+3])" = f(x) ,check.names = FALSE)
  print(dfb2)
  if (done == 1) {
    cat("\nConverging and Root after", i, "iterations is:", x3,"\n")
  } else {
    cat("\nNot Converging and Root after", i, "iterations is:", x3,"\n")
  }
}
muller()
carryon()