regula = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ****** In Regula Falsi Method ****** \n")
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
    if (f(x0) * f(x1) < 0) {
      break()
    }
  }
  e <- as.numeric(readline("Enter error tolerance: "))
  maxitr <- 30
  x <- c()
  k <- c(1)
  x0k <- c(x0)
  x1k <- c(x1)
  i <- 1
  while (i < maxitr) {
    x2 <- x1 - ((x1-x0)/(f(x1)-f(x0)))*f(x1)
    x = c(x,x2)
    if (f(x0) * f(x2) < 0) {
      x1 <- x2
    } else {
      x0 <- x2
    }
    if (abs(f(x2)) < e) {
      done = 1
      break()    
    }
    i <- i + 1
    x0k <- c(x0k, x0)
    x1k <- c(x1k, x1)
    k <- c(k, i)
  }
  
  dfb2 = data.frame("x[i]" = x0k, "x[i+1]" = x1k, "x[i+2]" = x, "f(x[i+2])" = f(x), check.names = FALSE)
  print(dfb2)
  if (done == 1) {
    cat("\nConverging and Root after", i, "iterations is:", x[i],"\n")
  } else {
    cat("\nNot Converging and Root after", i, "iterations is:", x[i],"\n")
  }
}
regula()
carryon()