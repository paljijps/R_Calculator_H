aitken = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ****** In Aitken Delta Square Method ****** \n")
  done = 0
  source("jaswinder/fun.R")
  funcs <- eq_select()
  f <- funcs[[1]]
  show(f)
  intervals(f)
  #fd <- funcs[[2]]
  #fdd <- funcs[[3]]
  phi <- funcs[[4]]
 
  
  x0 <- as.numeric(readline("Enter one initial approximation: "))
  e <- as.numeric(readline("Enter error tolerance: "))
  maxitr <- 30
  x <- c()
  k <- c()
  x0k <- c()
  x1k <- c()
  x2k <- c()
  i <- 1
  while (i < maxitr) {
    x1 <- phi(x0)
    x2 <- phi(x1)
    x0s <- x0 - ( ((x1 - x0)^2) / (x2 - 2*x1 + x0) ) 
    x <- c(x,x0s)
    x0k <- c(x0k, x0)
    x1k <- c(x1k, x1)
    x2k <- c(x2k, x2)
    k <- c(k, i)
    if (abs(f(x0s)) < e) {
      done = 1
      break()    
    }
    x0 = x0s
    i <- i + 1
  }
  
  dfb2 = data.frame("x0" = x0k, "x1" = x1k, "x2" = x2k, "x0*" = x, "f(x0*)" = f(x), check.names = FALSE)
  print(dfb2)
  if (done == 1) {
    cat("\nConverging and Root after", i, "iterations is:", x0s ,"\n")
  } else {
    cat("\nNot Converging and Root after", i, "iterations is:", x0s ,"\n")
  }
}
aitken()
carryon()