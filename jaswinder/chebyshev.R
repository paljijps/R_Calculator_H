chebyshev = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ****** In Chebyshev Method ****** \n")
  done = 0
  source("jaswinder/fun.R")
  funcs <- eq_select()
  f <- funcs[[1]]
  show(f)
  fd <- funcs[[2]]
  fdd <- funcs[[3]]
  #phi <- funcs[[4]]
  intervals(f)
  
  x0 <- as.numeric(readline("Enter one initial approximation: "))
  e <- as.numeric(readline("Enter error tolerance: "))
  maxitr <- 30
  x <- c()
  k <- c()
  x0k <- c()
  i <- 1
  while (i < maxitr) {
    x1 <- x0 - f(x0)/fd(x0) - ((f(x0))^2 * fdd(x0))/(2*(fd(x0))^3)
    x0k <- c(x0k, x0)
    k <- c(k, i)
    x = c(x,x1)
    x0 = x1
    if (abs(f(x1)) < e) {
      done = 1
      break()    
    }
    i <- i + 1

  }
  
  dfb2 = data.frame("x[i]" = x0k, "x[i+1]" = x, "f(x[i+1])" = f(x), check.names = FALSE)
  print(dfb2)
  if (done == 1) {
    cat("\nConverging and Root after", i, "iterations is:", x[i],"\n")
  } else {
    cat("\nNot Converging and Root after", i, "iterations is:", x[i],"\n")
  }
}
chebyshev()
carryon()