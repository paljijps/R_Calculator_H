bisection = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ****** In Bisection Method ****** \n")
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
    a <- as.numeric(readline("Enter first initial approximation: "))
    b <- as.numeric(readline("Enter second initial approximation: "))
    if (f(a) * f(b) < 0) {
      break()
    } 
  }
  e <- as.numeric(readline("Enter error tolerance: "))
  maxitr <- 30
  x <- c()
  k <- c()
  ak <- c()
  bk <- c()
  i <- 1
  while (i < maxitr) {
    x1 <- (a + b) / 2
    x = c(x,x1)
    ak <- c(ak, a)
    bk <- c(bk, b)
    k <- c(k, i)
    if (f(a) * f(x1) < 0) {
      b <- x1
    } else {
      a <- x1
    }
    if (abs(f(x1)) < e) {
      done = 1
      break()    
    }
    i <- i + 1
  }
  
  dfb2 = data.frame("a[i]" = ak, "b[i]" = bk, "m[i+1]" = x, "f(m[i+1])" = f(x), check.names = FALSE)
  print(dfb2)
  if (done == 1) {
    cat("\nConverging and Root after", i, "iterations is:", x1,"\n")
  } else {
    cat("\nNot Converging and Root after", i, "iterations is:", x1,"\n")
  }
}
bisection()
carryon()