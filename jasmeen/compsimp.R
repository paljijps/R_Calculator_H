compsimpson <- function() {
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n ***** In Composite Simpson's Rule *****\n") # 1/3 Composite Simpson
  source("jaswinder/integrals.R")
  this = integrals()
  g = this[[1]] #function
  a = this[[2]][1] #lower limit
  b = this[[2]][2] #upper limit
  show(g)
  
  n = as.numeric(readline("Enter number of sub-intervals: "))
  if (n < 2) {
    n = 2
    cat("\nMinimum number of sub-intervals = 2, taken 2\n")
  }
  h=(b-a)/n
  x=c()
  x[1] = a
  for (i in 2:n ) {
    x[i] = x[i-1] + h
  }
  x[n+1] = b
  f = c()
  for (i in 1:(n+1)) {
    f[i] = g(x[i])
  }
  cat("h = ",h,"\n\n")
  df = data.frame(x = x, "f(x)" = f, check.names = FALSE)
  print(df)
  if (n > 2) {
    res = h*(f[1] + 4*( sum(f[seq(2,n,2)]) ) + 2*( sum(f[seq(3, n, 2)]) ) + f[n+1])/3
  } else if(n == 2) {
    res = h*( f[1] + 4*f[2] + f[n+1] )/3
  }

  cat("\n Required value of integral using Composite Simpson's Rule is: ", res)
}
compsimpson()
carryon()