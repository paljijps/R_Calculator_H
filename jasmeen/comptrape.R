comptrapez <- function() {
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n ***** In Composite Trapezoidal Rule *****\n")
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
  
  res = h*(f[1] + 2*(sum(f[2:n])) + f[n+1])/2
  cat("\n Required value of integral using Composite Trapezoidal Rule is: ",res)
}
comptrapez()
carryon()