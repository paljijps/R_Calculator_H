trape = function(){
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n ***** In Trapezoidal Rule *****\n")
  source("jaswinder/integrals.R")
  this = integrals()
  g = this[[1]] #function
  a = this[[2]][1] #lower limit
  b = this[[2]][2] #upper limit
  show(g)
  result = (b-a)*(g(a)+g(b))/2
  cat("Required value of integral by Simple Trapezoidal Rule is: ", result)
}
trape()
carryon()