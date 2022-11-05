
simp13 = function(){
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n ***** In Composite Simpson's 1/3 Rule *****\n")
  source("jaswinder/integrals.R")
  this = integrals()
  g = this[[1]] #function
  a = this[[2]][1] #lower limit
  b = this[[2]][2] #upper limit
  show(g)
  result = (b-a)*(g(a)+ 4*g((a+b)/2) + g(b))/6
  cat("Required value of integral by Simple Simpson's 1/3 Rule is: ", result)
}
simp13()
carryon()