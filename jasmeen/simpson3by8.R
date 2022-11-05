
simp38 = function(){
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n ***** In Simpson's 3/8 Rule ***** \n")
  source("jaswinder/integrals.R")
  this = integrals()
  g = this[[1]] #function
  a = this[[2]][1] #lower limit
  b = this[[2]][2] #upper limit
  show(g)
  result = 3*(b-a)*( g(a)+ 3*g((2*a+b)/3) + 3*g((a+2*b)/3) + g(b) )/24
  cat("Required value of integral by Simple Simpson's 3/8 Rule is: ", result)
}
simp38()
carryon()