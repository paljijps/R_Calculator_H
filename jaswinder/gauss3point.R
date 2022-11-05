gauss3pt <- function() {
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n ***** In Gauss Legendre 3 Point Formula *****\n")
  source("jaswinder/integrals.R")
  this = integrals()
  g = this[[1]] #function
  a = this[[2]][1] #lower limit
  b = this[[2]][2] #upper limit
  show(g)
  f = function(t){
    ((b-a)/2)*g(((b-a)*t + (b+a))/2)
  }
  
  res = (5*f(-sqrt(3/5)) + 8*f(0) + 5*f(sqrt(3/5)))/9
  cat("\n Required value of integral using Gauss-Legendre 3-Point Formula is:", res)
}
gauss3pt()
carryon()