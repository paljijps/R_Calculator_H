pdf = function(){
  rm(list = ls())
  
  f1 = function(x){
    2*x
  }
  f2 = function(x,b){
    (1/b)*exp(-x/b)   #exponential distribution
  }
  f3 = function(x,m,s){
    exp((-(x-m)/s)-exp(-(x-m)/s))/s    #extreme value distribution
  }
  f4 = function(x,a,b){
    (exp(-(x-a)/b))/(b*(1+exp(-(x-a)/b))^2) #logistic distribution
  }

  while (TRUE) {
    pdfs = c("2x", "(1/b)*exp^(-x/b))", "exp((-(x-m)/s)-exp(-(x-m)/s))/s", "(exp(-(x-a)/b))/(b*(1+exp(-(x-a)/b))^2)")
    print(data.frame("f(x)" = pdfs,  check.names = FALSE ) )
    j = as.numeric(readline("Enter pdf no. to generate random variable: "))
    if (j == 1) {
      f = f1
      break
    } else if (j == 2) {
      f = f2
      break
    } else if (j == 3) {
      f = f3
      break
    } else if (j == 4) {
      f = f4
      break
    }
  }
  list(f,j)
}
carryon()