integrals = function(){
  rm(list = ls())
  
  g1 = function(x){
    1/(1+x) # 0 to 1
  }
  g2 = function(x){
    1/(5 + 3*x) # 0 to 2
  }
  g3 = function(x){
    1/(1 + x^3) # 1 to 2
  }
  g4 = function(x){
    1/(x^2 + 2*x + 10) # 0 to 2
  }
  
  while (TRUE) {
    intgs = c("1/(1+x)", "1/(5 + 3*x)", "1/(1 + x^3)", "  1/(x^2 + 2*x + 10)")
    from = c(0,0,1,0)
    to = c(1,2,2,2)
    print(data.frame(from = from, to = to, "f(x)" = intgs,  check.names = FALSE ) )
    j = as.numeric(readline("Enter integral to solve: "))
    if (j == 1) {
      g = g1
      a = 0
      b = 1
      break
    } else if (j == 2) {
      g = g2
      a = 0
      b = 2
      break
    } else if (j == 3) {
      g = g3
      a = 0
      b = 1
      break
    } else if (j == 4) {
      g = g4
      a = 0
      b = 2
      break
    }
  }
  list(g, c(a,b))
}

