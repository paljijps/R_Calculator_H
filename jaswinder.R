base_jaswinder = function(){
  rm(list = ls())
  cat("\014")
  jpson()
  cat("\n-----------------------------------")
  cat("\nEnter","\t","Calculations","\n")
  cat("-----------------------------------\n")
  del = c(1:8,0)
  base1_names = c("Bisection Method","Regula Falsi Method","Secant Method","Newton-Raphson Method","Chebyshev Method","Muller Method","Aitken Delta Square Method","Guass-Legendre 3 point formula","Previous")
  for (i in 1:9) {
    cat(del[i],"\t",base1_names[i],"\n")
  }
}

select_jaswinder = function(){
  rm(list = ls())
  while(TRUE){
    base_jaswinder()
    i = as.numeric(readline("Enter your choice \U0001f600: "))
    switch(i,source("jaswinder/bisect.R"),source("jaswinder/regula.R"),source("jaswinder/secant.R"),source("jaswinder/newton.R"),source("jaswinder/chebyshev.R"),source("jaswinder/muller.R"), source("jaswinder/aitken.R"), source("jaswinder/gauss3point.R"))
    if (i == 0) {
      break
    }
  }
}
select_jaswinder()