
base_jasmeen = function(){
  rm(list = ls())
  cat("\014")
  jasmeenon()
  cat("\n-----------------------------------")
  cat("\nEnter","\t","Calculations","\n")
  cat("-----------------------------------\n")
  del = c(1:5,0)
  base1_names = c("Trapezoidal Rule","Simpson's 1/3 Rule","Simpson's 3/8 Rule","Composite Trapezoidal Rule","Composite Simpson's Rule","Previous")
  for (i in 1:6) {
    cat(del[i],"\t",base1_names[i],"\n")
  }
}

select_jasmeen = function(){
  rm(list = ls())
  while(TRUE){
    base_jasmeen()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,source("jasmeen/trapezoidal.R"),source("jasmeen/simpson1by3.R"),source("jasmeen/simpson3by8.R"),source("jasmeen/comptrape.R"),source("jasmeen/compsimp.R"))
    if (i == 0) {
      break
    }
  }
}
select_jasmeen()