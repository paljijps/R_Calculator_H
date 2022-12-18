rm(list = ls())

base1 = function(){
  cat("\014")
  cat("\n ******* \U0001f600 Calculator Project \U0001f600 *******\n\n")
  cat("-----------------------------------")
  cat("\nEnter","\t","Calculations","\n")
  cat("-----------------------------------\n")
base1_names = c("Mathematical Calculations ","Statistical Calculations ","Numerical Methods - 1 ","Numerical Methods - 2 ","Simulation ","Exit")
del = c(1:5,0)  
for (i in 1:6) {
  cat(del[i],"\t",base1_names[i],"\n")
}
}

select = function(){
  while(TRUE){
    base1()
    i = as.numeric(readline("Enter your choice \U0001f64b: "))
    switch(i,source("tarun.R"),source("harmeet.R"),source("jaswinder.R"),source("jasmeen.R"),source("jaswinder_hitika.R"))
    if (i == 0) {
      break
    }
  }
}


carryon = function(){
  cat("\n")
  ch = readline("Press Enter to continue...")
}
jpson = function(){
  cat("\n","3. In Numerical Methods - 1: \n ****** \U0001f607 JASWINDERPAL SINGH \U0001f607 ****** \n")
}
jpson2 = function(){
  cat("\n","5. In Statistical Simulation: \n ****** \U0001f607 JASWINDERPAL SINGH \U0001f607 ****** \n")
}
tarunon = function(){
  cat("\n","1. In Mathematical Calculations : \n ****** TARUN ****** \n")
}
harmeeton = function(){
  cat("\n","2. In Statistical Calculations: \n ****** HARMEET KAUR ****** \n")
}
jasmeenon = function(){
  cat("\n","4. In Numerical Methods - 2: \n ****** JASMEEN SHARMA ****** \n")
}
hitikaon = function(){
  cat("\n","5. In Statistical Simulation: \n ****** HITIKA ****** \n")
}