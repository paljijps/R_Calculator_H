mid_squ_rn<-function(){
  rm(list=ls())
  cat("\014")
  hitikaon()
  cat("\n ***** In Mid Square Method ***** \n")
  rn<-c()
  seed = as.numeric(readline("Enter SEED: "))
  n = as.numeric(readline("How many numbers to generate: "))
  for(i in 1:n){
    rn<-c(rn,seed)
    x <- seed^2
    x <- x %/% 100
    seed <- x %% 10000
  }
  
  cat("Generated Random Numbers are: \n",rn)
  return(rn)
}
mid_squ_rn()
carryon()
