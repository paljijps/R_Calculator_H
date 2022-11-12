base_hitika = function(){
  rm(list = ls())
  cat("\014")
  hitikaon()
  cat("\n-----------------------------------")
  cat("\nEnter","\t","Calculations","\n")
  cat("-----------------------------------\n")
  del = c(1:4,0)
  base1_names = c("Buffen Needle Problem","Mid-Square Method","Congruential Generator","ITFM","Previous")
  for (i in 1:5) {
    cat(del[i],"\t",base1_names[i],"\n")
  }
  cat("\n Be Patient, Calculations may take a while to complete.\n")
}

select_hitika = function(){
  rm(list = ls())
  while(TRUE){
    base_hitika()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,source("hitika/hitbuffen.R"), source("hitika/midsq.R"), source("jaswinder/cong.R"),source("hitika/itfm.R"))
    if (i == 0) {
      break
    }
  }
}
select_hitika()