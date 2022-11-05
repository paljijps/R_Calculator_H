base_tarun = function(){
  cat("\014")
  tarunon()
  cat("\n-----------------------------------")
  cat("\nEnter","\t","Calculations","\n")
  cat("-----------------------------------\n")
  del = c(1:7,0)
  base1_names = c("Addition","subtraction","Multiplication","division","integer division","Remainder","Power","Previous")
  for (i in 1:8) {
    cat(del[i],"\t",base1_names[i],"\n")
  }
}

select_tarun = function(){
  while(TRUE){
    base_tarun()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,source("tarun/Addition.R"),source("tarun/Subtraction.R"),source("tarun/Multiply.R"),source("tarun/Division.R"),source("tarun/integer_division.R"),source("tarun/Mod.R"),source("tarun/Power.R"))
    if (i == 0) {
      break
    }
  }
}
select_tarun()