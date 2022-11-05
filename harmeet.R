base_harmeet_kaur = function(){
  cat("\014")
  harmeeton()
  cat("\n-----------------------------------")
  cat("\nVarious functions avaliable","\n")
  cat("-----------------------------------\n")
  del = c(1:12,0)
  base1_names = c("Mean","Median","Mode","Geometric-mean","Harmonic-mean","Quartiles and Quartile deviation","Mean deviation about mean","Variance and Standard deviation","Central-moments","Skewness-Kurtosis","Correlation-Regression","Distribution Tables","Previous")
  for (i in 1:13) {
    cat(del[i],"\t",base1_names[i],"\n")
  }
}

select_harmeet_kaur = function(){
  while(TRUE){
    base_harmeet_kaur()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,source("harmeet/mean.R"),source("harmeet/median.R"),source("harmeet/mode.R"),source("harmeet/gm.R"),source("harmeet/hm.R"),source("harmeet/quartile.R"),source("harmeet/meandev.R"),source("harmeet/var_sdev.R"),source("harmeet/cental_mom.R"),source("harmeet/skewness.R"),source("harmeet/cor_reg.R"), source("harmeet/tables.R"))
    if (i == 0) {
      break
    }
  }
}
select_harmeet_kaur()