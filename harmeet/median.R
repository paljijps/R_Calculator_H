cat("\014")
harmeeton()
cat("\n","In MEDIAN")

data_choice= function(){
  rm(list = ls())
  cat("\nEnter your choice of data","\n")
  cat("-----------------------------------\n")
  base1_names = c("Discrete","Grouped frquency","Ungrouped frquency","Previous","\n")
  for (i in 1:4) {
    cat(i,"\t",base1_names[i],"\n")
  }
}


##user defined functions:

median_discrete_data= function(){
  rm(list = ls())
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  cat("Median of data is :",median(x))
}

median_un_freq_data= function(){
  rm(list = ls())
  cat("Press the enter key twice to stop the inputting process: ","\n")
  
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  #calculation of cumulative frequency
  n=length(x)
  cf=rep(0,n)
  j=1
  cf[j]=f[j]
  for(i in 2:length(f)){
    cf[i]=cf[i-1]+f[i]
  }
  
  # cat("x","f","cf","\n")
  # 
  # for(i in 1:n){
  #   cat(x[i],f[i],cf[i],"\n")
  # }
  print(data.frame(x = x, freq = f, cumm_freq = cf, check.names = FALSE))
  N=sum(f)
  for (i in 1:n){
    if(cf[i]>(N/2)){
      t=i
      break
    }
  }

  cat("Median of data is :",x[t])
}

median_grped_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("\n")
  cat("Enter the lower limits: ")
  l=scan(what = double())
  cat("Enter the upper limits: ")
  u=scan(what = double())
  
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  #calculation of cumulative frequency
  n=length(l)
  cf=rep(0,n)
  j=1
  cf[j]=f[j]
  for(i in 2:length(f)){
    cf[i]=cf[i-1]+f[i]
  }
  
  # cat("low-up","f","cf","\n")
  # cat("----------","\n")
  # 
  # #showing input data
  # for(i in 1:length(l)){
  #   cat(l[i],"-",u[i],f[i],cf[i],"\n")
  # }
  CI = c()
  for(i in 1:length(l)){
    #cat(l[i],"-",u[i],"|",f[i],"\n")
    CI = c(CI, paste(l[i],"-",u[i]) )
  }
  print(data.frame(CI = CI , freq = f, "cumm_freq" = cf, check.names = FALSE))
  
  N=sum(f)
  for (i in 1:n){
    if(cf[i]>(N/2)){
      t=i
      break
    }
  }
  L=l[t] #lower limit of median class
  F=f[t]
  C=cf[t-1]
  h=u[t]-l[t]
  
  median= L+(h*((N/2)-C))/F
  cat("Median of data is :",median)
}


select_h_median = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,median_discrete_data(),median_grped_freq_data(),median_un_freq_data(),break)
  }
}
select_h_median()
carryon()