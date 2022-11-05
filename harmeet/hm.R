cat("\014")
harmeeton()
cat("\n","In HARMONIC MEAN")
data_choice= function(){
  cat("\nEnter your choice of data","\n")
  cat("-----------------------------------\n")
  base1_names = c("Discrete","Grouped frquency","Ungrouped frquency","Previous","\n")
  for (i in 1:4) {
    cat(i,"\t",base1_names[i],"\n")
  }
}

##user defined functions:

hm_discrete_data= function(){
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  sum=0
  for(i in 1:length(x)){
    sum=sum+(1/x[i])
  }
  n=length(x)
  SUM=sum/n
  cat("Harmonic mean of data is :",(1/SUM))
}


hm_un_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  # cat("x","f","f/x","\n")
  # #showing input data
  # for(i in 1:length(x)){
  #   cat(x[i],f[i],f[i]/x[i],"\n")
  # }
  print(data.frame(x = x, freq = f, "f/x" = f/x, check.names = FALSE))
  
  sum=0
  for(i in 1:length(x)){
    sum=sum+(f[i]/x[i])
  }
  n=sum(f)
  SUM=sum/n
  cat("Harmonic mean of data is :",(1/SUM))
}


hm_grped_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("\n")
  cat("Enter the lower limits: ")
  l=scan(what = double())
  cat("Enter the upper limits: ")
  u=scan(what = double())
  
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  # cat("low-up","f","\n")
  # cat("----------","\n")
  # 
  # #showing input data
  # for(i in 1:length(l)){
  #   cat(l[i],"-",u[i],f[i],"\n")
  # }
  #showing input data
  CI = c()
  for(i in 1:length(l)){
    #cat(l[i],"-",u[i],"|",f[i],"\n")
    CI = c(CI, paste(l[i],"-",u[i]) )
  }
  print(data.frame(CI = CI , freq = f))
  
  n=length(l)
  fx=0
  x=rep(0,n)
  for(i in 1:n){
    x[i]=(l[i]+u[i])/2
    fx=fx+(f[i]*x[i])
  }
  
  
  sum=0
  for(i in 1:length(x)){
    sum=sum+(f[i]/x[i])
  }
  n=sum(f)
  SUM=sum/n
  cat("Harmonic mean of data is :",(1/SUM))
  
}

select_h_hm = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,hm_discrete_data(),hm_grped_freq_data(),hm_un_freq_data(),break)
  }
}
select_h_hm()
carryon()