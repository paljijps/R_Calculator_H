cat("\014")
harmeeton()
cat("\n","In VARIANCE AND STANDARD DEVIATION ","\n")
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

var_discrete_data= function(){
  rm(list = ls())
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  s=0
  for(i in 1:length(x)){
    s=s+((x[i]-mean(x))^2)
  }
  varx = s/length(x)
  cat("Variance of data is :",varx,"\n")
  cat("Standard deviation of data is :",sqrt(varx),"\n")
  cat("Coefficient of variation: ",((sqrt(varx))*100)/(mean(x)))
}


var_un_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  # cat("x","|","f","\n")
  # #showing input data
  # for(i in 1:length(x)){
  #   cat(x[i],"|",f[i],"\n")
  # }
  print(data.frame(x = x, freq = f))
  n=length(x)
  fx=0
  for(i in 1:n){
    fx=fx+(f[i]*x[i])
  }
  mean=fx/sum(f)
  
  s=0
  for(i in 1:length(x)){
    s=s+(f[i]*((x[i]-mean)^2))
  }
  varx = s/sum(f)
  cat("Variance of data is :",varx,"\n")
  cat("Standard deviation of data is :",sqrt(varx),"\n")
  cat("Coefficient of variation: ",((sqrt(varx))*100)/(mean))

}


var_grped_freq_data= function(){
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
  # for(i in 1:length(l)){
  #   cat(l[i],"-",u[i],"|",f[i],"\n")
  # }
  
  #showing input data
  CI = c()
  for(i in 1:length(l)){
    #cat(l[i],"-",u[i],"|",f[i],"\n")
    CI = c(CI, paste(l[i],"-",u[i]) )
  }
  print(data.frame(CI = CI , freq = f))
  
  #calculation of midpoints
  n=length(l)
  fx=0
  x=rep(0,n)
  for(i in 1:n){
    x[i]=(l[i]+u[i])/2
    fx=fx+(f[i]*x[i])
  }
  mean=fx/sum(f)
  
  s=0
  for(i in 1:length(l)){
    s=s+(f[i]*((x[i]-mean)^2))
  }
  varx = s/sum(f)
  cat("Variance of data is :",varx,"\n")
  cat("Standard deviation of data is :",sqrt(varx))
  cat("Coefficient of variation: ",((sqrt(varx))*100)/(mean))

}

select_h_var= function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,var_discrete_data(),var_grped_freq_data(),var_un_freq_data(),break)
  }
}
select_h_var()
carryon()
