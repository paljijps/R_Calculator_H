cat("\014")
harmeeton()
cat("\n","In CENTRAL MOMENTS ","\n")
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

moment_discrete_data= function(){
  rm(list = ls())
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  m2=0
  m3=0
  m4=0
  for(i in 1:length(x)){
    m2=m2+((x[i]-mean(x))^2)
    m3=m3+((x[i]-mean(x))^3)
    m4=m4+((x[i]-mean(x))^4)
  }
  cat("Second order central moment is :",m2/length(x),"\n")
  cat("Third order central moment is :",m3/length(x),"\n")
  cat("Fourth order central moment is :",m4/length(x),"\n")
  
}


moment_un_freq_data= function(){
  rm(list = ls())
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  print(data.frame(x = x, freq = f))
  #cat("x","|","f","\n")
  #showing input data
  #for(i in 1:length(x)){
  # cat(x[i],"|",f[i],"\n")
  #}
  
  n=length(x)
  fx=0
  for(i in 1:n){
    fx=fx+(f[i]*x[i])
  }
  mean=fx/sum(f)
  
  m2=0
  m3=0
  m4=0
  for(i in 1:length(x)){
    m2=m2+f[i]*((x[i]-mean)^2)
    m3=m3+f[i]*((x[i]-mean)^3)
    m4=m4+f[i]*((x[i]-mean)^4)
  }
  cat("Second order central moment is :",m2/sum(f),"\n")
  cat("Third order central moment is :",m3/sum(f),"\n")
  cat("Fourth order central moment is :",m4/sum(f),"\n")
}


moment_grped_freq_data= function(){
  rm(list = ls())
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("\n")
  cat("Enter the lower limits: ")
  l=scan(what = double())
  cat("Enter the upper limits: ")
  u=scan(what = double())
  
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  #cat("low-up","f","\n")
  #cat("----------","\n")
  
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
  m2=0
  m3=0
  m4=0
  for(i in 1:length(x)){
    m2=m2+f[i]*((x[i]-mean)^2)
    m3=m3+f[i]*((x[i]-mean)^3)
    m4=m4+f[i]*((x[i]-mean)^4)
  }
  cat("Second order central moment is :",m2/sum(f),"\n")
  cat("Third order central moment is :",m3/sum(f),"\n")
  cat("Fourth order central moment is :",m4/sum(f),"\n")
  
}

select_h_central_momemt = function(){
  rm(list = ls())
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,moment_discrete_data(),moment_grped_freq_data(),moment_un_freq_data(),break)
  }
}
select_h_central_momemt()
carryon()
