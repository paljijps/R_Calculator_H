cat("\014")
harmeeton()
cat("\n","In GEOMETRIC MEAN")
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

gm_discrete_data= function(){
  rm(list = ls())
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  sum=0
  for(i in 1:length(x)){
    sum=sum+log(x[i])
  }
  cat("Geometric mean of data is :",exp(sum/length(x)))
}


gm_un_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  #cat(x,f,"\n")
  #showing input data
  #for(i in 1:length(x)){
   # cat(x[i],f[i],"\n")
  #}
  print(data.frame(x = x, freq = f))
  
  n=length(x)
  fx=0
  for(i in 1:n){
    fx=fx+(f[i]*log(x[i]))
  }
  cat("Geometric mean of data is :",exp((fx/sum(f))))
}


gm_grped_freq_data= function(){
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
    fx=fx+(f[i]*log(x[i]))
  }
  cat("Geometric mean of data is :",exp((fx/sum(f))))
}

select_h_gm = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,gm_discrete_data(),gm_grped_freq_data(),gm_un_freq_data(),break)
  }
}
select_h_gm()
carryon()