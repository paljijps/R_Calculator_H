
cat("\014")
harmeeton()
cat("\n","In QUARTILE")
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

q_discrete_data= function(){
  rm(list = ls())
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  n=length(x)
  q1=quantile(x,0.25)
  q3=quantile(x,0.75)
  cat("First quartile Q1 is :",q1,"\n")
  cat("Third quartile Q3 is :",q3,"\n")
  cat("Inter quartile range is :",q3-q1,"\n")
  cat("Quartile Deviation is :",(q3-q1)/2)
}

q_un_freq_data= function(){
  rm(list = ls())
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
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
  #showing input data
  # for(i in 1:length(x)){
  #   cat(x[i],"|",f[i],"|",cf[i],"\n")
  # }
  print(data.frame(x = x, freq = f, "cumm_freq" = cf, check.names = FALSE))
  
  N=sum(f)
  for (i in 1:length(x)){
    if(cf[i]>(N/4)){
      t=i
      break
    }
  }
  cat("First quartile is :",x[t],"\n")
  for (i in 1:length(x)){
    if(cf[i]>(3*N/4)){
      T=i
      break
    }
  }
  cat("Third quartile is :",x[T],"\n")
  cat("Inter quartile range is: ",x[T]-x[t],"\n")
  cat("Quartile Deviation is :",(x[T]-x[t])/2)
}

q_grped_freq_data= function(){
  rm(list = ls())
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
  
  # cat("low-up","|","f","|","cf","\n")
  # cat("--------------------------------------","\n")
  # 
  #showing input data
  CI = c()
  for(i in 1:length(l)){
    #cat(l[i],"-",u[i],"|",f[i],"\n")
    CI = c(CI, paste(l[i],"-",u[i]) )
  }
  print(data.frame(CI = CI , freq = f, "cumm_freq" = cf, check.names = FALSE))
  
  # for(i in 1:length(l)){
  #   cat(l[i],"-",u[i],"|",f[i],"|",cf[i],"\n")
  # }
  N=sum(f)
  for (i in 1:length(l)){
    if(cf[i]>(N/4)){
      T=i
      break
    }
  }
  N=sum(f)
  L=l[T]
  h=u[T]-l[T]
  F=f[T]
  C=cf[T-1]
  cat("First quartile is :",L+ (h*((N/4)-C))/F,"\n")
  
  for (i in 1:length(l)){
    if(cf[i]>(3*N/4)){
      T=i
      break
    }
  }
  N=sum(f)
  L=l[T]
  h=u[T]-l[T]
  F=f[T]
  C=cf[T-1]
  cat("\n","Third quartile is :",L+ (h*((3*N/4)-C))/F)
  cat("\nInter quartile range is: ",(L+ (h*((3*N/4)-C)))-(L+ (h*((N/4)-C))/F),"\n")
  cat("Quartile Deviation is :",((L+ (h*((3*N/4)-C)))-(L+ (h*((N/4)-C))/F))/2)
  
   
  
}

select_h_q = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,q_discrete_data(),q_grped_freq_data(),q_un_freq_data(),break)
  }
}
select_h_q()
carryon()