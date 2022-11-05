cat("\014")
harmeeton()
cat("\n","In MODE")

data_choice= function(){
  cat("\nEnter your choice of data","\n")
  cat("-----------------------------------\n")
  base1_names = c("Discrete","Grouped frquency","Ungrouped frquency","Previous","\n")
  for (i in 1:4) {
    cat(i,"\t",base1_names[i],"\n")
  }
}
mode_discrete_data= function(){
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are:", "\n") 
  print(x)
  unique_val=unique(x) #to get the unique values out of the vector
  counts=vector()
  for (i in 1:length(unique_val)){
    counts[i]=length(which(x==unique_val[i]))
  }
  position=c(which(counts==max(counts)))
  if(length(unique_val)==length(x))
    cat("Mode does not exist")
  else
    cat("Mode is: ", mode_x=unique_val[position])

}

mode_un_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  # cat("x","f","\n")
  print(data.frame(x = x, freq = f))
  #showing input data
  # for(i in 1:length(x)){
  #   cat(x[i],f[i],"\n")
  # }
  
  t=which.max(f)
  cat("Mode of data is :",x[t])
}

mode_grped_freq_data= function(){
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
  
#finding lower limit of modal class(with highest freq)  
  t=which.max(f)
  L=l[t] #lower limit 
  f1=f[t]
  f0=f[t-1]
  f2=f[t+1]
  h=u[t]-l[t]
  cat("Mode is:",L+((h*(f1-f0))/(2*f1-f0-f2)))
}

select_h_mode = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,mode_discrete_data(),mode_grped_freq_data(),mode_un_freq_data(),break)
  }
}
select_h_mode()
carryon()