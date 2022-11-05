cat("\014")
harmeeton()
cat("\n","In MEAN DEVIATION ")
data_choice= function(){
  cat("\nEnter your choice of data","\n")
  cat("-----------------------------------\n")
  base1_names = c("Discrete","Grouped frquency","Ungrouped frquency","Previous","\n")
  for (i in 1:4) {
    cat(i,"\t",base1_names[i],"\n")
  }
}

##user defined functions:

md_discrete_data= function(){
  cat("Enter the data and press the enter key twice to stop the process: ","\n")
  x=scan(what = double())
  cat("Entered data points are: ","\n") 
  print(x)
  cat("Mean of data is :",mean(x),"\n")
  n=length(x)
  md=0
  for (i in 1:n){
    md=md+abs(x[i]-mean(x))
  }
  cat("Mean deviation about mean is: ",md/n,"\n")
  
}

md_un_freq_data= function(){
  cat("Press the enter key twice to stop the inputting process: ","\n\n")
  cat("Enter the data points: ")
  x=scan(what = double())
  cat("Enter the corresponding frequencies: ")
  f=scan(what = double())
  
  # cat("x","f","\n")
  #showing input data
  # for(i in 1:length(x)){
  #   cat(x[i],"|",f[i],"\n")
  # }
  # 
  print(data.frame(x = x, freq = f))
  
  n=length(x)
  fx=0
  for(i in 1:n){
    fx=fx+(f[i]*x[i])
  }
  mean=fx/sum(f)
  cat("Mean of data is :",mean)
  
  md=0
  for (i in 1:n){
    md=md+f[i]*(abs(x[i]-mean))
  }
  cat("Mean deviation about mean is: ",md/sum(f))

}
  

md_grped_freq_data= function(){
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
  #   cat(l[i],"-",u[i],"|",f[i],"\n")
  # }
  # 
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
  cat("Mean of data is :",mean)
  
  md=0
  for (i in 1:n){
    md=md+f[i]*(abs(x[i]-mean))
  }
  cat("Mean deviation about mean is: ",md/sum(f))
  
}

select_h_md = function(){
  while(TRUE){
    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,md_discrete_data(),md_grped_freq_data(),md_un_freq_data(),break)
  }
}
select_h_md()
carryon()