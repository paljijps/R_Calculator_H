cat("\014")
harmeeton()
cat("\n","In SKEWNESS AND KURTOSIS","\n")

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

s_k_discrete_data= function(){
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
  m2 = m2/length(x)
  m3 = m3/length(x)
  m4 = m4/length(x)
  beta=((m3)^2)/((m2)^3)
  sk=sqrt(beta)
  if(sk>0){
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is positively skewed","\n")
  }else if(sk==0){
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is symmetric","\n")
  }else{
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is negatively skewed","\n")
  }
  
  g=(m4)/((m2)^2)
  kurt=( g - 3)
  if(kurt>3){
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is leptokurtic","\n")
  }else if(kurt==3){
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is mesokurtic","\n")
  }else{
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is platykurtic","\n")
  }
}


s_k_un_freq_data= function(){
  rm(list = ls())
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
  
  m2=0
  m3=0
  m4=0
  for(i in 1:length(x)){
    m2=m2+f[i]*((x[i]-mean)^2)
    m3=m3+f[i]*((x[i]-mean)^3)
    m4=m4+f[i]*((x[i]-mean)^4)
  }
  m2 = m2/sum(f)
  m3 = m3/sum(f)
  m4 = m4/sum(f)
  
  beta=((m3)^2)/((m2)^3)
  sk=sqrt(beta)
  if(sk>0){
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is positively skewed","\n")
  }else if(sk==0){
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is symmetric","\n")
  }else{
    cat("Moment coefficient of skewness is : ",sk,"Hence the given data is negatively skewed","\n")
  }
  g=(m4)/((m2)^2)
  kurt=(g - 3) 
  
  if(kurt>3){
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is leptokurtic","\n")
  }else if(kurt==3){
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is mesokurtic","\n")
  }else{
    cat("Moment coefficient of kurtosis is : ",kurt,"Hence the given data is platykurtic","\n")
  }
}



s_k_grped_freq_data= function(){
  rm(list = ls())
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
  m2 = m2/sum(f)
  m3 = m3/sum(f)
  m4 = m4/sum(f)
  beta=((m3)^2)/((m2)^3)
  sk=sqrt(beta)
  if(sk>0){
    cat("Moment coefficient of skewness is : ",sk," Hence the given data is positively skewed","\n")
  }else if(sk==0){
    cat("Moment coefficient of skewness is : ",sk," Hence the given data is symmetric","\n")
  }else{
    cat("Moment coefficient of skewness is : ",sk," Hence the given data is negatively skewed","\n")
  }
  g=(m4)/((m2)^2)
  kurt=( g - 3)
  if(kurt>0){
    cat("Moment coefficient of kurtosis is : ",kurt," Hence the given data is leptokurtic","\n")
  }else if(kurt==0){
    cat("Moment coefficient of kurtosis is : ",kurt," Hence the given data is mesokurtic","\n")
  }else{
    cat("Moment coefficient of kurtosis is : ",kurt," Hence the given data is platykurtic","\n")
  }
}

select_h= function(){
  while(TRUE){

    data_choice()
    i = as.numeric(readline("Enter your choice: "))
    switch(i,s_k_discrete_data(),s_k_grped_freq_data(),s_k_un_freq_data(),break)
  }
}
select_h()
carryon()