division = function(){  
  rm(list = ls())
  cat("\014")
  tarunon()
  cat("\n In Division\n")
  a<-readline(prompt="Enter divisor = ")
  a<-as.numeric(a)
  b<-readline(prompt="Enter dividend = ")
  b<-as.numeric(b)
  res<-b/a
  cat("Result = ",res)
}
division()
carryon()