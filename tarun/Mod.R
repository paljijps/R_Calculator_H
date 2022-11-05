
remainder = function(){ 
  cat("\014")
  tarunon()
  cat("\n In Remainder \n")
a<-readline(prompt="Enter divisor = ")
a<-as.numeric(a)
b<-readline(prompt="Enter dividend = ")
b<-as.numeric(b)
res<-b%%a
cat("Remainder = ",res)
}
remainder()
carryon()