
int_div = function(){ 
  cat("\014")
  tarunon()
  cat("\n In Integer Division \n")
a<-readline(prompt="Enter divisor = ")
a<-as.numeric(a)
b<-readline(prompt="Enter dividend = ")
b<-as.numeric(b)
res<-b%/%a
cat("Quotient = ",res)
}
int_div()
carryon()