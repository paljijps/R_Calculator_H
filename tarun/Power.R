
pow = function(){
  cat("\014")
  tarunon()
  cat("\n In Power \n")
n<-readline(prompt="Enter number = ")
n<-as.numeric(n)
pow<-readline(prompt="Enter power = ")
pow<-as.numeric(pow)
res<-n^pow
print(res)
}
pow()
carryon()