
multiply = function(){ 
  cat("\014")
  tarunon()
  cat("\n In Multiplication\n")
n<-readline(prompt="Enter values to be multiplited = ")
num<-scan(text=n,quiet=TRUE,sep=" ",blank.lines.skip=TRUE)
num<-as.numeric(num)
res<-1
for(i in num){
  res<-res*i
}
print(res)
}
multiply()
carryon()