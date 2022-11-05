
difference = function(){ 
  cat("\014")
  tarunon()
  cat("\n In Subtraction\n")
a<- readline(prompt="Enter first number = ")
a<- as.numeric(a)
b<- readline(prompt="Enter second number = ")
b<- as.numeric(b)
res<- a-b
cat("Difference (First Number - Second Number) = ",res)
}
difference()
carryon()