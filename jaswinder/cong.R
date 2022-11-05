gcd = function(x,y){
  while(y){
    temp = y
    y = x %% y
    x = temp
  }
  return(x)
}

isPrime = function(n){
  flag = TRUE
  if (n == 1) {
    flag = FALSE
  } else if (n == 2 || n == 3) {
    flag = TRUE
  } else if( n > 3 ) {
    for (i in 2:(n-1)) {
      if (n%%i == 0) {
        flag = FALSE
      }
    }
  }
  return(flag)
}

primefactors = function(x){
  facts = c()
  for (i in 1:x) {
    if (x %% i == 0) {
      if (isPrime(i)) {
        facts = c(facts,i)
      }
    }
  }
  return(facts)
}

isKnuth = function(a,c,x1,m){
  
  cond1 = function(c,m){
    if (gcd(c,m) == 1) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } 
  
  
  cond2 = function(a,m){
    pf = primefactors(m)
    flag = TRUE
    for (g in pf) {
      if ((a-1)%%g != 0) {
        flag = FALSE
        break
      }
    }
    return(flag)
  }
  
  cond3 = function(a,m){
    if (m %% 4 == 0) {
      flag = FALSE
      if ((a-1) %% 4 == 0) {
        flag = TRUE
      }
    } else {
      flag = TRUE
    }
    return(flag)
  }
  
  cat("\n ***** Knuth Conditions ***** \n\n")
  cnd = c("Condition 1", "Condition 2", "Condition 3")
  df = data.frame("Condition" = cnd, "Validation" = c(cond1(c,m), cond2(a,m), cond3(a,m) ) , check.names = FALSE)
  print(df)
}

cong = function(){
  rm(list = ls())
  cat("\014")
  jpson2()
  cat("\n ***** In Congruential Generator ***** \n")
  
  while(TRUE){
    a = as.numeric(readline("Enter a: "))
    c = as.numeric(readline("Enter c: "))
    x1 = as.numeric(readline("Enter x1: "))
    m = as.numeric(readline("Enter m: "))
    if (a > 0 & c > 0 & x1 > 0 & m > 0 ) {
      break
    } else {
      cat("\nEnter +ve values only\n")
    }
  }
  a = round(a)
  c = round(c)
  x1 = round(x1)
  m = round(m)
  
  x = c()
  x[1] = x1
  for (i in 1:m) {
    x[i+1] = (a*x[i] + c)%% m
  }
  isKnuth(a,c,x1,m)
  cat("\nGenerated Random Numbers are: \n")
  cat(x)
}
cong()
carryon()