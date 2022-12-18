bin_to_dec = function(x){
  x = as.numeric(as.logical(x))
  n = length(x) - 1
  dec = c()
  for (i in n:0) {
    dec = c(dec, 2^i)
  }
  as.vector(t(x)%*%dec)
}

shift_gen_jps = function(){
  rm(list = ls())
  cat("\014")
  jpson2()
  cat("\n ****** In Shift Generator ****** \n")
  rand = c()
  binvecs = list()
  m = as.numeric(readline("How many random numbers to generate: "))
  n = as.numeric(readline("Enter No. of bits of binary number: "))
  cat("\n Enter n bit binary vector (each bit separated by a space): ")
  x1 = scan(nmax = n)
  
  for (j in 1:m) {
    cat("\n ***** Step 1 *****")
    cat("\n Binary SEED is: ",x1,"\n Its decimal equivalent is: ",bin_to_dec(x1))
    
    cat("\n\n ***** Step 2 *****")
    cat("\n Shifting SEED to right by 2 bits.")
    x2 = c(0,0,x1)
    x2 = x2[1:n]
    cat("\n",x2,"\n Its decimal equivalent is: ", bin_to_dec(x2) )
    
    cat("\n\n ***** Step 3 *****")
    cat("\n Pefroming Exclusive OR of Step 1 and Step 2: ")
    x3 = as.numeric(xor(x1,x2))
    cat("\n",x3,"\n Its decimal equivalent is: ", bin_to_dec(x3))
    
    cat("\n\n ***** Step 4 *****")
    cat("\n Shifting to left by 3 bits: ")
    x4 = c(x3,0,0,0)
    x4 = x4[4:(n+3)]
    cat("\n",x4, "\n Its decimal equivalent is: ", bin_to_dec(x4))  
    
    cat("\n\n ***** Step 5 *****")
    cat("\n Pefroming Exclusive OR of Step 3 and Step 4: ")
    x5 = as.numeric(xor(x3,x4))
    cat("\n",x5,"\n Its decimal equivalent is: ", bin_to_dec(x5))
    rand = c(rand, bin_to_dec(x5))
    binvecs[[j]] = x5
    x1 = x5
    cat("\n\n Again...........")
  }
  cat("\n\n\n ****** Result ******\n ------------------------ \n Required random numbers using Shift generator are: \n", rand)
  maxrand = bin_to_dec(rep(1,n))
  cat("\n\n Required random variates from U(0,1) are: \n", rand/maxrand)
  cat("\n\n Corresponding binary random vectors were: \n")
  show(binvecs)
}
shift_gen_jps()
carryon()

