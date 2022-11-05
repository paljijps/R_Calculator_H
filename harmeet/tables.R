#standard normal table
cat("\n***** Under Improvement ****\n")
cat("\n Normal Table\n")
t=seq(0,0.349,by=0.001)
p=pnorm(t)
T_=matrix(p,ncol=10,byrow = TRUE)

colnames(T_)=c("0","1","2","3","4","5","6","7","8","9")
rownames(T_)=c("0.00","0.01","0.02","0.03","0.04","0.05","0.06","0.07","0.08","0.09","0.10","0.11","0.12","0.13","0.14","0.15","0.16","0.17","0.18","0.19","0.20","0.21","0.22","0.23","0.24","0.25","0.26","0.27","0.28","0.29","0.30","0.31","0.32","0.33","0.34")
show(T_)

cat("\nt distribution table\n")
#t distribution table
a=qt(0.25,1:100,lower.tail = FALSE)
a
A=matrix(a,ncol=1,byrow=FALSE)
b=qt(0.20,1:100,lower.tail = FALSE)
b
B=matrix(b,ncol=1,byrow=FALSE)
c=qt(0.15,1:100,lower.tail = FALSE)
c
C=matrix(c,ncol=1,byrow=FALSE)
d=qt(0.1,1:100,lower.tail = FALSE)
d
D=matrix(d,ncol=1,byrow=FALSE)
e=qt(0.05,1:100,lower.tail = FALSE)

E=matrix(e,ncol=1,byrow=FALSE)
f=qt(0.025,1:100,lower.tail = FALSE)
f
G=matrix(f,ncol=1,byrow=FALSE)
M=cbind(A,B,C,D,E,G)

colnames(M)=c("t(0.75)","t(0.80)","t(0.85)","t(0.90)","t(0.95)","t(0.975)")
show(M)
carryon()