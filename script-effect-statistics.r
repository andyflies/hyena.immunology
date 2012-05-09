# First download R from www.r-project.org and read its HELP.
# R has extensive help material.
# Please learn how to use functions in R
# before you start loading the functions below.
# These functions, described in Nakagawa, S. & Cuthill, I.C. 2007. 
# Effect size, confidence interval and statistical significance: 
# a practical guide for biologists. Biological Reviews, 82, 591-605, 
# can be easily modified to make functions suitable for the readers'use.
# See that text for the notations used in the functions below.

# Equations 1 & 2
# E.data = a vector of Experimental data
# C.data = a vector of Control data

d.raw.data<-function(E.data,C.data){  
  d<-(mean(E.data)-mean(C.data))/
  sqrt(((length(E.data)-1)*var(E.data)+
  (length(C.data)-1)*var(C.data))/
  (length(E.data)+length(C.data)-2))
  names(d)<-"effect size d"
  return(d)
}

# Equation 3 

d.t.unpaired<-function(t.val,n1,n2){
  d<-t.val*sqrt((n1+n2)/(n1*n2))
  names(d)<-"effect size d"
  return(d)
}

# Equation 4
# information on r (corelation)may not be always avaiable
# Then, just use Equations 1 & 2
# Also note that when data are not normal, values calculated from
# Equations 1 & 2 and Equation 4 may not match up.

d.t.paired<-function(t.val,n,r){
  d<-t.val*sqrt(2*(1-r)/n)
  names(d)<-"effect size d"
  return(d)
}

# Equation 7 (see also Equations 5 & 6 in Table 2)
# See Table 2 for A, B, C, and D.
# use log() to convert to ln(OR) for calculaton CI etc.

OR<-function(A,B,C,D){
  ABCD<-c(A,B,C,D)
  ABCD<-ifelse(ABCD==0,ABCD+0.5,ABCD)
  OR<-(ABCD[1]*ABCD[4])/(ABCD[2]*ABCD[3])
  names(OR)<-"odds ratio"
  return(OR)
}

# Equation 8
# Use Equation 13 to get
# to convert ln(OR) to OR, use exp()

se.ln.OR<-function(A,B,C,D){
  se<-sqrt((1/A)+(1/B)+(1/C)+(1/D))
  names(se)<-"se for ln(OR)"
  return(se)
}

# Equation 9

table.to.r<-function(A,B,C,D){
  r<-((A*D)-(B*C))/sqrt((A+B)*(C+D)*(A+C)*(B+D))
  names(r)<-"Phi"
  print(r)
}

# Equation 10

partial.d<-function(t.val,df,n1,n2){
  d<-t.val*(n1+n2)/(sqrt(n1*n2)*sqrt(df))
  names(d)<-"effect size d"
  return(d)
}

# Equation 11

partial.r<-function(t.val,df){
  r<-t.val/sqrt((t.val)^2+df)
  names(r)<-"effect size r"
  return(r)
}
                    
# Equation 12
# partial correaltion between data1 and data2 controlling for data3

partial.cor<-function(data1,data2,data3){
  r<-(cor(data1,data2)-cor(data1,data3)*cor(data2,data3))/
  (sqrt((1-cor(data1,data3)^2)*(1-cor(data2,data3)^2)))
  names(r)<-"Partial r"
  return(r)
}

# Equation 13

r.adjusted<-function(R2,n,k){
  r<-sqrt(1-((n-1)*(1-R2))/(n-k-1))
  names(r)<-"Adjusted r"
  return(r)
}

# Equation 14

d.unbiased<-function(d,n1,n2){
  d<-d*(1-(3/(4*(n1+n2-2)-1)))
  names(d)<-"Unbiased d"
  return(d)
}

# Equation 15
# if sample size is small, use CI2() instead of CI1()

CI1<-function(ES,se){
  ci<-c((ES-(1.96)*se),(ES+(1.96)*se))
  names(ci)<-c("95% CI lower","95% CI upper")
  return(ci)
}
  
CI2<-function(ES,se,df){
  ci<-c((ES-qt(0.975,df)*se),(ES+qt(0.975,df)*se))
  names(ci)<-c("95% CI lower","95% CI upper")
  return(ci)
}

# Equation 16
# se calculated by Equations 16 & 17 will be nearly identical
# when smaple size is large
# one function for raw data and the other for d.
# n1 = sample size in Control group
# n2 = sample size in Experimental group

se.d.raw1<-function(E.data,C.data){
  n1<-length(C.data)
  n2<-length(E.data)
  d<-(mean(E.data)-mean(C.data))/
  sqrt(((n2-1)*var(E.data)+(n1-1)*var(C.data))/(n1+n2-2))
  se<-sqrt(((n1+n2-1)/(n1+n2-3))*((4/(n1+n2))*(1+(d^2/8))))
  names(se)<-"se for d"
  return(se)
}

se.d1<-function(d,n1,n2){
  se<-sqrt(((n1+n2-1)/(n1+n2-3))*((4/(n1+n2))*(1+(d^2/8))))
  names(se)<-"se for d"
  return(se)
}

# Equation 17
# for d, use d from Eqation 14

se.d.raw2<-function(E.data,C.data){
  n1<-length(C.data)
  n2<-length(E.data)
  d<-(mean(E.data)-mean(C.data))/
  sqrt(((n2-1)*var(E.data)+(n1-1)*var(C.data))/(n1+n2-2))
  d<-d*(1-(3/(4*(n1+n2-2)-1)))
  se<-sqrt((n1+n2)/(n1*n2)+(d^2)/(2*(n1+n2-2)))
  names(se)<-"se for d"
  return(se)
}

se.d2<-function(d,n1,n2){
  se<-sqrt((n1+n2)/(n1*n2)+(d^2)/(2*(n1+n2-2)))
  names(se)<-"se for d"
  return(se)
}

# Equation 18
# here we use d from Equation 14

se.d.raw.paired<-function(E.data,C.data){
  n<-length(C.data)
  d<-(mean(E.data)-mean(C.data))/
  sqrt(((n-1)*var(E.data)+(n-1)*var(C.data))/(2*n-2))
  d<-d*(1-(3/(8*(n-1)-1)))
  r<-cor(E.data,C.data)
  se<-sqrt((2*(1-r))/n+(d^2)/(2*(n-1)))
  names(se)<-"se for d"
  return(se)
}
  

se.d.paired<-function(d,r,n){
  se<-sqrt((2*(1-r))/n+(d^2)/(2*(n-1)))
  names(se)<-"se for d"
  return(se)
}
  
# Equations 19 and 20
# the functions below calcualte Zr and se
# it is recommanded to use 
# one for raw and another for r and n 

Zr.and.se1<-function(data1,data2){
  r<-cor(data1,data2)
  Zr<-0.5*log((1+r)/(1-r))
  se<-(1/sqrt(length(data1)-3))
  names(Zr)<-"Fisher's z"
  names(se)<-"se for Zr"
  c(Zr,se)
}

Zr.and.se2<-function(r,n){
  Zr<-0.5*log((1+r)/(1-r))
  se<-(1/sqrt(n-3))
  names(Zr)<-"Fisher's z"
  names(se)<-"se for Zr"
  c(Zr,se)
}
              
# Equation 21

Zr.to.r<-function(Zr){
  r<-(exp(2*Zr)-1)/(exp(2*Zr)+1)
  names(r)<-"effect size r"
  return(r)
}

# Equations 22 & 23
# to extend Equations 22, 23 & 24 to GLMM,
# quasipoisson or quasibinomial links should be used to get residual variance
# Also see the text for the case of binomial link

Rpeat<-function(s2b,s2e){#Equation 23
  R<-s2b/(s2b+s2e)
  names(R)<-"Repeatablity (ICC)"
  return(R)
}

d.mix<-function(t.val,ni,no,no1,no2,k,R){
  d<-(t.val*(1+(ni/no)*R)*sqrt(1-R)*(no1+no2))/
  (sqrt(no1*no2)*sqrt(no-k))
  names(d)<-"effect size d"
  return(d)
}

# Equations 24
# R from Equation 23

r.mix<-function(t.val,ni,no,k,R){
  r<-(t.val*(1+(ni/no)*R)*sqrt(1-R))/
  (sqrt(t.val^2*(1+(ni/no)*R)^2*sqrt(1-R)+no-k))
  names(r)<-"effect size r"
  return(r)
}

# Equation 25

d.to.r<-function(d,n1,n2){
  r<-d/sqrt(d^2+(n1+n2)^2/(n1*n2))
  names(r)<-"effect size r"
  return(r)
}

# Equation 26
# use Equation 14 to correct bias

r.to.d<-function(r){
  d<-2*r/sqrt(1-r^2)
  names(r)<-"effect size d"
  return(d)
}

# the end - please email itchyshin@yahoo.co.nz
# if you find some bugs in the code - thanks



                     

    

