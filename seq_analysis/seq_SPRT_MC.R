SPRT = function(H0,H1,alpha0,beta0){
  alpha=alpha0
  beta=beta0
  Cplus=(1-beta)/alpha   
  Cminus= beta/(1-alpha)
  cp1<- cumprod(H1)     
  cp0<- cumprod(H0)     
  m<- length(cp1)
  
  
  L<-cp1/cp0
  if(L[m]>Cminus & L[m]<Cplus){return(2)}
  else if(L[m]>Cplus){return(1)}
  else{return(0)}
}
alpha=0
beta =0
tzero=0
for (i in 1:10000){
  
  result = SPRT(dexp(rexp(1),0.3),dexp(rexp(1),0.4),0.2,0.2)
  if (result==1){
    alpha=alpha+1
  }
  else if (result == 0){
    beta = beta +1}
  
}
real_alpha=alpha/10000
real_beta = beta/10000
cat("real alpha=",real_alpha,"real beta=",real_beta)
###
# real alpha= 0.0121 real beta= 0.0089


en<- function(x0,p0,x1,p1,dname0,dname1,alpha0,beta0){
  a<-   log((1-beta0)/alpha0)
  b<-   log(beta0/(1-alpha0))
  zH0<- log(dname1(x0,p1)/dname0(x0,p0))
  zH1<- log(dname1(x1,p1)/dname0(x1,p0))
  if(mean(zH0)==0){
    
    EH0N= -(a*b)/mean((zH0)^2)
    print(EH0N)}
  else {
    EH0N<- (a*alpha0 +b*(1-beta0))/mean(zH0)
    print(EH0N)
  }
  
  if(mean(zH1)==0)
    EH1N<- -(a*b)/mean((zH1)^2)
  else 
    EH1N<- (a*(1-alpha0) + b*beta0)/mean(zH1)
  cat("The expected sample size for accepting H0 :",EH0N,"\n",
      "The expected sample size for accepting H1 :",EH1N,)
}

x0<- rexp(100000,3)
x1<- rexp(100000,1)
en(x0,c(3),x1,c(1),dexp,dexp,0.05,0.05)
####
# '''
# [1] 6.108954
# 
# The expected sample size for accepting H0 : 6.108954 
# The expected sample size for accepting H1 : 2.920147 
# '''
####