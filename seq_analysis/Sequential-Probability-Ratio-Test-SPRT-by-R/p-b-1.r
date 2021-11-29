

rm(list=ls())
en<- function(x0,p0,x1,p1,dname0,dname1,alpha,beta){
a<-   log((1-beta)/alpha)
b<-   log(beta/(1-alpha))
zH0<- log(dname1(x0,p1)/dname0(x0,p0))
zH1<- log(dname1(x1,p1)/dname0(x1,p0))
if(mean(zH0)==0)EH0N<- -(a*b)/mean((zH0)^2)
else EH0N<- (a*alpha +b*(1-beta))/mean(zH0)
if(mean(zH1)==0)EH1N<- -(a*b)/mean((zH1)^2)
else EH1N<- (a*(1-alpha) + b*beta)/mean(zH1)
cat("\n","tedad nemone morede entezar baraye ghabole H0 :",EH0N,"\n",
"tedad nemone morede entezar baraye ghabole H1 :",EH1N,"\n","\n")
}
x0<- rnorm(10000,0,1)
x1<- rnorm(10000,1,1)
en(x0,c(0,1),x1,c(1,1),dnorm,dnorm,0.05,0.05)















