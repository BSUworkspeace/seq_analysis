rm(list=ls())

# dname0 : Harfe "d" ra pish az ebarate ekhte sarie name tozie tahte H0 gharar midahim.
# p0 ::::: bordare parametrha tozie tahte H0.
# x0     : bordare dade haye shabih sazzi shode az tozie tahte H0.
# dname1 : Harfe "d" ra pish az ebarate ekhte sarie name tozie tahte H1 gharar midahim.
# p1 ::::: bordare parametrha tozie tahte H1.
# x1     : bordare dade haye shabih sazzi shode az tozie tahte H1.
# alpha::: ehtemale khataye noe avval.
# beta   : ehtemale khataye noe dovvom.


en.sprt<- function(dname0,p0,x0,dname1,p1,x1,alpha,beta){
a<-   log((1-beta)/alpha)
b<-   log(beta/(1-alpha))
if(length(p0)==3){
zH0<- log(dname1(x0,p1[1],p1[2],p1[3])/dname0(x0,p0[1],p0[2],p0[3]));
zH1<- log(dname1(x1,p1[1],p1[2],p1[3])/dname0(x1,p0[1],p0[2],p0[3]))}

else if(length(p0)==2){
zH0<- log(dname1(x0,p1[1],p1[2])/dname0(x0,p0[1],p0[2]));
zH1<- log(dname1(x1,p1[1],p1[2])/dname0(x1,p0[1],p0[2]))}

else if(length(p0)==1){
zH0<- log(dname1(x0,p1[1])/dname0(x0,p0[1]));
zH1<- log(dname1(x1,p1[1])/dname0(x1,p0[1]))}

if(mean(zH0)==0)EH0N<- -(a*b)/mean((zH0)^2)
else EH0N<- (a*alpha +b*(1-alpha))/mean(zH0)
if(mean(zH1)==0)EH1N<- -(a*b)/mean((zH1)^2)
else EH1N<- (a*(1-beta) + b*beta)/mean(zH1)
cat("\n","tedad nemone morede entezar baraye ghabole H0 :",EH0N,"\n",
"tedad nemone morede entezar baraye ghabole H1 :",EH1N,"\n","\n")
}


x0<- rnorm(10000,100,10)
x1<- rnorm(10000,105,10)
en.sprt(dnorm,c(100,10),x0,dnorm,c(105,10),x1,0.01,0.05)