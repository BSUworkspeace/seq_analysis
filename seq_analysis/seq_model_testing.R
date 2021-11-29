

sprt<-function(fH0,fH1,alpha,beta){ 
  A  <-(1-beta)/alpha      
  B  <- beta/(1-alpha)   
  f1 <-fH1               
  f0 <-fH0
  cp1<- cumprod(f1)     # If B < R[m] <A then countinue sampling. 
  cp0<- cumprod(f0)     # If R[m] > A then H0 is rejceted by N=m observation.
  m<- length(cp1)       # If R[m] < B then H0 is accepted by N=m observation.
  R<- rep(0,m)
  R<-cp1/cp0
  y<- R
  x<- rep(1:m)
  
  if(R[m]>B & R[m]<A){
    
    cat("\n","    B <  R","[",m,"]",
        "< A    ","\n","",B,"<",R[m],"<",A,"   -->",
        " Countinue sampling.","\n","\n")}
  
  else if(R[m]>A)
    cat("\n","R","[",m,"]","> A    ","\n",R[m],">",A,"      -->",
        " Rejcet H0.Finished","\n","\n")
  
  else cat("\n","B >","R","[",m,"]","\n",B,">",R[m],"       -->",
           " Accept H0.Finished","\n","\n")
  
  if(R[m]<B | R[m]>A) {
    plot(1,R[1],type="o",xlim=c(0,m),ylim=c(0,A+3),xlab="N",
         ylab=expression(R[N]), main=expression('curve '(N,R[N])));
    
    u<-c(0,0,m,m+100);v<-c(0,B,B,0);polygon(u,v,col='green');
    z<-c(A,A+100,A+100,A);polygon(u,z,col='7');
    
    lines(x,y,type="o",lwd=2);
    abline(h=A,col='red');abline(h=B,col='red');
    text(m/2,A+1.5,"Rejcet H0",col='blue',lwd=2);
    text(m/2,B/2,"Accept Ho",col='blue',lwd=2);
  }}

#__________________________________________________ Solve :_______________________

# z=(1,0,0,1,1,1,0,1,1, ... )

#-------------------------------------------------- Step 6:

x=rexp(10000)
sprt(dbinom(x,1,0.3),dbinom(x,1,0.6),0.2,0.2)
for (i in 1:10000){
  print(i)
  sprt(dexp(x,0.3),dexp(x,0.4),0.2,0.2)
}


rm(list=ls())

en<- function(x0,p0,x1,p1,dname0,dname1,alpha,beta){
  a<-   log((1-beta)/alpha)
  b<-   log(beta/(1-alpha))
  zH0<- log(dname1(x0,p1)/dname0(x0,p0))
  zH1<- log(dname1(x1,p1)/dname0(x1,p0))
  if(mean(zH0)==0){
    
    EH0N= -(a*b)/mean((zH0)^2)
    print(EH0N)}
  else {
    EH0N<- (a*alpha +b*(1-beta))/mean(zH0)
    print(EH0N)
  }
  
  if(mean(zH1)==0)
    EH1N<- -(a*b)/mean((zH1)^2)
  else 
    EH1N<- (a*(1-alpha) + b*beta)/mean(zH1)
  cat("\n","The expected sample size for accepting H0 :",EH0N,"\n",
      "The expected sample size for accepting H1 :",EH1N,"\n","\n")
}
x0<- rnorm(10000,0,1)
x1<- rnorm(10000,1,1)

x0<- rexp(100000,3)
x1<- rexp(100000,1)
en(x0,c(3),x1,c(1),dexp,dexp,0.05,0.05)
