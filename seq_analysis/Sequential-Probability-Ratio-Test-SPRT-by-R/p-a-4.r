# B.Falahi Fard 881537
 
#                     sprt : Sequential Probability Ratio Test
 
# Basic concepts of mathematical statistics by: Dr.A.Parsian second edition page:436
# If 'xm' is observed sample in step m:
# fH0  : f( x1,x2,...,xm | H0 )
# fH1  : f( x1,x2,...,xm | H1 )
# alpha: type I Error
# beta : type II Error
# R[m] = f( x1,x2,...,xm | H1 )/f( x1,x2,...,xm | H0 )
 
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
  
x<-c(1,0,0,1,1,1)
sprt(dbinom(x,1,0.3),dbinom(x,1,0.6),0.2,0.2)


rm(list=ls())


 
 